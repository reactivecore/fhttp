package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpHeader, HttpRequest, Multipart }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import net.reactivecore.fhttp.{ Input, TypedInput }
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import net.reactivecore.fhttp.helper.{ HListConcatAndSplit, VTree }
import shapeless._

trait RequestEncoder[Step] {

  type Input <: VTree

  /** Build a Request Building method for this Part. */
  def build(step: Step): RequestEncoder.Fn[Input]
}

object RequestEncoder {

  type Fn[Input] = (HttpRequest, Input) => HttpRequest

  type Aux[Step, InputT] = RequestEncoder[Step] {
    type Input = InputT
  }

  /** Applies c before putting input to fn.  */
  def contraMapFn[Input, Output](fn: Fn[Output], c: Input => Output): Fn[Input] = {
    (req, input) => fn(req, c(input))
  }

  def apply[T](implicit rb: RequestEncoder[T]): Aux[T, rb.Input] = rb

  /** Generate a new Request Encoder. */
  def make[Step, Consuming <: VTree](f: Step => Fn[Consuming]): Aux[Step, Consuming] = new RequestEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): (HttpRequest, Consuming) => HttpRequest = {
      f(step)
    }
  }

  /** Generate a simplified request encoder.  */
  private def makeSimple[Step, Consuming <: VTree](f: (HttpRequest, Step, Consuming) => HttpRequest) =
    make[Step, Consuming] { step => (request, value) => f(request, step, value)
    }

  implicit val encodeExtraPath = makeSimple[Input.ExtraPath.type, VTree.Leaf[String]] {
    case (request, _, value) =>
      val path = request.uri.path / value.x
      request.withUri(request.uri.copy(path = path))
  }

  implicit val encodeExtraPathFixed = makeSimple[Input.ExtraPathFixed, VTree.Empty] {
    case (request, step, _) =>
      val path = step.pathElements.foldLeft(request.uri.path)(_ / _)
      request.withUri(request.uri.copy(path = path))
  }

  implicit val addQueryParameter = makeSimple[Input.AddQueryParameter, VTree.Leaf[String]] {
    case (request, step, value) =>
      val extended: Query = Query(request.uri.query() :+ (step.name -> value.x): _*)
      val uri = request.uri.withQuery(extended)
      request.withUri(uri)
  }

  implicit def encodeMapped[T] = make[Input.MappedPayload[T], VTree.Leaf[T]] { step =>
    val contentType = AkkaHttpHelper.forceContentType(step.mapping.contentType)
    (request, value) => {
      val encoded = step.mapping.encode(value.x).getOrElse {
        throw new IllegalArgumentException("Could not encode value")
      }
      request.withEntity(contentType, ByteString.fromByteBuffer(encoded))
    }
  }

  implicit val encodeBinary = make[Input.Binary.type, VTree.LeafBranch[String, Source[ByteString, _]]] { _ => (request, value) => {
    val contentType = AkkaHttpHelper.binaryContentTypeForName(value.l.x)
    request.withEntity(HttpEntity.apply(contentType, value.r.x))
  }
  }

  implicit def encodeQueryParameterMap[T] = make[Input.QueryParameterMap[T], VTree.Leaf[T]] { step => (request, value) => {
    val encoded = step.mapping.encode(value.x).getOrElse {
      throw new IllegalArgumentException("Could not encode value")
    }
    val extended: Query = Query(request.uri.query() ++ encoded: _*)
    val uri = request.uri.withQuery(extended)
    request.withUri(uri)
  }
  }

  implicit val encodeAddHeader = make[Input.AddHeader, VTree.Leaf[String]] { step => (request, value) =>
    val encoded = HttpHeader.parse(step.name, value.x) match {
      case v: ParsingResult.Ok => v.header
      case e: ParsingResult.Error =>
        throw new IllegalArgumentException(s"Invalid header ${e.errors}")
    }
    request.addHeader(encoded)
  }

  implicit def encodeMultipart[Parts <: HList, PartArgumentsV <: VTree](
    implicit
    aux: MultipartEncoder.Aux[Parts, PartArgumentsV]
  ) = make[Input.Multipart[Parts], PartArgumentsV] { step =>
    val prepared = aux.build(step.parts)
    (request, values) => {
      val parts = prepared(Nil, values)
      val entity = Multipart.FormData(parts: _*).toEntity()
      request.withEntity(entity)
    }
  }

  implicit def encodeMappedInput[A, B, T <: TypedInput[A], V](
    implicit
    aux: RequestEncoder.Aux[T, VTree.Leaf[A]]
  ) = make[Input.MappedInput[A, B, T], VTree.Leaf[B]] { step =>
    val prepared = aux.build(step.original)
    (request, value) => {
      val mapped = step.mapping.encode(value.x).getOrElse {
        throw new IllegalArgumentException(s"Could not encode value")
      }
      prepared(request, VTree.Leaf(mapped))
    }
  }

  implicit val encodeNil = makeSimple[HNil, VTree.Empty] {
    case (request, _, __) =>
      request
  }

  implicit def encodeElem[H, T <: HList, HC <: VTree, TC <: VTree](
    implicit
    h: RequestEncoder.Aux[H, HC],
    aux: RequestEncoder.Aux[T, TC]
  ) = make[H :: T, VTree.Branch[HC, TC]] { step =>
    val headPrepared = h.build(step.head)
    val tailPrepared = aux.build(step.tail)

    (request, value) => {
      val requestAfterHead = headPrepared(request, value.l)
      val requestAfterTail = tailPrepared(requestAfterHead, value.r)
      requestAfterTail
    }
  }
}
