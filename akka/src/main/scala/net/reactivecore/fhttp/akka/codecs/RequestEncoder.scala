package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpHeader, HttpRequest, Multipart }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import net.reactivecore.fhttp.{ Input, TypedInput }
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import net.reactivecore.fhttp.helper.{ ConcatenateAndSplitHList, SimpleArgumentLister }
import shapeless._

trait RequestEncoder[Step] {

  type Input <: HList

  /** Build a Request Building method for this Part. */
  def build(step: Step): RequestEncoder.Fn[Input]
}

object RequestEncoder {

  type Fn[Input] = (HttpRequest, Input) => HttpRequest

  type Aux[Step, InputT] = RequestEncoder[Step] {
    type Input = InputT
  }

  def apply[T](implicit rb: RequestEncoder[T]): Aux[T, rb.Input] = rb

  /** Generate a new Request Encoder. */
  def make[Step, Consuming <: HList](f: Step => (HttpRequest, Consuming) => HttpRequest): Aux[Step, Consuming] = new RequestEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): (HttpRequest, Consuming) => HttpRequest = {
      f(step)
    }
  }

  /** Generate a simplified request encoder.  */
  private def makeSimple[Step, Consuming <: HList](f: (HttpRequest, Step, Consuming) => HttpRequest) = make[Step, Consuming] { step => (request, value) => f(request, step, value)
  }

  implicit val encodeExtraPath = makeSimple[Input.ExtraPath.type, String :: HNil] {
    case (request, _, value) =>
      val path = request.uri.path / value.head
      request.withUri(request.uri.copy(path = path))
  }

  implicit val encodeExtraPathFixed = makeSimple[Input.ExtraPathFixed, HNil] {
    case (request, step, _) =>
      val path = step.pathElements.foldLeft(request.uri.path)(_ / _)
      request.withUri(request.uri.copy(path = path))
  }

  implicit val addQueryParameter = makeSimple[Input.AddQueryParameter, String :: HNil] {
    case (request, step, value) =>
      val extended: Query = Query(request.uri.query() :+ (step.name -> value.head): _*)
      val uri = request.uri.withQuery(extended)
      request.copy(uri = uri)
  }

  implicit def encodeMapped[T] = make[Input.MappedPayload[T], T :: HNil] { step =>
    val contentType = AkkaHttpHelper.forceContentType(step.mapping.contentType)
    (request, value) => {
      val encoded = step.mapping.encode(value.head).getOrElse {
        throw new IllegalArgumentException("Could not encode value")
      }
      request.withEntity(contentType, ByteString.fromByteBuffer(encoded))
    }
  }

  implicit val encodeBinary = make[Input.Binary.type, Source[ByteString, _] :: String :: HNil] { _ => (request, value) => {
    val contentType = AkkaHttpHelper.binaryContentTypeForName(value.tail.head)
    request.withEntity(HttpEntity.apply(contentType, value.head))
  }
  }

  implicit def encodeQueryParameterMap[T] = make[Input.QueryParameterMap[T], T :: HNil] { step => (request, value) => {
    val encoded = step.mapping.encode(value.head).getOrElse {
      throw new IllegalArgumentException("Could not encode value")
    }
    val extended: Query = Query(request.uri.query() ++ encoded: _*)
    val uri = request.uri.withQuery(extended)
    request.copy(uri = uri)
  }
  }

  implicit val encodeAddHeader = make[Input.AddHeader, String :: HNil] { step => (request, value) =>
    val encoded = HttpHeader.parse(step.name, value.head) match {
      case v: ParsingResult.Ok => v.header
      case e: ParsingResult.Error =>
        throw new IllegalArgumentException(s"Invalid header ${e.errors}")
    }
    request.addHeader(encoded)
  }

  implicit def encodeMultipart[Parts <: HList, PartArgumentsH <: HList](
    implicit
    aux: MultipartEncoder.Aux[Parts, PartArgumentsH]
  ) = make[Input.Multipart[Parts], PartArgumentsH] { step =>
    val prepared = aux.build(step.parts)
    (request, values) => {
      val parts = prepared(Nil, values)
      val entity = Multipart.FormData(parts: _*).toEntity()
      request.withEntity(entity)
    }
  }

  implicit def encodeMappedInput[A, B, T <: TypedInput[A], V](
    implicit
    aux: RequestEncoder.Aux[T, A :: HNil]
  ) = make[Input.MappedInput[A, B, T], B :: HNil] { step =>
    val prepared = aux.build(step.original)
    (request, value) => {
      val mapped = step.mapping.encode(value.head).getOrElse {
        throw new IllegalArgumentException(s"Could not encode value")
      }
      prepared(request, mapped :: HNil)
    }
  }

  implicit val encodeNil = makeSimple[HNil, HNil] {
    case (request, _, __) =>
      request
  }

  implicit def encodeElem[H, T <: HList, HC <: HList, TC <: HList, Combined <: HList](
    implicit
    h: RequestEncoder.Aux[H, HC],
    aux: RequestEncoder.Aux[T, TC],
    concatenateAndSplit: ConcatenateAndSplitHList.Aux[HC, TC, Combined]
  ) = make[H :: T, Combined] { step =>
    val tailPrepared = aux.build(step.tail)
    val headPrepared = h.build(step.head)

    (request, value) => {
      val (head, tail) = concatenateAndSplit.split(value)
      val requestAfterTail = tailPrepared(request, tail)
      val result = headPrepared(requestAfterTail, head)
      result
    }
  }
}
