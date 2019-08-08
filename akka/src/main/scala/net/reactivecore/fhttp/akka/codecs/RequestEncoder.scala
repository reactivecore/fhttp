package net.reactivecore.fhttp.akka.codecs

import akka.http.javadsl.model.RequestEntity
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpRequest }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Encoder
import RequestEncoder.Fn
import net.reactivecore.fhttp.Input
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import shapeless._

trait RequestEncoder[Step] {

  type Input <: Any

  /** Build a Request Building method for this Part. */
  def build(step: Step): RequestEncoder.Fn[Input]

  def contraMap[X](f: X => Input) = new RequestEncoder[Step] {
    override type Input = X

    override def build(step: Step): Fn[Input] = {
      val built = RequestEncoder.this.build(step)
      (request, input) => {
        built(request, f(input))
      }
    }
  }
}

object RequestEncoder {

  type Fn[Input] = (HttpRequest, Input) => HttpRequest

  type Aux[Step, InputT] = RequestEncoder[Step] {
    type Input = InputT
  }

  def apply[T](implicit rb: RequestEncoder[T]): RequestEncoder[T] = rb

  private def make[Step, Consuming](f: Step => (HttpRequest, Consuming) => HttpRequest) = new RequestEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): (HttpRequest, Consuming) => HttpRequest = {
      f(step)
    }
  }

  // Generate a simple RequestBuilder which doesn't need much preparation
  private def makeSimple[Step, Consuming](f: (HttpRequest, Step, Consuming) => HttpRequest) = make[Step, Consuming] { step => (request, value) => f(request, step, value)
  }

  implicit val encodeExtraPath = makeSimple[Input.ExtraPath.type, String] {
    case (request, _, value) =>
      val path = request.uri.path / value
      request.withUri(request.uri.copy(path = path))
  }

  implicit val addQueryParameter = makeSimple[Input.AddQueryParameter, String] {
    case (request, step, value) =>
      val extended: Query = Query(request.uri.query() :+ (step.name -> value): _*)
      val uri = request.uri.withQuery(extended)
      request.copy(uri = uri)
  }

  implicit def encodeMapped[T] = make[Input.Mapped[T], T] { step =>
    val contentType = AkkaHttpHelper.forceContentType(step.mapping.contentType)
    (request, value) => {
      val encoded = step.mapping.encode(value)
      request.withEntity(contentType, ByteString.fromByteBuffer(encoded))
    }
  }

  implicit val encodeBinary = make[Input.Binary.type, (String, Source[ByteString, _])] { _ => (request, value) => {
    val contentType = AkkaHttpHelper.binaryContentTypeForName(value._1)
    request.withEntity(HttpEntity.apply(contentType, value._2))
  }
  }

  implicit val encodeNil = makeSimple[HNil, HNil] {
    case (request, _, __) =>
      request
  }

  implicit def encodeElem[H, T <: HList, X <: HList](
    implicit
    h: RequestEncoder[H],
    aux: RequestEncoder.Aux[T, X]
  ) = make[H :: T, h.Input :: X] { step =>
    val tailPrepared = aux.build(step.tail)
    val headPrepared = h.build(step.head)
    (request, value) => {
      val requestAfterTail = tailPrepared(request, value.tail)
      val result = headPrepared(requestAfterTail, value.head)
      result
    }
  }
}
