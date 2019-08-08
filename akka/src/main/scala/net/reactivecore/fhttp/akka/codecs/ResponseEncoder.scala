package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpResponse }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Encoder
import io.circe.syntax._
import net.reactivecore.fhttp.Output
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import shapeless._

trait ResponseEncoder[Step] {
  type Input <: Any

  def build(step: Step): ResponseEncoder.Fn[Input]

  def contraMap[X](f: X => Input) = new ResponseEncoder[Step] {
    override type Input = X

    override def build(step: Step): ResponseEncoder.Fn[Input] = {
      val built = ResponseEncoder.this.build(step)
      (response, input) => {
        built(response, f(input))
      }
    }
  }
}

object ResponseEncoder {
  type Aux[Step, InputT] = ResponseEncoder[Step] {
    type Input = InputT
  }

  type Fn[Input] = (HttpResponse, Input) => HttpResponse

  def apply[T](implicit rb: ResponseEncoder[T]): ResponseEncoder[T] = rb

  private def make[Step, Consuming](f: Step => (HttpResponse, Consuming) => HttpResponse) = new ResponseEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): (HttpResponse, Consuming) => HttpResponse = {
      f(step)
    }
  }

  implicit def encodeMapped[T] = make[Output.Mapped[T], T] { step =>
    val contentType = AkkaHttpHelper.forceContentType(step.mapping.contentType)
    (response, value) => {
      val encoded = step.mapping.encode(value)
      response.withEntity(contentType, ByteString.fromByteBuffer(encoded))
    }
  }

  implicit val encodeBinaryResponse = make[Output.Binary.type, (String, Source[ByteString, _])] { step => (response, value) => {
    val contentType = AkkaHttpHelper.forceContentType(value._1)
    response.withEntity(HttpEntity.apply(contentType, value._2))
  }
  }

  implicit def requestErrorResponse[F <: Output, S <: Output, FT, ST](
    implicit
    f: ResponseEncoder.Aux[F, FT],
    s: ResponseEncoder.Aux[S, ST]
  ) = make[Output.ErrorSuccess[F, S], Either[(Int, FT), ST]] { step =>
    val preparedFailure = f.build(step.f)
    val preparedSuccess = s.build(step.s)
    (response, value) => {
      value match {
        case Left((statusCode, value)) =>
          if (statusCode >= 200 && statusCode < 300) {
            println(s"Error with status code ${statusCode} ??")
          }
          preparedFailure(response.withStatus(statusCode), value)
        case Right(ok) =>
          preparedSuccess(response, ok)
      }
    }
  }

  implicit val encodeNil = make[HNil, HNil] { _ => (response, _) => response
  }

  implicit def encodeElem[H, T <: HList, X <: HList](
    implicit
    h: ResponseEncoder[H],
    aux: ResponseEncoder.Aux[T, X]
  ) = make[H :: T, h.Input :: X] { step =>
    val preparedTail = aux.build(step.tail)
    val preparedHead = h.build(step.head)
    (response, input) => {
      val afterTail = preparedTail(response, input.tail)
      val afterHead = preparedHead(afterTail, input.head)
      afterHead
    }
  }
}