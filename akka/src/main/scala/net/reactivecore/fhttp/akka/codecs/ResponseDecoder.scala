package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Decoder
import net.reactivecore.fhttp.Output
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }

class DecodingContext(
    implicit
    val ec: ExecutionContext,
    implicit val materializer: Materializer
)

trait ResponseDecoder[OutputStep] {
  type Output <: Any

  def build(step: OutputStep): ResponseDecoder.Fn[Output]

  def map[X](f: Output => X) = new ResponseDecoder[OutputStep] {
    override type Output = X

    override def build(step: OutputStep): ResponseDecoder.Fn[X] = {
      val built = ResponseDecoder.this.build(step)
      (response, context) => {
        import context._
        built(response, context).map { result =>
          f(result)
        }
      }
    }
  }
}

object ResponseDecoder {
  type Aux[Step, OutputT] = ResponseDecoder[Step] {
    type Output = OutputT
  }

  type Fn[Output] = (HttpResponse, DecodingContext) => Future[Output]

  def apply[Step](implicit rd: ResponseDecoder[Step]) = rd

  def make[Step, Producing](f: Step => (HttpResponse, DecodingContext) => Future[Producing]) = new ResponseDecoder[Step] {
    override type Output = Producing

    override def build(step: Step): (HttpResponse, DecodingContext) => Future[Producing] = {
      f(step)
    }
  }

  implicit def decodeMapped[T] = make[Output.Mapped[T], T] { step =>
    implicit val unmapper = AkkaHttpHelper.unmarshallerFromMapping(step.mapping)
    (response, context) => {
      import context._
      val entity = step.limit match {
        case Some(limit) =>
          response.entity.withSizeLimit(limit)
        case None =>
          response.entity
      }
      Unmarshal(entity).to[T].map { decoded =>
        decoded
      }
    }
  }

  implicit val decodeBinary = make[Output.Binary.type, (String, Source[ByteString, _])] { step => (response, _) => {
    val contentType = response.entity.contentType.value
    val source = response.entity.dataBytes
    Future.successful(
      contentType -> source
    )
  }
  }

  implicit def decodeFailureSuccess[F <: Output, S <: Output, FT, ST](
    implicit
    f: ResponseDecoder.Aux[F, FT],
    s: ResponseDecoder.Aux[S, ST]
  ) = make[Output.ErrorSuccess[F, S], Either[(Int, FT), ST]] { step =>
    val preparedFailure = f.build(step.f)
    val preparedSuccess = s.build(step.s)
    (response, context) => {
      import context._
      if (response.status.isSuccess()) {
        preparedSuccess(response, context).map(Right(_))
      } else {
        preparedFailure(response, context).map { parsed =>
          Left(response.status.intValue() -> parsed)
        }
      }
    }
  }

  implicit val decodeNil = make[HNil, HNil] { _ => (_, _) => {
    Future.successful(HNil)
  }
  }

  implicit def decodeElem[H, T <: HList, X <: HList](
    implicit
    h: ResponseDecoder[H],
    aux: ResponseDecoder.Aux[T, X]
  ) = make[H :: T, h.Output :: X] { step =>
    val decodeTail = aux.build(step.tail)
    val decodeHead = h.build(step.head)
    (response, context) => {
      val decodeTailFuture = decodeTail(response, context)
      val decodeHeadFuture = decodeHead(response, context)
      import context._
      for {
        decodedTail <- decodeTailFuture
        decodedHead <- decodeHeadFuture
      } yield {
        decodedHead :: decodedTail
      }
    }
  }
}
