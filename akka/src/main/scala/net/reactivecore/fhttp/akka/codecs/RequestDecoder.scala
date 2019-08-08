package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.server.RequestContext
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Decoder
import net.reactivecore.fhttp.Input
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import shapeless._

import scala.concurrent.Future

/** Counterpart to RequestBuilder, decodes a Request */
trait RequestDecoder[Step] {
  type Output

  // TODO: Error Error result type
  def build(s: Step): RequestDecoder.Fn[Output]

  def map[X](x: Output => X) = new RequestDecoder[Step] {
    type Output = X

    override def build(s: Step): RequestDecoder.Fn[X] = {
      val built = RequestDecoder.this.build(s)
      context => {
        import context._
        built(context).map {
          case (context2, out) =>
            context2 -> x(out)
        }
      }
    }
  }
}

object RequestDecoder {

  type Fn[Output] = RequestContext => Future[(RequestContext, Output)]

  type Aux[Step, OutputT] = RequestDecoder[Step] {
    type Output = OutputT
  }

  def apply[T](implicit rd: RequestDecoder[T]): Aux[T, rd.Output] = rd

  private def make[Step, Producing](f: Step => RequestContext => Future[(RequestContext, Producing)]): Aux[Step, Producing] = new RequestDecoder[Step] {
    override type Output = Producing

    override def build(s: Step): RequestContext => Future[(RequestContext, Producing)] = {
      f(s)
    }
  }

  implicit val stringPathDecoder = make[Input.ExtraPath.type, String] { _ => request =>
    val head = request.unmatchedPath
    val withoutSlash = if (head.startsWithSlash) {
      head.dropChars(1)
    } else {
      head
    }
    val updatedRequest = request.mapUnmatchedPath { _.dropChars(head.charCount) }
    Future.successful(
      updatedRequest -> withoutSlash.toString()
    )
  }

  implicit def mapped[T] = make[Input.Mapped[T], T] { step =>
    implicit val unmarshaller = AkkaHttpHelper.unmarshallerFromMapping[T](step.mapping)
    requestContext => {
      import requestContext._
      Unmarshal(request.entity).to[T].map { decoded =>
        requestContext -> decoded
      }
    }
  }

  implicit val binaryStreamDecoder = make[Input.Binary.type, (String, Source[ByteString, _])] { step => requestContext => {
    val entity = requestContext.request.entity
    val contentType = entity.contentType.value
    val dataSource = entity.dataBytes
    Future.successful(
      requestContext -> (contentType, dataSource)
    )
  }
  }

  implicit val queryParameterDecoder = make[Input.AddQueryParameter, String] { step => requestContext => {
    val v = requestContext.request.uri.query().get(step.name).getOrElse {
      throw new IllegalArgumentException("Missing query parameter")
    }
    Future.successful(requestContext -> v)
  }
  }

  implicit val nilDecoder = make[HNil, HNil] { _ => requestContext => {
    Future.successful(requestContext -> HNil)
  }
  }

  implicit def elemDecoder[H, T <: HList, X <: HList](
    implicit
    h: RequestDecoder[H],
    aux: RequestDecoder.Aux[T, X]
  ) = make[H :: T, h.Output :: X] { step =>
    // head first!
    val headPrepared = h.build(step.head)
    val tailPrepared = aux.build(step.tail)
    requestContext => {
      import requestContext._
      for {
        (afterHeadRequest, afterHeadResult) <- headPrepared(requestContext)
        (afterTailRequest, afterTailResult) <- tailPrepared(afterHeadRequest)
      } yield {
        afterTailRequest -> (afterHeadResult :: afterTailResult)
      }
    }
  }

}

