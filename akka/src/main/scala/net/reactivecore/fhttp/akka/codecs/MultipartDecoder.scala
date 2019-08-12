package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.common.StrictForm
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.server.RequestContext
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import net.reactivecore.fhttp.Input.Multipart
import net.reactivecore.fhttp.{Mapping, TextMapping}
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import shapeless._

import scala.concurrent.Future

trait MultipartDecoder[MultipartDefinition] {
  type Output

  def build(definition: MultipartDefinition): MultipartDecoder.Fn[Output]

}

object MultipartDecoder {
  type Aux[Definition, Output0] = MultipartDecoder[Definition] {
    type Output = Output0
  }

  type Fn[Output] = (RequestContext, FormData) => Future[Output]

  private def make[Step, Producing](f: Step =>  Fn[Producing]): Aux[Step, Producing] = new MultipartDecoder[Step] {
    override type Output = Producing

    override def build(s: Step): Fn[Output] = {
      f(s)
    }
  }

  implicit val textDecoder = make[Multipart.MultipartText, String] { step =>
    (req, formData) => {
      import req._
      formData.parts.filter(_.name == step.name).runWith(Sink.headOption).flatMap {
        case None => throw new IllegalArgumentException(s"Missing field ${step.name}")
        case Some(field) =>
          Unmarshal(field.entity).to[String]
      }
    }
  }

  implicit val binaryDecoder = make[Multipart.MultipartFile, (String, Source[ByteString, _])] { step =>
    (req, formData) => {
      import req._
      formData.parts.filter(_.name == step.name).runWith(Sink.headOption).map {
        case None => throw new IllegalArgumentException(s"Missing field ${step.name}")
        case Some(field) =>
          // Is this really streaming?
          val contentType = field.entity.contentType.value
          contentType -> field.entity.dataBytes
      }
    }
  }

  implicit def headDecoder[H, HD, T <: HList, TC <: HList](
    implicit h: MultipartDecoder.Aux[H, HD],
    a: MultipartDecoder.Aux[T, TC]
  ) = make[H :: T, HD :: TC] { step =>
    val tailPrepared = a.build(step.tail)
    val headPrepared = h.build(step.head)
    (req, strictForm) => {
      import req._
      for {
        tailValue <- tailPrepared.apply(req, strictForm)
        headValue <- headPrepared.apply(req, strictForm)
      } yield headValue :: tailValue
    }
  }

  implicit val nilDecoder = make[HNil, HNil] { _ =>
    (_, step) => Future.successful(HNil)
  }
}