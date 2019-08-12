package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.HttpEntity.IndefiniteLength
import akka.http.scaladsl.model.{ HttpRequest, Multipart }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import shapeless._
import net.reactivecore.fhttp.Input.{ Multipart => FHttpMultipart }
import net.reactivecore.fhttp.akka.AkkaHttpHelper

trait MultipartEncoder[MultipartDefinition] {

  type Input <: Any

  def build(step: MultipartDefinition): MultipartEncoder.Fn[Input]
}

object MultipartEncoder {

  type Fn[Input] = (List[Multipart.FormData.BodyPart], Input) => List[Multipart.FormData.BodyPart]

  type Aux[Step, InputT] = MultipartEncoder[Step] {
    type Input = InputT
  }

  def apply[T](implicit me: MultipartEncoder[T]): Aux[T, me.Input] = me

  private def make[Step, Consuming](f: Step => Fn[Consuming]): Aux[Step, Consuming] = new MultipartEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): Fn[Input] = {
      f(step)
    }
  }

  implicit val encodeText = make[FHttpMultipart.MultipartText, String] { part => (parts, value) => {
    Multipart.FormData.BodyPart(part.name, value) :: parts
  }
  }

  implicit val encodeFile = make[FHttpMultipart.MultipartFile, (String, Source[ByteString, _])] { part => (parts, value) => {
    val ct = AkkaHttpHelper.binaryContentTypeForName(value._1)
    val entity = IndefiniteLength(ct, value._2)
    val extraHeader = part.fileName.map { fileName =>
      Map("filename" -> fileName)
    }.getOrElse(Map.empty)
    Multipart.FormData.BodyPart(part.name, entity, _additionalDispositionParams = extraHeader) :: parts
  }
  }

  implicit val encodeNil: Aux[HNil, HNil] = make[HNil, HNil] { _ => (list, _) => list
  }

  implicit def encodeElem[H, HC, T <: HList, TC <: HList](
    implicit
    h: MultipartEncoder.Aux[H, HC],
    aux: MultipartEncoder.Aux[T, TC]
  ) = make[H :: T, HC :: TC] { step =>
    val tailPrepared = aux.build(step.tail)
    val headPrepared = h.build(step.head)
    (list, value) => {
      val afterTail = tailPrepared(list, value.tail)
      val afterHead = headPrepared(afterTail, value.head)
      afterHead
    }
  }
}