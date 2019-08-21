package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.HttpEntity.IndefiniteLength
import akka.http.scaladsl.model.{ HttpRequest, Multipart }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import shapeless._
import net.reactivecore.fhttp.Input.{ Multipart => FHttpMultipart }
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import net.reactivecore.fhttp.helper.VTree

trait MultipartEncoder[MultipartDefinition] {

  type Input <: VTree

  def build(step: MultipartDefinition): MultipartEncoder.Fn[Input]
}

object MultipartEncoder {

  type Fn[Input] = (List[Multipart.FormData.BodyPart], Input) => List[Multipart.FormData.BodyPart]

  type Aux[Step, InputT] = MultipartEncoder[Step] {
    type Input = InputT
  }

  def apply[T](implicit me: MultipartEncoder[T]): Aux[T, me.Input] = me

  private def make[Step, Consuming <: VTree](f: Step => Fn[Consuming]): Aux[Step, Consuming] = new MultipartEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): Fn[Input] = {
      f(step)
    }
  }

  implicit val encodeText = make[FHttpMultipart.MultipartText, VTree.Leaf[String]] { part => (parts, value) => {
    Multipart.FormData.BodyPart(part.name, value.x) :: parts
  }
  }

  implicit val encodeFile = make[FHttpMultipart.MultipartFile, VTree.LeafBranch[String, Source[ByteString, _]]] { part => (parts, value) => {
    val ct = AkkaHttpHelper.binaryContentTypeForName(value.l.x)
    val entity = IndefiniteLength(ct, value.r.x)
    val extraHeader = part.fileName.map { fileName =>
      Map("filename" -> fileName)
    }.getOrElse(Map.empty)
    Multipart.FormData.BodyPart(part.name, entity, _additionalDispositionParams = extraHeader) :: parts
  }
  }

  implicit val encodeNil: Aux[HNil, VTree.Empty] = make[HNil, VTree.Empty] { _ => (list, _) => list
  }

  implicit def encodeElem[H, HC <: VTree, T <: HList, TC <: VTree](
    implicit
    h: MultipartEncoder.Aux[H, HC],
    aux: MultipartEncoder.Aux[T, TC]
  ) = make[H :: T, VTree.Branch[HC, TC]] { step =>
    val headPrepared = h.build(step.head)
    val tailPrepared = aux.build(step.tail)
    (list, value) => {
      val afterHead = headPrepared(list, value.l)
      val afterTail = tailPrepared(afterHead, value.r)
      afterTail
    }
  }
}