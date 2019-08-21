package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpResponse }
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Encoder
import io.circe.syntax._
import net.reactivecore.fhttp.Output
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import net.reactivecore.fhttp.helper.VTree
import shapeless._

trait ResponseEncoder[Step] {
  type Input <: VTree

  def build(step: Step): ResponseEncoder.Fn[Input]
}

object ResponseEncoder {
  type Aux[Step, InputT] = ResponseEncoder[Step] {
    type Input = InputT
  }

  type Fn[Input] = (HttpResponse, Input) => HttpResponse

  /** Applies a function c for input values before applying fn. */
  def contraMapFn[From, To](fn: Fn[From], c: To => From): Fn[To] = { (response, input) =>
    fn(response, c(input))
  }

  def apply[T](implicit rb: ResponseEncoder[T]): Aux[T, rb.Input] = rb

  def make[Step, Consuming <: VTree](f: Step => (HttpResponse, Consuming) => HttpResponse): Aux[Step, Consuming] = new ResponseEncoder[Step] {
    override type Input = Consuming

    override def build(step: Step): (HttpResponse, Consuming) => HttpResponse = {
      f(step)
    }
  }

  def makeLeaf[Step, Consuming](f: Step => Fn[Consuming]): Aux[Step, VTree.Leaf[Consuming]] = new ResponseEncoder[Step] {
    override type Input = VTree.Leaf[Consuming]

    override def build(step: Step): Fn[VTree.Leaf[Consuming]] = {
      val built = f(step)
      contraMapFn(built, _.x)
    }
  }

  implicit def encodeMapped[T] = makeLeaf[Output.MappedPayload[T], T] { step =>
    val contentType = AkkaHttpHelper.forceContentType(step.mapping.contentType)
    (response, value) => {
      val encoded = step.mapping.encode(value).getOrElse {
        throw new IllegalArgumentException("Could not encode value")
      }
      response.withEntity(contentType, ByteString.fromByteBuffer(encoded))
    }
  }

  implicit val encodeBinaryResponse = make[Output.Binary.type, VTree.Branch[VTree.Leaf[String], VTree.Leaf[Source[ByteString, _]]]] { step => (response, value) => {
    val contentType = AkkaHttpHelper.forceContentType(value.l.x)
    response.withEntity(HttpEntity.apply(contentType, value.r.x))
  }
  }

  implicit def requestErrorResponse[F <: Output, S <: Output, FT <: VTree, ST <: VTree](
    implicit
    f: ResponseEncoder.Aux[F, FT],
    s: ResponseEncoder.Aux[S, ST]
  ) = make[Output.ErrorSuccess[F, S], VTree.ContraBranch[VTree.Branch[VTree.Leaf[Int], FT], ST]] { step =>
    val preparedFailure = f.build(step.f)
    val preparedSuccess = s.build(step.s)
    (response, value) => {
      value.v match {
        case Left(bad) =>
          val statusCode = bad.l.x
          val value = bad.r
          if (statusCode >= 200 && statusCode < 300) {
            println(s"Error with status code ${statusCode} ??")
          }
          preparedFailure(response.withStatus(statusCode), value)
        case Right(ok) =>
          preparedSuccess(response, ok)
      }
    }
  }

  implicit val encodeEmpty = make[Output.Empty.type, VTree.Empty] { _ => (response, _) => response }

  implicit val encodeNil = make[HNil, VTree.Empty] { _ => (response, _) => response
  }

  implicit def encodeElem[H, HT <: VTree, T <: HList, TT <: VTree](
    implicit
    h: ResponseEncoder.Aux[H, HT],
    aux: ResponseEncoder.Aux[T, TT]
  ) = make[H :: T, VTree.Branch[HT, TT]] { step =>
    val preparedTail = aux.build(step.tail)
    val preparedHead = h.build(step.head)
    (response, input) => {
      val afterTail = preparedTail(response, input.r)
      val afterHead = preparedHead(afterTail, input.l)
      afterHead
    }
  }
}