package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Decoder
import net.reactivecore.fhttp.Output
import net.reactivecore.fhttp.akka.AkkaHttpHelper
import net.reactivecore.fhttp.helper.VTree
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }
import akka.http.scaladsl.util.FastFuture._

class DecodingContext(
    implicit
    val ec: ExecutionContext,
    implicit val materializer: Materializer
)

trait ResponseDecoder[OutputStep] {
  type Output <: VTree

  def build(step: OutputStep): ResponseDecoder.Fn[Output]
}

object ResponseDecoder {
  type Aux[Step, OutputT] = ResponseDecoder[Step] {
    type Output = OutputT
  }

  type Fn[Output] = (HttpResponse, DecodingContext) => Future[Output]

  /** Map the function m onto the result of f. */
  def mapFn[From, To](f: Fn[From], m: From => To): Fn[To] = {
    (response, context) =>
      {
        import context._
        f(response, context).map { result =>
          m(result)
        }
      }
  }

  def apply[Step](implicit rd: ResponseDecoder[Step]): Aux[Step, rd.Output] = rd

  def make[Step, Producing <: VTree](f: Step => (HttpResponse, DecodingContext) => Future[Producing]): Aux[Step, Producing] = new ResponseDecoder[Step] {
    override type Output = Producing

    override def build(step: Step): (HttpResponse, DecodingContext) => Future[Producing] = {
      f(step)
    }
  }

  def makeLeaf[Step, Producing](f: Step => Fn[Producing]): Aux[Step, VTree.Leaf[Producing]] = new ResponseDecoder[Step] {
    override type Output = VTree.Leaf[Producing]

    override def build(step: Step): Fn[VTree.Leaf[Producing]] = {
      val built = f(step)
      mapFn(built, VTree.Leaf.apply)
    }
  }

  implicit def decodeMapped[T] = makeLeaf[Output.MappedPayload[T], T] { step =>
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

  implicit val decodeBinary = make[Output.Binary.type, VTree.Branch[VTree.Leaf[String], VTree.Leaf[Source[ByteString, _]]]] { step => (response, _) => {
    val contentType = response.entity.contentType.value
    val source = response.entity.dataBytes
    FastFuture.successful(
      VTree.Branch.fromLeafs(contentType, source)
    )
  }
  }

  implicit def decodeFailureSuccess[F <: Output, S <: Output, FT <: VTree, ST <: VTree](
    implicit
    f: ResponseDecoder.Aux[F, FT],
    s: ResponseDecoder.Aux[S, ST]
  ) = make[Output.ErrorSuccess[F, S], VTree.ContraBranch[VTree.Branch[VTree.Leaf[Int], FT], ST]] { step =>
    val preparedFailure = f.build(step.f)
    val preparedSuccess = s.build(step.s)
    (response, context) => {
      import context._
      if (response.status.isSuccess()) {
        preparedSuccess(response, context).map { successValue =>
          VTree.ContraBranch(
            Right(successValue)
          )
        }
      } else {
        preparedFailure(response, context).map { parsed =>
          val statusCode = response.status.intValue()
          VTree.ContraBranch(
            Left(
              VTree.Branch(VTree.Leaf(statusCode), parsed)
            )
          )
        }
      }
    }
  }

  implicit val decodeEmpty = make[Output.Empty.type, VTree.Empty] { _ => (_, _) =>
    FastFuture.successful(VTree.Empty)
  }

  implicit val decodeNil = make[HNil, VTree.Empty] { _ => (_, _) => {
    FastFuture.successful(VTree.Empty)
  }
  }

  implicit def decodeElem[H, T <: HList, HT <: VTree, TT <: VTree](
    implicit
    h: ResponseDecoder.Aux[H, HT],
    aux: ResponseDecoder.Aux[T, TT]
  ) = make[H :: T, VTree.Branch[HT, TT]] { step =>
    val decodeTail = aux.build(step.tail)
    val decodeHead = h.build(step.head)
    (response, context) => {
      val decodeTailFuture = decodeTail(response, context)
      val decodeHeadFuture = decodeHead(response, context)
      import context._
      for {
        decodedTail <- decodeTailFuture.fast
        decodedHead <- decodeHeadFuture.fast
      } yield {
        VTree.Branch(decodedHead, decodedTail)
      }
    }
  }
}
