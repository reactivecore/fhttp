package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.common.StrictForm
import akka.http.scaladsl.server.PathMatcher.{Matched, Unmatched}
import akka.http.scaladsl.server.{PathMatchers, RequestContext}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Decoder
import net.reactivecore.fhttp.{Input, TypedInput}
import net.reactivecore.fhttp.akka.{AkkaHttpHelper, codecs}
import net.reactivecore.fhttp.helper.{ConcatenateAndSplitHList, SimpleArgumentLister, VTree}
import shapeless._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Counterpart to RequestBuilder, decodes a Request */
trait RequestDecoder[Step] {
  type Output <: VTree

  def build(s: Step): RequestDecoder.Fn[Output]
}

object RequestDecoder {

  sealed trait DecodingError {
    def msg: String
  }
  object DecodingError {
    case class MissingExpectedValue(msg: String) extends DecodingError
    case class InvalidPath(msg: String) extends DecodingError
    case class InvalidPayload(msg: String) extends DecodingError
    case class InvalidQuery(msg: String) extends DecodingError
  }

  type Fn[Output] = RequestContext => Future[Either[DecodingError, (RequestContext, Output)]]

  def mapFn[From, To](f: Fn[From], m: From => To): Fn[To] = {
    context => {
      import context._
      f(context).map {
        _.right.map { case (context, value) =>
          context -> m(value)
        }
      }
    }
  }

  type Aux[Step, OutputT] = RequestDecoder[Step] {
    type Output = OutputT
  }

  def apply[T](implicit rd: RequestDecoder[T]): Aux[T, rd.Output] = rd

  /** Generate a new Request Decoder. */
  def make[Step, Producing <: VTree](f: Step => Fn[Producing]): Aux[Step, Producing] = new RequestDecoder[Step] {
    override type Output = Producing

    override def build(s: Step): Fn[Output] = {
      f(s)
    }
  }

  implicit val stringPathDecoder = make[Input.ExtraPath.type, VTree.Leaf[String]] { _ => request =>
    val matcher = PathMatchers.Neutral / PathMatchers.Segment
    matcher(request.unmatchedPath) match {
      case Matched(rest, ok) =>
        val updated = request.withUnmatchedPath(rest)
        Future.successful(
          Right(updated -> (VTree.Leaf(ok._1)))
        )
      case Unmatched =>
        RequestDecoder.failedDecoding(DecodingError.InvalidPath("Expected sub path"))
    }
  }

  implicit val fixedPathDecoder = make[Input.ExtraPathFixed, VTree.Empty] { step =>
    {
      val matcher = PathMatchers.Neutral / PathMatchers.Segments(step.pathElements.size)
      request =>
        matcher.apply(request.unmatchedPath) match {
          case Matched(rest, matches) if matches._1 == step.pathElements =>
            val updatedRequest = request.withUnmatchedPath(rest)
            Future.successful(Right(updatedRequest -> VTree.Empty))
          case Matched(_, _) =>
            RequestDecoder.failedDecoding(DecodingError.InvalidPath("Expected a different path"))
          case _ =>
            RequestDecoder.failedDecoding(DecodingError.InvalidPath(s"Expected path ${step.pathElements}"))
        }
    }
  }

  implicit def mapped[T] = make[Input.MappedPayload[T], VTree.Leaf[T]] { step =>
    implicit val unmarshaller = AkkaHttpHelper.unmarshallerFromMapping[T](step.mapping)
    requestContext => {
      import requestContext._
      Unmarshal(request.entity).to[T].map { decoded =>
        Right(requestContext -> (VTree.Leaf(decoded)))
      }
    }
  }

  implicit val binaryStreamDecoder = make[Input.Binary.type, VTree.LeafBranch[String, Source[ByteString, _]]] { step => requestContext => {
    val entity = requestContext.request.entity
    val contentType = entity.contentType.value
    val dataSource = entity.dataBytes
    Future.successful(
      Right(requestContext -> VTree.Branch.fromLeafs(contentType, dataSource))
    )
  }
  }

  implicit val queryParameterDecoder = make[Input.AddQueryParameter, VTree.Leaf[String]] { step => requestContext => {
    requestContext.request.uri.query().get(step.name) match {
      case None =>
        failedDecoding(DecodingError.MissingExpectedValue(s"Missing Query parameter ${step.name}"))
      case Some(ok) =>
        Future.successful(Right(requestContext -> (VTree.Leaf(ok))))
    }
  }
  }

  implicit def queryParameterMapDecoder[T] = make[Input.QueryParameterMap[T], VTree.Leaf[T]] { step => requestContext => {
    val queryParameters = requestContext.request.uri.query().toMap
    step.mapping.decode(queryParameters) match {
      case Left(error) =>
        RequestDecoder.failedDecoding(DecodingError.InvalidQuery(s"Could not parse query map (${error})"))
      case Right(ok) =>
        Future.successful(Right(requestContext -> (VTree.Leaf(ok))))
    }
  }
  }

  implicit val headerValueDecoder = make[Input.AddHeader, VTree.Leaf[String]] { step => requestContext => {
    val value = requestContext.request.headers.collectFirst {
      case h if h.name() == step.name => h.value()
    }
    value match {
      case Some(existing) =>
        Future.successful(Right(requestContext -> (VTree.Leaf(existing))))
      case _ =>
        RequestDecoder.failedDecoding(DecodingError.MissingExpectedValue(s"Missing expected header ${step.name}"))
    }

  }
  }

  import scala.concurrent.duration._
  private val MultipartBufferingTimeout = 60.seconds // TODO: Make this changeable.

  implicit def multipartDecoder[T <: HList, Producing <: VTree](
    implicit
    multipartDecoder: MultipartDecoder.Aux[T, Producing]
  ) = make[Input.Multipart[T], Producing] { step =>
    val built = multipartDecoder.build(step.parts)
    requestContext => {
      import requestContext._
      for {
        fields <- Unmarshal(requestContext.request.entity).to[akka.http.scaladsl.model.Multipart.FormData]
        // We are buffering the request on server side
        // Otherwise it's tricky to decode the single form fields differently.
        strictForm <- fields.toStrict(MultipartBufferingTimeout)
        values <- built(requestContext, strictForm)
      } yield {
        values match {
          case Left(bad) => Left(bad)
          case Right(ok) => Right(requestContext -> ok)
        }
      }
    }
  }

  implicit def decodeMapped[A, B, T <: TypedInput[A], V](
    implicit
    aux: RequestDecoder.Aux[T, VTree.Leaf[A]]
  ) = make[Input.MappedInput[A, B, T], VTree.Leaf[B]] { step =>
    val built = aux.build(step.original)
    requestContext => {
      import requestContext._
      built(requestContext).map {
        _.right.map {
          case (context, value) =>
            val encodedValue = step.mapping.decode(value.x).getOrElse {
              throw new IllegalArgumentException("Could not encode value")
            }
            context -> (VTree.Leaf(encodedValue))
        }
      }
    }
  }

  implicit val nilDecoder = make[HNil, VTree.Empty] { _ => requestContext => {
    Future.successful(Right(requestContext -> VTree.Empty))
  }
  }

  implicit def elemDecoder[H, T <: HList, HP <: VTree, TP <: VTree](
    implicit
    h: RequestDecoder.Aux[H, HP],
    aux: RequestDecoder.Aux[T, TP]
  ) = make[H :: T, VTree.Branch[HP, TP]] { step =>
    val tailPrepared = aux.build(step.tail)
    val headPrepared = h.build(step.head)
    requestContext => {
      import requestContext._

      tailPrepared(requestContext).flatMap {
        case Left(bad) => Future.successful(Left(bad))
        case Right((afterTailRequest, afterTailResult)) =>
          headPrepared(afterTailRequest).map {
            _.right.map {
              case (afterHeadRequest, afterHeadResult) =>
                val finalValue = VTree.Branch(afterHeadResult, afterTailResult)
                afterHeadRequest -> finalValue
            }
          }
      }
    }
  }

  /** Convert the result of an unmarshalling operation into a future with extracted decoding error. */
  private[codecs] def decodeResultToResult[T](in: Future[T])(implicit ec: ExecutionContext): Future[Either[DecodingError, VTree.Leaf[T]]] = {
    in.transform {
      case Failure(error) => Success(Left(DecodingError.InvalidPayload(error.getMessage)))
      case Success(value) => Success(Right(VTree.Leaf(value)))
    }
  }

  private[codecs] def failedDecoding(e: DecodingError): Future[Either[DecodingError, Nothing]] = {
    Future.successful(Left(e))
  }

}

