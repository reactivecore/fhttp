package net.reactivecore.fhttp.akka.codecs

import akka.http.scaladsl.common.StrictForm
import akka.http.scaladsl.server.PathMatcher.{ Matched, Unmatched }
import akka.http.scaladsl.server.{ PathMatchers, RequestContext }
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.Decoder
import net.reactivecore.fhttp.{ Input, TypedInput }
import net.reactivecore.fhttp.akka.{ AkkaHttpHelper, codecs }
import net.reactivecore.fhttp.helper.SimpleArgumentLister
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

/** Counterpart to RequestBuilder, decodes a Request */
trait RequestDecoder[Step] {
  type Output

  def build(s: Step): RequestDecoder.Fn[Output]

  def map[X](x: Output => X) = new RequestDecoder[Step] {
    type Output = X

    override def build(s: Step): RequestDecoder.Fn[X] = {
      val built = RequestDecoder.this.build(s)
      context => {
        import context._
        built(context).map { value =>
          value.right.map {
            case (context2, out) =>
              context2 -> x(out)
          }
        }
      }
    }
  }
}

object RequestDecoder {

  sealed trait DecodingError {
    def msg: String
  }
  object DecodingError {
    case class MissingExpectedValue(msg: String) extends DecodingError
    case class InvalidPath(msg: String) extends DecodingError
    case class InvalidPayload(msg: String) extends DecodingError
  }

  type Fn[Output] = RequestContext => Future[Either[DecodingError, (RequestContext, Output)]]

  type Aux[Step, OutputT] = RequestDecoder[Step] {
    type Output = OutputT
  }

  def apply[T](implicit rd: RequestDecoder[T]): Aux[T, rd.Output] = rd

  private def make[Step, Producing](f: Step => Fn[Producing]): Aux[Step, Producing] = new RequestDecoder[Step] {
    override type Output = Producing

    override def build(s: Step): Fn[Output] = {
      f(s)
    }
  }

  implicit val stringPathDecoder = make[Input.ExtraPath.type, String] { _ => request =>
    val matcher = PathMatchers.Neutral / PathMatchers.Segment
    matcher(request.unmatchedPath) match {
      case Matched(rest, ok) =>
        val updated = request.withUnmatchedPath(rest)
        Future.successful(
          Right(updated -> ok._1)
        )
      case Unmatched =>
        RequestDecoder.failedDecoding(DecodingError.InvalidPath("Expected sub path"))
    }
  }

  implicit val fixedPathDecoder = make[Input.ExtraPathFixed, Unit] { step =>
    {
      val matcher = PathMatchers.Neutral / PathMatchers.Segments(step.pathElements.size)
      request =>
        matcher.apply(request.unmatchedPath) match {
          case Matched(rest, matches) if matches._1 == step.pathElements =>
            val updatedRequest = request.withUnmatchedPath(rest)
            Future.successful(Right(updatedRequest -> (())))
          case Matched(_, _) =>
            RequestDecoder.failedDecoding(DecodingError.InvalidPath("Expected a different path"))
          case _ =>
            RequestDecoder.failedDecoding(DecodingError.InvalidPath(s"Expected path ${step.pathElements}"))
        }
    }
  }

  implicit def mapped[T] = make[Input.MappedPayload[T], T] { step =>
    implicit val unmarshaller = AkkaHttpHelper.unmarshallerFromMapping[T](step.mapping)
    requestContext => {
      import requestContext._
      Unmarshal(request.entity).to[T].map { decoded =>
        Right(requestContext -> decoded)
      }
    }
  }

  implicit val binaryStreamDecoder = make[Input.Binary.type, (String, Source[ByteString, _])] { step => requestContext => {
    val entity = requestContext.request.entity
    val contentType = entity.contentType.value
    val dataSource = entity.dataBytes
    Future.successful(
      Right(requestContext -> (contentType, dataSource))
    )
  }
  }

  implicit val queryParameterDecoder = make[Input.AddQueryParameter, String] { step => requestContext => {
    val v = requestContext.request.uri.query().get(step.name).getOrElse {
      throw new IllegalArgumentException("Missing query parameter")
    }
    Future.successful(Right(requestContext -> v))
  }
  }

  implicit def queryParameterMapDecoder[T] = make[Input.QueryParameterMap[T], T] { step => requestContext => {
    val queryParameters = requestContext.request.uri.query().toMap
    step.mapping.decode(queryParameters) match {
      case Left(error) =>
        // TODO: Better handling
        throw new IllegalArgumentException(s"Could not parse request ${error}")
      case Right(ok) =>
        Future.successful(Right(requestContext -> ok))
    }
  }
  }

  implicit val headerValueDecoder = make[Input.AddHeader, String] { step => requestContext => {
    val headerValue = requestContext.request.headers.collectFirst {
      case h if h.name() == step.name => h.value()
    }.getOrElse {
      throw new IllegalArgumentException(s"Missing expected header ${step.name}")
    }
    Future.successful(Right(requestContext -> headerValue))
  }
  }

  import scala.concurrent.duration._
  private val MultipartBufferingTimeout = 60.seconds // TODO: Make this changeable.

  implicit def multipartDecoder[T <: HList, ProducingH <: HList, Producing](
    implicit
    multipartDecoder: MultipartDecoder.Aux[T, ProducingH],
    argumentLister: SimpleArgumentLister.Aux[ProducingH, Producing]
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
          case Right(ok) => Right(requestContext -> argumentLister.unlift(ok))
        }
      }
    }
  }

  implicit def decodeMapped[A, B, T <: TypedInput[A], V](
    implicit
    aux: RequestDecoder.Aux[T, A]
  ) = make[Input.MappedInput[A, B, T], B] { step =>
    val built = aux.build(step.original)
    requestContext => {
      import requestContext._
      built(requestContext).map {
        _.right.map {
          case (context, value) =>
            val encodedValue = step.mapping.decode(value).getOrElse {
              throw new IllegalArgumentException("Could not encode value")
            }
            context -> encodedValue
        }
      }
    }
  }

  implicit val nilDecoder = make[HNil, HNil] { _ => requestContext => {
    Future.successful(Right(requestContext -> HNil))
  }
  }

  implicit def elemDecoder[H, T <: HList, X <: HList](
    implicit
    h: RequestDecoder[H],
    aux: RequestDecoder.Aux[T, X]
  ) = make[H :: T, h.Output :: X] { step =>
    // head first (? really? )
    val headPrepared = h.build(step.head)
    val tailPrepared = aux.build(step.tail)
    requestContext => {
      import requestContext._

      headPrepared(requestContext).flatMap {
        case Left(bad) => Future.successful(Left(bad))
        case Right((afterHeadRequest, afterHeadResult)) =>
          tailPrepared(afterHeadRequest).map {
            _.right.map {
              case (afterTailRequest, afterTailResult) =>
                afterTailRequest -> (afterHeadResult :: afterTailResult)
            }
          }
      }
    }
  }

  /** Convert the result of an unmarshalling operation into a future with extracted decoding error. */
  private[codecs] def decodeResultToResult[T](in: Future[T])(implicit ec: ExecutionContext): Future[Either[DecodingError, T]] = {
    in.transform {
      case Failure(error) => Success(Left(DecodingError.InvalidPayload(error.getMessage)))
      case Success(value) => Success(Right(value))
    }
  }

  private[codecs] def failedDecoding(e: DecodingError): Future[Either[DecodingError, Nothing]] = {
    Future.successful(Left(e))
  }

}

