package net.reactivecore.fhttp.akka

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.{ RequestContext, Route, RouteResult }
import net.reactivecore.fhttp.akka.codecs.RequestDecoder.DecodingError
import net.reactivecore.fhttp.{ ApiCall, ApiHeader }
import net.reactivecore.fhttp.akka.codecs.{ RequestDecoder, ResponseEncoder }
import net.reactivecore.fhttp.helper.SimpleArgumentLister
import shapeless._

import scala.concurrent.Future

/** Creates Akka Routes from [[ApiCall]] */
trait RouteBuilder {

  def bind[In <: HList, Out <: HList, ArgumentH <: HList, Argument, ResultH <: HList, Result](call: ApiCall[In, Out])(
    implicit
    requestDecoder: RequestDecoder.Aux[In, ArgumentH],
    argumentLister: SimpleArgumentLister.Aux[ArgumentH, Argument],
    resultEncoder: ResponseEncoder.Aux[Out, ResultH],
    responseLister: SimpleArgumentLister.Aux[ResultH, Result]
  ): Binder[Argument, Result] = Binder(
    call.header,
    requestDecoder.map(argumentLister.unlift).build(call.input),
    resultEncoder.contraMap(responseLister.lift).build(call.output)
  )

  case class Binder[Argument, Result](
      header: ApiHeader,
      requestDecoder: RequestDecoder.Fn[Argument],
      responseEncoder: ResponseEncoder.Fn[Result]
  ) {

    def to(f: Argument => Future[Result]): Route = {
      val requiredMethod = AkkaHttpHelper.methodForName(header.method)
      val requiredPath = AkkaHttpHelper.forceParsePath(header.path)

      requestContext => {
        if (requestContext.request.method == requiredMethod &&
          requestContext.unmatchedPath.startsWith(requiredPath)) {

          val updated = requestContext.mapUnmatchedPath { path =>
            path.dropChars(requiredPath.charCount)
          }
          executeSelectedRequest(updated, f)
        } else {
          requestContext.reject()
        }
      }
    }

    /** Execute a Request assuming that prefix path and Method are already checked. */
    private def executeSelectedRequest(requestContext: RequestContext, f: Argument => Future[Result]): Future[RouteResult] = {
      import requestContext._

      requestDecoder(requestContext).flatMap {
        case Left(e: DecodingError.MissingExpectedValue) =>
          requestContext.reject()
        case Left(e: DecodingError.InvalidPath) =>
          requestContext.reject()
        case Left(e: DecodingError.InvalidPayload) =>
          requestContext.reject()
        case Right((_, decoded)) =>
          for {
            executed <- f(decoded)
            rootResponse = HttpResponse()
            serialized = responseEncoder.apply(rootResponse, executed)
          } yield {
            RouteResult.Complete(serialized)
          }
      }
    }
  }
}

object RouteBuilder extends RouteBuilder