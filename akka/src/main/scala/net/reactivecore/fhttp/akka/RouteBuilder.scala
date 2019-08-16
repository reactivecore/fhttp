package net.reactivecore.fhttp.akka

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.{ MethodRejection, RequestContext, Route, RouteResult }
import net.reactivecore.fhttp.akka.codecs.RequestDecoder.DecodingError
import net.reactivecore.fhttp.{ ApiCall, ApiHeader }
import net.reactivecore.fhttp.akka.codecs.{ RequestDecoder, ResponseEncoder }
import net.reactivecore.fhttp.helper.{ SimpleArgumentLister, VTree }
import net.reactivecore.fhttp.helper.VTree.TupleConversion
import shapeless._

import scala.concurrent.Future

/** Creates Akka Routes from [[ApiCall]] */
trait RouteBuilder {

  def bind[In <: HList, Out <: HList, ArgumentH <: HList, Argument, ResultV <: VTree, Result](call: ApiCall[In, Out])(
    implicit
    requestDecoder: RequestDecoder.Aux[In, ArgumentH],
    argumentLister: SimpleArgumentLister.Aux[ArgumentH, Argument],
    resultEncoder: ResponseEncoder.Aux[Out, ResultV],
    responseConversion: TupleConversion.Aux[ResultV, Result]
  ): Binder[Argument, Result] = {
    val appliedDecoder = requestDecoder.build(call.input)
    val appliedEncoder = resultEncoder.build(call.output)

    val liftedInput: RequestDecoder.Fn[Argument] = { request =>
      import request.executionContext
      appliedDecoder(request).map {
        _.right.map {
          case (req, value) => req -> argumentLister.unlift(value)
        }
      }
    }

    val liftedOutput = ResponseEncoder.contraMapFn(appliedEncoder, responseConversion.fromTuple)

    Binder(
      call.header,
      liftedInput,
      liftedOutput
    )
  }

  case class Binder[Argument, Result](
      header: ApiHeader,
      requestDecoder: RequestDecoder.Fn[Argument],
      responseEncoder: ResponseEncoder.Fn[Result]
  ) {

    def to(f: Argument => Future[Result]): Route = {
      val requiredMethod = AkkaHttpHelper.methodForName(header.method)
      val requiredPath = AkkaHttpHelper.forceParsePath(header.path)

      requestContext => {
        if (requestContext.request.method != requiredMethod) {
          requestContext.reject(MethodRejection(requiredMethod))
        } else {
          if (requestContext.unmatchedPath.startsWith(requiredPath)) {
            val pending = requestContext.unmatchedPath.dropChars(requiredPath.charCount)
            if (pending.isEmpty || pending.startsWithSlash) {
              val updated = requestContext.withUnmatchedPath(pending)
              executeSelectedRequest(updated, f)
            } else {
              requestContext.reject()
            }
          } else {
            requestContext.reject()
          }
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