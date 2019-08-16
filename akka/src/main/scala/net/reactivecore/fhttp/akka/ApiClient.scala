package net.reactivecore.fhttp.akka

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.{ HttpRequest, HttpResponse, Uri }
import akka.stream.Materializer
import net.reactivecore.fhttp.ApiCall
import net.reactivecore.fhttp.akka.ApiClient.RequestExecutor
import net.reactivecore.fhttp.akka.codecs.{ DecodingContext, RequestEncoder, ResponseDecoder }
import net.reactivecore.fhttp.helper.{ SimpleArgumentLister, VTree }
import net.reactivecore.fhttp.helper.VTree.TupleConversion
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }

class ApiClient(requestExecutor: RequestExecutor, rootUri: Uri)(implicit ec: ExecutionContext, materializer: Materializer) {

  def this(httpExt: HttpExt, rootUri: Uri)(implicit ec: ExecutionContext, materializer: Materializer) {
    this(httpExt.singleRequest(_), rootUri)
  }

  def prepare[In <: HList, Out <: HList, ArgumentH <: HList, Argument, ResultT <: VTree, Result](call: ApiCall[In, Out])(
    implicit
    encoder: RequestEncoder.Aux[In, ArgumentH],
    argumentLister: SimpleArgumentLister.Aux[ArgumentH, Argument],
    responseDecoder: ResponseDecoder.Aux[Out, ResultT],
    responseConversion: TupleConversion.Aux[ResultT, Result]
  ): Argument => Future[Result] = {
    val requestBuilder = prepareHttpRequestBuilder(call)
    val builtResponseDecoder = responseDecoder.build(call.output)
    val liftedDecoder = ResponseDecoder.mapFn(builtResponseDecoder, responseConversion.toTuple)
    args => {
      val argsLifted = argumentLister.lift(args)
      val req = requestBuilder(argsLifted)
      val context = new DecodingContext()
      val responseFuture = requestExecutor(req)
      responseFuture.flatMap { response =>
        liftedDecoder(response, context)
      }
    }
  }

  private def prepareHttpRequestBuilder[In <: HList, ArgumentH <: HList, Argument](call: ApiCall[In, _])(
    implicit
    encoder: codecs.RequestEncoder.Aux[In, Argument]
  ): Argument => HttpRequest = {
    val method = AkkaHttpHelper.methodForName(call.header.method)

    val fullPath = call.header.path.foldLeft(rootUri.path)(_ / _)
    val fullUri = rootUri.withPath(fullPath)

    val base = HttpRequest(
      method,
      fullUri
    )

    val mainEncoder = encoder.build(call.input)

    args => {
      mainEncoder(base, args)
    }
  }
}

object ApiClient {
  type RequestExecutor = HttpRequest => Future[HttpResponse]
}