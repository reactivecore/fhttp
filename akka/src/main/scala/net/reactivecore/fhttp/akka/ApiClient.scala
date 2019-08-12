package net.reactivecore.fhttp.akka

import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.{ HttpRequest, Uri }
import akka.stream.Materializer
import net.reactivecore.fhttp.ApiCall
import net.reactivecore.fhttp.akka.codecs.{ DecodingContext, RequestEncoder, ResponseDecoder }
import net.reactivecore.fhttp.helper.SimpleArgumentLister
import shapeless._

import scala.concurrent.{ ExecutionContext, Future }

class ApiClient(httpExt: HttpExt, rootUri: Uri)(implicit ec: ExecutionContext, materializer: Materializer) {

  def prepare[In <: HList, Out <: HList, ArgumentH <: HList, Argument, ResultH <: HList, Result](call: ApiCall[In, Out])(
    implicit
    encoder: RequestEncoder.Aux[In, ArgumentH],
    argumentLister: SimpleArgumentLister.Aux[ArgumentH, Argument],
    responseDecoder: ResponseDecoder.Aux[Out, ResultH],
    resultLister: SimpleArgumentLister.Aux[ResultH, Result]
  ): Argument => Future[Result] = {
    val requestBuilder = prepareHttpRequestBuilder(call)(encoder.contraMap(argumentLister.lift))
    val decoder = responseDecoder.map(resultLister.unlift).build(call.output)
    args => {
      val req = requestBuilder(args)
      val context = new DecodingContext()
      val responseFuture = httpExt.singleRequest(req)
      responseFuture.flatMap { response =>
        decoder(response, context)
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
