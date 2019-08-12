package net.reactivecore.fhttp.akka

import akka.http.scaladsl.server.{ RequestContext, Route, RouteConcatenation, RouteResult }
import net.reactivecore.fhttp.ApiCall
import net.reactivecore.fhttp.akka.codecs.{ RequestDecoder, ResponseEncoder }
import net.reactivecore.fhttp.helper.SimpleArgumentLister
import shapeless.HList

import scala.concurrent.Future

/**
 * Like [[RouteBuilder]] but concatenates multiple Calls into one single route.
 * Note: there is mutable state, collecting all routes.
 */
trait ApiServerRoute extends Route {

  private val routeBuilder = List.newBuilder[Route]

  /** Bind an Api Call to a function and add it to the routes. */
  protected def bind[In <: HList, Out <: HList, ArgumentH <: HList, Argument, ResultH <: HList, Result](
    call: ApiCall[In, Out]
  )(
    implicit
    requestDecoder: RequestDecoder.Aux[In, ArgumentH],
    argumentLister: SimpleArgumentLister.Aux[ArgumentH, Argument],
    resultEncoder: ResponseEncoder.Aux[Out, ResultH],
    responseLister: SimpleArgumentLister.Aux[ResultH, Result]
  ): AddingBinder[In, Out, Argument, Result] = {
    val binder = RouteBuilder.bind(call)
    AddingBinder(call, binder)
  }

  /** Add (another kind of) to the result. */
  protected def add(route: Route): Unit = {
    routeBuilder += route
  }

  case class AddingBinder[In <: HList, Out <: HList, Argument, Result](
      call: ApiCall[In, Out],
      binder: RouteBuilder.Binder[Argument, Result]
  ) {

    def to(f: Argument => Future[Result]): Unit = {
      val converted = binder.to(f)
      routeBuilder += converted
    }
  }

  /** Resulting concatenated route. */
  lazy val route: Route = {
    val allRoutes = routeBuilder.result()
    RouteConcatenation.concat(allRoutes: _*)
  }

  override def apply(v1: RequestContext): Future[RouteResult] = {
    route(v1)
  }
}
