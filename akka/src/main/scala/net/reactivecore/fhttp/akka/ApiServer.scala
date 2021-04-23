package net.reactivecore.fhttp.akka

import akka.actor.ActorSystem
import akka.http.scaladsl.Http.HttpTerminated
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.{ Http, HttpExt }
import akka.http.scaladsl.server.Route
import akka.stream.Materializer

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.collection.immutable

/** Dummy server for testing Web APis. */
class ApiServer(
    route: Route,
    interface: String = "localhost",
    port: Int = 9000
)(implicit as: ActorSystem, materializer: Materializer) {
  private val HttpUpDownTimeout = 60.seconds

  private val http = Http()
  private val bindFuture = http.newServerAt(interface, port).bind(route)
  private val bindResult = Await.result(bindFuture, HttpUpDownTimeout)

  println(s"Server listening on ${interface}:${port}")

  def close(): Future[HttpTerminated] = {
    bindResult.terminate(HttpUpDownTimeout)
  }

}
