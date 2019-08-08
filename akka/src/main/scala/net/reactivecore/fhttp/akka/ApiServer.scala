package net.reactivecore.fhttp.akka

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.{ Http, HttpExt }
import akka.http.scaladsl.server.Route
import akka.stream.Materializer

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._
import scala.collection.immutable

/** Dummy server for testing Web APis. */
class ApiServer(
    routes: immutable.Seq[Route],
    interface: String = "localhost",
    port: Int = 9000
)(implicit as: ActorSystem, materializer: Materializer) {
  private val HttpUpDownTimeout = 60.seconds

  private val http = Http()
  private val combinedRoute: Route = concat(routes: _*)
  private val bindFuture = http.bindAndHandle(combinedRoute, interface, port)
  private val bindResult = Await.result(bindFuture, HttpUpDownTimeout)

  println(s"Server listening on ${interface}:${port}")

  def close(): Unit = {
    bindResult.terminate(HttpUpDownTimeout)
  }

}
