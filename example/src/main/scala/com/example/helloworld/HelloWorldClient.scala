package com.example.helloworld

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.{ ActorMaterializer, Materializer }
import net.reactivecore.fhttp.akka.ApiClient
import shapeless.HNil

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.language.reflectiveCalls

object HelloWorldClient extends App {

  implicit val actorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val materializer = Materializer.apply(actorSystem)
  val http = Http()
  val client = new ApiClient(http, "http://localhost:9000") {
    val version = prepare(HelloWorldApi.version)
    val sum = prepare(HelloWorldApi.sum)
    val divide = prepare(HelloWorldApi.divide)
  }

  def await[T](in: Future[T]): T = Await.result(in, 10.seconds)

  val versionResponse = await(client.version(()))
  println(s"Connected to ${versionResponse}")

  val sumResponse = await(client.sum(Numbers(4, 5)))
  println(s"Sum: ${sumResponse}")

  val divideResponse = await(client.divide(Numbers(8, 2)))
  println(s"Divide Response (success): ${divideResponse}")

  val badDivision = await(client.divide(Numbers(8, 0)))
  println(s"Divide Response (bad): ${badDivision}")
}
