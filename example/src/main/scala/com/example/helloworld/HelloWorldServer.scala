package com.example.helloworld

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import net.reactivecore.fhttp.akka.{ ApiServer, RouteBuilder }

import scala.concurrent.{ ExecutionContext, Future }

object HelloWorldServer extends App {
  implicit val ac = ActorSystem()
  implicit val mat = ActorMaterializer()
  implicit def ec: ExecutionContext = ac.dispatcher

  val server = new ApiServer(
    routes = List(

      RouteBuilder.bind(HelloWorldApi.version).to { _ =>
        Future.successful("Hello World v 1.0")
      },

      RouteBuilder.bind(HelloWorldApi.sum).to { numbers =>
        Future {
          NumberResult(numbers.a + numbers.b)
        }
      },

      RouteBuilder.bind(HelloWorldApi.divide).to { numbers =>
        Future {
          if (numbers.b == 0) {
            Left(400 -> "Cannot divide by zero")
          } else {
            Right(NumberResult(numbers.a / numbers.b))
          }
        }
      }
    )
  )

}
