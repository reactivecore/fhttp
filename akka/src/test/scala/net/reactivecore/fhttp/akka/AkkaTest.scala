package net.reactivecore.fhttp.akka

import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.circe.generic.JsonCodec
import net.reactivecore.fhttp.{ ApiBuilder, Input, Output }

import scala.concurrent.Future
import shapeless._

import scala.language.reflectiveCalls
import io.circe.syntax._

class AkkaTest extends TestBase {

  object Api1 extends ApiBuilder {
    val HelloWorld = add(
      get("hello")
        .responding(Output.text())
    )

    val ErrorResponse = add(
      get("error")
        .responding(
          Output.ErrorSuccess(
            Output.text(),
            Output.text()
          )
        )
    )

    val PathDependent = add(
      get("path")
        .expecting(Input.ExtraPath)
        .responding(Output.text())
    )

    val FileUpload = add(
      post("upload")
        .expecting(Input.Binary)
        .responding(Output.text())
    )

    val FileDownload = add(
      get("download")
        .expecting(Input.ExtraPath)
        .responding(Output.Binary)
    )

    val QueryParameters = add(
      get("query")
        .expecting(Input.circeQuery[QueryParameters])
        .responding(Output.circe[QueryParameters]())
    )
  }

  it should "server this simple examples" in {
    val route = new ApiServerRoute {
      bind(Api1.HelloWorld).to { _ =>
        Future.successful("Hello World")
      }

      bind(Api1.ErrorResponse).to { _ =>
        Future.successful(
          Left(401 -> "Forbidden")
        )
      }

      bind(Api1.PathDependent).to { x =>
        Future.successful(x)
      }

      bind(Api1.FileUpload).to { x =>
        val contentType = x._1: String
        val dataSource = x._2: Source[ByteString, _]

        dataSource.map { bs =>
          bs.utf8String
        }.runWith(Sink.seq).map { result =>
          contentType + " --> " + result.mkString(",")
        }
      }

      bind(Api1.FileDownload).to { fileName =>
        Future.successful("plain/text" -> Source(List(ByteString(fileName), ByteString("A Nice text file"))))
      }

      bind(Api1.QueryParameters).to { input =>
        Future.successful(input)
      }
    }
    val server = new ApiServer(
      route
    )

    val client = new ApiClient(http, "http://localhost:9000") {
      val helloWorldPrepared = prepare(Api1.HelloWorld)

      val errorResponsePrepared = prepare(Api1.ErrorResponse)

      val pathDependent = prepare(Api1.PathDependent)

      val uploadClient = prepare(Api1.FileUpload)

      val downloadClient = prepare(Api1.FileDownload)

      val queryPrepared = prepare(Api1.QueryParameters)

    }

    val response = await(client.helloWorldPrepared(HNil))
    response shouldBe "Hello World"

    val response2 = await(client.errorResponsePrepared(HNil))
    response2 shouldBe Left(401 -> "Forbidden")

    val response3 = await(client.pathDependent("file1"))
    response3 shouldBe "file1"

    val response4 = await(client.uploadClient("application/octet-stream", Source(
      List(ByteString("a"), ByteString("b"))
    )))

    response4 shouldBe "application/octet-stream --> a,b"

    val response5: (String, Source[ByteString, _]) = await(client.downloadClient("path"))
    collectByteSource(response5._2).utf8String shouldBe "pathA Nice text file"

    val response6 = await(client.queryPrepared(QueryParameters(a = "Hello", b = None)))
    response6 shouldBe QueryParameters("Hello", None)

    val response7 = await(client.queryPrepared(QueryParameters(a = "Hello", b = Some("foo"))))
    response7 shouldBe QueryParameters("Hello", Some("foo"))
  }

}
