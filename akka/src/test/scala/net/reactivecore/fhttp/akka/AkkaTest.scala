package net.reactivecore.fhttp.akka

import akka.http.scaladsl.model.{ HttpRequest, HttpResponse }
import akka.stream.scaladsl.{ Sink, Source }
import akka.util.ByteString
import net.reactivecore.fhttp.{ ApiBuilder, Input, Output }
import shapeless._

import scala.concurrent.Future
import scala.language.reflectiveCalls
import scala.util.Try

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

    val HeaderParameter = add(
      get("header_parameter")
        .expecting(Input.AddHeader("header1"))
        .responding(Output.text())
    )

    val Multipart = add(
      post("multipart")
        .expecting(Input.Multipart.make(
          Input.Multipart.MultipartText("text1"),
          Input.Multipart.MultipartFile("file1")
        ))
        .responding(Output.text())
    )

    val DeepPath = add(
      get("deep", "path")
        .expecting(Input.ExtraPath)
        .expecting(Input.ExtraPathFixed(List("foo", "bar")))
        .expecting(Input.ExtraPath)
        .responding(Output.text())
    )

    val intMapping = input.pureMapping[String, Int](
      x => Right(x.toInt),
      x => Try(x.toString).toEither.left.map(_.toString)
    )

    val MappedQueryParameter = add(
      get("foo")
        .expecting(Input.MappedInput(Input.AddQueryParameter("number"), intMapping))
        .expecting(Input.AddQueryParameter("string"))
        .responding(Output.text())
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

      bind(Api1.HeaderParameter).to { input =>
        Future.successful(input)
      }

      bind(Api1.Multipart).to {
        case (text, (contentType, data)) =>
          val collected = collectByteSource(data).utf8String
          Future.successful(text + "," + contentType + "," + collected)
      }

      bind(Api1.MappedQueryParameter).to {
        case (x, y) =>
          Future.successful(y + (x + 1).toString)
      }

      bind(Api1.DeepPath).to {
        case (first, second) =>
          Future.successful(s"${first},${second}")
      }
    }
    val server = new ApiServer(
      route
    )

    val requestExecutor: HttpRequest => Future[HttpResponse] = { req =>
      println(s"Executing ${req.method} ${req.uri}")
      http.singleRequest(req)
    }

    val client = new ApiClient(requestExecutor, "http://localhost:9000") {
      val helloWorldPrepared = prepare(Api1.HelloWorld)

      val errorResponsePrepared = prepare(Api1.ErrorResponse)

      val pathDependent = prepare(Api1.PathDependent)

      val uploadClient = prepare(Api1.FileUpload)

      val downloadClient = prepare(Api1.FileDownload)

      val queryPrepared = prepare(Api1.QueryParameters)

      val headerParameter = prepare(Api1.HeaderParameter)

      val multipart = prepare(Api1.Multipart)

      val mappedQueryParameter = prepare(Api1.MappedQueryParameter)

      val deepPath = prepare(Api1.DeepPath)

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

    val response8 = await(client.headerParameter.apply("boom!"))
    response8 shouldBe "boom!"

    val response9 = await(client.multipart.apply(
      ("foo", "application/octet-stream" -> Source(List(ByteString("abc"), ByteString("cde"))))
    ))

    response9 shouldBe "foo,application/octet-stream,abccde"

    val response10 = await(client.mappedQueryParameter((10, "Hello")))
    response10 shouldBe "Hello11"

    val response11 = await(client.deepPath("1", "2"))
    response11 shouldBe "1,2"
  }

}
