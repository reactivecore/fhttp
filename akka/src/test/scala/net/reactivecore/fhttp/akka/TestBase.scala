package net.reactivecore.fhttp.akka

import akka.actor.ActorSystem
import akka.http.scaladsl.{ Http, HttpExt }
import akka.stream.scaladsl.{ Sink, Source }
import akka.stream.{ ActorMaterializer, Materializer }
import akka.util.ByteString
import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers }

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._

abstract class TestBase extends FlatSpec with Matchers with BeforeAndAfterAll {

  private var _actorSystem: ActorSystem = _
  private var _materializer: Materializer = _
  private var _http: HttpExt = _

  implicit protected def actorSystem: ActorSystem = _actorSystem
  implicit protected def materializer: Materializer = _materializer
  implicit protected def ec: ExecutionContext = _actorSystem.dispatcher

  protected def http: HttpExt = _http

  override protected def beforeAll(): Unit = {
    _actorSystem = ActorSystem("test")
    _materializer = ActorMaterializer()
    _http = Http()
  }

  override protected def afterAll(): Unit = {
    _actorSystem.terminate()
    _actorSystem = null
    _materializer = null
    _http = null
  }

  protected def await[T](in: Future[T]): T = {
    Await.result(in, 10.seconds)
  }

  protected def collectByteSource(in: Source[ByteString, _]): ByteString = {
    await(in.runWith(Sink.seq)).foldLeft(ByteString())(_ ++ _)
  }
}
