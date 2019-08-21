package net.reactivecore.fhttp.akka

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.{ ContentType, ContentTypes, HttpEntity, HttpMethod, HttpMethods }
import akka.http.scaladsl.unmarshalling.Unmarshaller
import io.circe.{ Decoder, Encoder }
import net.reactivecore.fhttp.Mapping

private[akka] object AkkaHttpHelper {

  def methodForName(name: String): HttpMethod = {
    HttpMethods.getForKeyCaseInsensitive(name).getOrElse {
      throw new IllegalArgumentException(s"Unknown method ${name}")
    }
  }

  def forceParsePath(path: List[String]): Path = {
    if (path.isEmpty) {
      Path.SingleSlash
    } else {
      path.foldLeft(Path.Empty: Path)(_ / _)
    }
  }

  def binaryContentTypeForName(name: String): ContentType.Binary = {
    ContentType.parse(name) match {
      case Left(notFound)               => throw new IllegalArgumentException(s"Invalid content type ${name}")
      case Right(x: ContentType.Binary) => x
      case Right(other)                 => throw new IllegalArgumentException(s"Expected binary, got non binary content type ${name}")
    }
  }

  def forceContentType(name: String): ContentType = {
    ContentType.parse(name).right.getOrElse {
      throw new IllegalArgumentException(s"Invalid content type ${name}")
    }
  }

  def unmarshallerFromMapping[T](mapping: Mapping[T]) = {
    val contentType = forceContentType(mapping.contentType)

    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(
        contentType
      )
      .map { byteString =>
        mapping.decode(byteString.asByteBuffer) match {
          case Left(error) => throw new RuntimeException(s"Could not decode ${error}")
          case Right(ok)   => ok
        }
      }
  }
}
