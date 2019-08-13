package net.reactivecore.fhttp

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import io.circe.{ Decoder, Encoder, Json, ObjectEncoder }

trait PureMapping[From, To] {
  def encode(value: To): Either[String, From]

  def decode(in: From): Either[String, To]
}

/** Maps some [T] from and to plain bytes. */
trait Mapping[T] extends PureMapping[ByteBuffer, T] {

  /** Content Type for serializing and accepting */
  val contentType: String
}

object TextMapping extends Mapping[String] {
  override val contentType: String = "text/plain; charset=utf-8"

  override def encode(value: String): Either[String, ByteBuffer] = Right(ByteBuffer.wrap(value.getBytes(StandardCharsets.UTF_8)))

  override def decode(in: ByteBuffer): Either[String, String] = {
    Right(StandardCharsets.UTF_8.decode(in).toString)
  }
}

// In future this could go into it's own library.
case class CirceJsonMapping[T]()(implicit encoder: Encoder[T], decoder: Decoder[T]) extends Mapping[T] {
  override val contentType: String = "application/json"

  override def encode(value: T): Either[String, ByteBuffer] = Right(ByteBuffer.wrap(encoder.apply(value).toString().getBytes(StandardCharsets.UTF_8)))

  override def decode(in: ByteBuffer): Either[String, T] = {
    val result = for {
      parsed <- io.circe.jawn.parseByteBuffer(in)
      converted <- parsed.as[T]
    } yield converted
    result.left.map(_.toString)
  }
}

// Mapping used for query encoding
// Note: only works correctly for objects serializing into string objects.
case class CirceJsonStringMapping[T]()(implicit encoder: ObjectEncoder[T], decoder: Decoder[T]) extends PureMapping[Map[String, String], T] {

  override def encode(value: T): Either[String, Map[String, String]] = {
    val result = encoder.encodeObject(value).toList.flatMap {
      case (key, value) if value.isNull => None
      case (key, value) =>
        value.asString match {
          case None    => Some(key -> value.toString())
          case Some(s) => Some(key -> s)
        }
    }.toMap
    Right(result)
  }

  override def decode(in: Map[String, String]): Either[String, T] = {
    val json = Json.obj(in.map {
      case (key, value) =>
        key -> Json.fromString(value)
    }.toSeq: _*)
    decoder.decodeJson(json).left.map(_.toString())
  }
}

