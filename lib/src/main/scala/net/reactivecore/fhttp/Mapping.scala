package net.reactivecore.fhttp

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import io.circe.{ Decoder, Encoder }

/** Maps some [T] from and to plain bytes. */
trait Mapping[T] {

  /** Content Type for serializing and accepting */
  val contentType: String

  def encode(value: T): ByteBuffer

  def decode(in: ByteBuffer): Either[String, T]
}

object TextMapping extends Mapping[String] {
  override val contentType: String = "text/html; charset=utf-8"

  override def encode(value: String): ByteBuffer = ByteBuffer.wrap(value.getBytes(StandardCharsets.UTF_8))

  override def decode(in: ByteBuffer): Either[String, String] = {
    Right(StandardCharsets.UTF_8.decode(in).toString)
  }
}

// In future this could go into it's own library.
case class CirceJsonMapping[T]()(implicit encoder: Encoder[T], decoder: Decoder[T]) extends Mapping[T] {
  override val contentType: String = "application/json"

  override def encode(value: T): ByteBuffer = ByteBuffer.wrap(encoder.apply(value).toString().getBytes(StandardCharsets.UTF_8))

  override def decode(in: ByteBuffer): Either[String, T] = {
    val result = for {
      parsed <- io.circe.jawn.parseByteBuffer(in)
      converted <- parsed.as[T]
    } yield converted
    result.left.map(_.toString)
  }
}