package net.reactivecore.fhttp

import io.circe.{ Decoder, Encoder }

/** Describes an Output Transformation */
trait Output

/**
 * A Typed input
 * @tparam T a hint what should be parsed on some input transformations (can be wrapped in some Monad etc.)
 */
trait TypedOutput[T] extends Output

object Output {
  case class Mapped[T](mapping: Mapping[T], limit: Option[Long] = None) extends TypedOutput[T]
  case object Binary extends TypedOutput[(String, Array[Byte])]
  case class ErrorSuccess[Failure <: Output, Success <: Output](f: Failure, s: Success) extends TypedOutput[Either[_, _]]

  def text(limit: Option[Long] = None): Mapped[String] = Mapped(TextMapping, limit)
  def circe[T](limit: Option[Long] = None)(implicit encoder: Encoder[T], decoder: Decoder[T]): Mapped[T] = Mapped(CirceJsonMapping[T], limit)
}
