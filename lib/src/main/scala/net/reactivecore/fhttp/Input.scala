package net.reactivecore.fhttp

import io.circe.{ Decoder, Encoder }

/**
 * Describes an Input Transformation
 */
trait Input

/**
 * A Typed input
 * @tparam T a hint what should be parsed on some input transformations (can be wrapped in some Monad etc.)
 */
trait TypedInput[T] extends Input

object Input {
  case object ExtraPath extends TypedInput[String]
  case class Mapped[T](mapping: Mapping[T], maxLength: Option[Long] = None) extends TypedInput[T]
  case object Binary extends TypedInput[(String, Array[Byte])]
  case class AddQueryParameter(name: String) extends TypedInput[String]

  def text(limit: Option[Long] = None): Mapped[String] = Mapped(TextMapping, limit)
  def circe[T](limit: Option[Long] = None)(implicit encoder: Encoder[T], decoder: Decoder[T]): Mapped[T] = Mapped(CirceJsonMapping[T], limit)
}
