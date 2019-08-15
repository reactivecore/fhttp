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
  /** Mapped Entity Payload. */
  case class MappedPayload[T](mapping: Mapping[T], limit: Option[Long] = None) extends TypedOutput[T]
  /**
   * Binary content
   * Resulting type is binding dependent, but should contain content type and byte data.
   */
  case object Binary extends Output

  /** An empty response (useful for an empty case in ErrorSuccess). */
  case object Empty extends Output

  /** Splits Error and success case into an either. The failure case should include the HTTP Status code. */
  case class ErrorSuccess[Failure <: Output, Success <: Output](f: Failure, s: Success) extends TypedOutput[Either[_, _]]

  def text(limit: Option[Long] = None): MappedPayload[String] = MappedPayload(TextMapping, limit)
  def circe[T](limit: Option[Long] = None)(implicit encoder: Encoder[T], decoder: Decoder[T]): MappedPayload[T] = MappedPayload(CirceJsonMapping[T], limit)
}
