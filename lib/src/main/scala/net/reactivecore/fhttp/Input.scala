package net.reactivecore.fhttp

import io.circe.{ Decoder, Encoder, ObjectEncoder }

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
  /** Add an extra dynamic path. */
  case object ExtraPath extends TypedInput[String]
  /** Map payload to some value. */
  case class Mapped[T](mapping: Mapping[T], maxLength: Option[Long] = None) extends TypedInput[T]
  /** Expect a binary input stream (with content type). */
  case object Binary extends TypedInput[(String, Array[Byte])]
  /** Add a query parameter */
  case class AddQueryParameter(name: String) extends TypedInput[String]
  /** Parse query parameters into a circe-extended class, values are converted into strings before. */
  case class QueryParameterMap[T](mapping: PureMapping[Map[String, String], T]) extends TypedInput[T]
  /** Add an header with value. */
  case class AddHeader(name: String) extends TypedInput[String]

  def text(limit: Option[Long] = None): Mapped[String] = Mapped(TextMapping, limit)
  def circe[T](limit: Option[Long] = None)(implicit encoder: Encoder[T], decoder: Decoder[T]): Mapped[T] = Mapped(CirceJsonMapping[T], limit)
  def circeQuery[T](implicit encoder: ObjectEncoder[T], decoder: Decoder[T]) = QueryParameterMap(
    CirceJsonStringMapping()
  )
}
