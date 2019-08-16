package net.reactivecore.fhttp

import io.circe.{ Decoder, Encoder, ObjectEncoder }
import shapeless._

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
  /** Add an extra non-dynamic path (to be used after [[ExtraPath]]). */
  case class ExtraPathFixed(pathElements: List[String]) extends Input

  /** Map payload to some value. */
  case class MappedPayload[T](mapping: Mapping[T], maxLength: Option[Long] = None) extends TypedInput[T]

  /**
   * Expect a binary input stream (with content type).
   * Resulting type should be something like content-type and binary data.
   */
  case object Binary extends Input
  /** Add a query parameter */
  case class AddQueryParameter(name: String) extends TypedInput[String]
  /** Parse query parameters into a circe-extended class, values are converted into strings before. */
  case class QueryParameterMap[T](mapping: PureMapping[Map[String, String], T]) extends TypedInput[T]
  /** Add an header with value. */
  case class AddHeader(name: String) extends TypedInput[String]

  /** Maps a typed input to another type B. */
  case class MappedInput[From, To, Underlying <: TypedInput[From]](original: Underlying, mapping: PureMapping[From, To]) extends TypedInput[To]

  /**
   * Multipart input
   * (Constructed by make Methods in [[Multipart$]] functions )
   */
  case class Multipart[Parts <: HList](parts: Parts) extends Input

  trait MultipartPart

  object Multipart {
    /** A Multipart text field. */
    case class MultipartText(name: String) extends MultipartPart
    /** A Multipart file part. */
    case class MultipartFile(name: String, fileName: Option[String] = None) extends MultipartPart

    // Convenience constructors for Multiparts
    def make[A <: MultipartPart](a: A) = Multipart(a :: HNil)
    def make[A <: MultipartPart, B <: MultipartPart](a: A, b: B) = Multipart(a :: b :: HNil)
    def make[A <: MultipartPart, B <: MultipartPart, C <: MultipartPart](a: A, b: B, c: C) = Multipart(a :: b :: c :: HNil)
  }

  def text(limit: Option[Long] = None): MappedPayload[String] = MappedPayload(TextMapping, limit)
  def circe[T](limit: Option[Long] = None)(implicit encoder: Encoder[T], decoder: Decoder[T]): MappedPayload[T] = MappedPayload(CirceJsonMapping[T], limit)
  def circeQuery[T](implicit encoder: ObjectEncoder[T], decoder: Decoder[T]) = QueryParameterMap(
    CirceJsonStringMapping()
  )

  def pureMapping[A, B](mapping: A => Either[String, B], contraMap: B => Either[String, A]) = new PureMapping[A, B] {
    override def encode(value: B): Either[String, A] = contraMap(value)

    override def decode(in: A): Either[String, B] = mapping(in)
  }
}
