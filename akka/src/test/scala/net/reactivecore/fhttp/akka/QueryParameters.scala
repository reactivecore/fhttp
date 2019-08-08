package net.reactivecore.fhttp.akka

import io.circe.generic.semiauto

case class QueryParameters(
    a: String,
    b: Option[String]
)

object QueryParameters {
  implicit val encoder = semiauto.deriveEncoder[QueryParameters]
  implicit val decoder = semiauto.deriveDecoder[QueryParameters]
}
