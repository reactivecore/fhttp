package net.reactivecore.fhttp

import shapeless._
import shapeless.ops.hlist.{ Reverse, Tupler }

/** A Builder for APIs. */
trait ApiBuilder {

  protected def post(path: String*): ApiCallBuilder[HNil, HNil] = {
    empty(
      ApiHeader("POST", path.toList)
    )
  }

  protected def get(path: String*): ApiCallBuilder[HNil, HNil] = {
    empty(
      ApiHeader("GET", path.toList)
    )
  }

  protected def delete(path: String*): ApiCallBuilder[HNil, HNil] = {
    empty(
      ApiHeader("DELETE", path.toList)
    )
  }

  // Shortcut to input
  protected def input: Input.type = Input

  // Shortcut to input
  protected def output: Output.type = Output

  private def empty(header: ApiHeader): ApiCallBuilder[HNil, HNil] = {
    ApiCallBuilder(header, HNil, HNil)
  }

  protected def add[In <: HList: Reverse: Tupler, Out <: HList: Reverse](builder: ApiCallBuilder[In, Out]) = {
    ApiCall(
      builder.header,
      builder.reverseInput.reverse,
      builder.reverseOutput.reverse
    )
  }
}
