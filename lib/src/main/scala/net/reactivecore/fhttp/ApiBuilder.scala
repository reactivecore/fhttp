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

  protected def add[InReverse <: HList, In <: HList, OutReverse <: HList, Out <: HList](builder: ApiCallBuilder[InReverse, OutReverse])(
    implicit
    inReverse: Reverse.Aux[InReverse, In],
    outReverse: Reverse.Aux[OutReverse, Out]
  ): ApiCall[In, Out] = {
    ApiCall(
      builder.header,
      inReverse(builder.reverseInput),
      outReverse(builder.reverseOutput)
    )
  }
}
