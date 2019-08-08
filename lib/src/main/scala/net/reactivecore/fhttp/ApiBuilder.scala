package net.reactivecore.fhttp

import shapeless._
import shapeless.ops.hlist.Tupler

/** A Builder for APIs. */
trait ApiBuilder {

  protected def post(path: String): ApiCallBuilder[HNil, HNil] = {
    empty(
      ApiHeader("POST", path)
    )
  }

  protected def get(path: String): ApiCallBuilder[HNil, HNil] = {
    empty(
      ApiHeader("GET", path)
    )
  }

  private def empty(header: ApiHeader): ApiCallBuilder[HNil, HNil] = {
    ApiCallBuilder(ApiCall(header, HNil, HNil))
  }

  protected def add[In <: HList: Tupler, Out <: HList](builder: ApiCallBuilder[In, Out]) = {
    builder.call
  }
}
