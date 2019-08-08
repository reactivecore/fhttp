package net.reactivecore.fhttp

import shapeless.HList

case class ApiHeader(
    method: String,
    path: String
)

case class ApiCall[In <: HList, Out <: HList](
    header: ApiHeader,
    input: In,
    output: Out
)
