package net.reactivecore.fhttp

import shapeless.HList

case class ApiHeader(
    method: String,
    path: String
)

/**
 * A Single API Call
 * @param header basic information (path, method) of this call.
 * @param input an HList of Input-Transformations. The head is the last operation to be applied.
 * @param output an HList of Output-Transformations. The head is the last operation to be applied.
 */
case class ApiCall[In <: HList, Out <: HList](
    header: ApiHeader,
    input: In,
    output: Out
)
