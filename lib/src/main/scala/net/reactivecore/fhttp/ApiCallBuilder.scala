package net.reactivecore.fhttp

import shapeless._

case class ApiCallBuilder[In <: HList, Out <: HList](
    call: ApiCall[In, Out]
) {

  def expecting[T <: Input](
    input: T
  ) = addInputStep(input)

  def responding[T <: Output](
    output: T
  ) = addOutputStep(output)

  private def addInputStep[U](step: U) = {
    copy(
      call = call.copy(input = step :: call.input)
    )
  }

  private def addOutputStep[U](step: U) = {
    copy(
      call = call.copy(output = step :: call.output)
    )
  }
}
