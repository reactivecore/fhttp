package net.reactivecore.fhttp

import shapeless._

case class ApiCallBuilder[In <: HList, Out <: HList](
    header: ApiHeader,
    reverseInput: In,
    reverseOutput: Out
) {

  def expecting[T <: Input](
    input: T
  ) = addInputStep(input)

  def responding[T <: Output](
    output: T
  ) = addOutputStep(output)

  private def addInputStep[U](step: U) = {
    copy(
      reverseInput = step :: reverseInput
    )
  }

  private def addOutputStep[U](step: U) = {
    copy(
      reverseOutput = step :: reverseOutput
    )
  }
}
