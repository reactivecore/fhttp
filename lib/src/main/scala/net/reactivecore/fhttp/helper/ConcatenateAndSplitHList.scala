package net.reactivecore.fhttp.helper

import shapeless.{ HList, Nat }
import shapeless.ops.hlist.{ Length, Prepend, Split }

/** Helper concatenating two HLists and splitting them again. */
trait ConcatenateAndSplitHList[Left <: HList, Right <: HList] {
  type Out <: HList

  def concatenate(left: Left, right: Right): Out

  def split(out: Out): (Left, Right)
}

object ConcatenateAndSplitHList {
  type Aux[Left <: HList, Right <: HList, Out0 <: HList] = ConcatenateAndSplitHList[Left, Right] {
    type Out = Out0
  }

  implicit def make[Left <: HList, Right <: HList, LeftLength <: Nat, Result <: HList](
    implicit
    length: Length.Aux[Left, LeftLength],
    prepend: Prepend.Aux[Left, Right, Result],
    splitter: Split.Aux[Result, LeftLength, Left, Right]
  ): Aux[Left, Right, Result] = new ConcatenateAndSplitHList[Left, Right] {
    type Out = Result

    override def concatenate(left: Left, right: Right): Result = prepend(left, right)

    override def split(out: Result): (Left, Right) = {
      splitter.apply(out)
    }
  }
}