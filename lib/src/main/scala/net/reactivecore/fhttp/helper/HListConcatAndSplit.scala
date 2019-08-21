package net.reactivecore.fhttp.helper

import shapeless.{ HList, Nat }
import shapeless.ops.hlist.{ Length, Prepend, Split }

/** Helper concatenating two HLists and splitting them again. */
trait HListConcatAndSplit[Left <: HList, Right <: HList] {
  type Out <: HList

  def concat(left: Left, right: Right): Out

  def split(out: Out): (Left, Right)
}

object HListConcatAndSplit {
  type Aux[Left <: HList, Right <: HList, Out0 <: HList] = HListConcatAndSplit[Left, Right] {
    type Out = Out0
  }

  implicit def make[Left <: HList, Right <: HList, LeftLength <: Nat, Result <: HList](
    implicit
    length: Length.Aux[Left, LeftLength],
    prepend: Prepend.Aux[Left, Right, Result],
    splitter: Split.Aux[Result, LeftLength, Left, Right]
  ): Aux[Left, Right, Result] = new HListConcatAndSplit[Left, Right] {
    type Out = Result

    override def concat(left: Left, right: Right): Result = prepend(left, right)

    override def split(out: Result): (Left, Right) = {
      splitter.apply(out)
    }
  }
}