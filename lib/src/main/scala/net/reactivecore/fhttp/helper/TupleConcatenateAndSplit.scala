package net.reactivecore.fhttp.helper

import net.reactivecore.fhttp.helper.TupleConcatAndSplit.make
import shapeless._
import shapeless.ops.tuple.{ Length, Prepend, Split }

/** Simple Type class allowing concatenation and splitting of tuples. */
trait TupleConcatAndSplit[L, R] {
  type Result

  def concat(left: L, right: R): Result

  def split(result: Result): (L, R)
}

trait TupleConcatenateAndSplitLowPriority {
  implicit def prefixValue[L: IsNotTuple, R, Result](
    implicit
    prepend: Prepend.Aux[Tuple1[L], R, Result],
    splitter: Split.Aux[Result, Nat._1, (Tuple1[L], R)]
  ) = make[L, R, Result](
    (l, r) => prepend(Tuple1(l), r),
    v => {
      val (l, r) = splitter(v)
      l._1 -> r
    }
  )

  implicit def suffixValue[L, R: IsNotTuple, N <: Nat, Result](
    implicit
    prepend: Prepend.Aux[L, Tuple1[R], Result],
    length: Length.Aux[L, N],
    splitter: Split.Aux[Result, N, (L, Tuple1[R])]
  ) = make[L, R, Result](
    (l, r) => prepend(l, Tuple1(r)),
    v => {
      val (l, r) = splitter(v)
      l -> r._1
    }
  )

  implicit def concatValue[L: IsNotTuple, R: IsNotTuple] = make[L, R, (L, R)](
    (l, r) => (l, r),
    v => v
  )
}

trait TupleConcatenateAndSplitHighPriority extends TupleConcatenateAndSplitLowPriority {

  implicit val doubleUnit = make[Unit, Unit, Unit](
    (_, _) => (),
    _ => ((), ())
  )

  implicit def suffixUnit[L] = make[L, Unit, L](
    (l, _) => l,
    l => (l, ())
  )

  implicit def prefixUnit[R] = make[Unit, R, R](
    (_, r) => r,
    r => ((), r)
  )

  implicit def generic[L: IsTuple, R: IsTuple, Result, N <: Nat](
    implicit
    prepend: Prepend.Aux[L, R, Result],
    length: Length.Aux[L, N],
    splitter: Split.Aux[Result, N, (L, R)]
  ) = make[L, R, Result](
    (l, r) => prepend(l, r),
    result => {
      val (l, r) = splitter(result)
      (l, r)
    }
  )
}

object TupleConcatAndSplit extends TupleConcatenateAndSplitHighPriority {
  type Aux[L, R, Result0] = TupleConcatAndSplit[L, R] {
    type Result = Result0
  }

  def apply[L, R](implicit t: TupleConcatAndSplit[L, R]): Aux[L, R, t.Result] = t

  def make[L, R, Result0](c: (L, R) => Result0, s: Result0 => (L, R)): Aux[L, R, Result0] = new TupleConcatAndSplit[L, R] {
    override type Result = Result0

    override def concat(left: L, right: R): Result0 = c(left, right)

    override def split(result: Result0): (L, R) = s(result)
  }
}
