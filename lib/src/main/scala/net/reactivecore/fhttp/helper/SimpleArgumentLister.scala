package net.reactivecore.fhttp.helper

import shapeless._
import shapeless.ops.hlist.Tupler

/**
 * Type class which provides you with a function Tuple --> HList
 * if you have the HList type given.
 * For a HList of one type, only the type itself is used.
 */
trait SimpleArgumentLister[H <: HList] {
  type PlainType

  def lift(t: PlainType): H

  def unlift(t: H): PlainType
}

object SimpleArgumentLister {
  type Aux[H <: HList, P0] = SimpleArgumentLister[H] {
    type PlainType = P0
  }

  def apply[H <: HList](implicit h: SimpleArgumentLister[H]): Aux[H, h.PlainType] = h

  implicit def single[X]: Aux[X :: HNil, X] = new SimpleArgumentLister[X :: HNil] {
    type PlainType = X

    override def lift(t: X): X :: HNil = t :: HNil

    override def unlift(t: X :: HNil): X = t.head
  }

  implicit def make[H <: HList, T](
    implicit
    tupler: Tupler.Aux[H, T],
    generic: Generic.Aux[T, H]
  ): Aux[H, T] = new SimpleArgumentLister[H] {
    type PlainType = T

    override def lift(t: T): H = generic.to(t)

    override def unlift(t: H): T = tupler.apply(t)
  }
}

