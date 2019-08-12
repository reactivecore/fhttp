package net.reactivecore.fhttp.helper

import shapeless._
import shapeless.ops.hlist.{ Reverse, Tupler }

/**
 * Type class which provides you with a function Tuple --> HList
 * if you have the HList type given.
 * For a HList of one type, only the type itself is used.
 *
 * Note: the hlist is reversed.
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

  implicit def make[H <: HList, HR <: HList, T](
    implicit
    reverse1: Reverse.Aux[H, HR],
    reverse2: Reverse.Aux[HR, H],
    tupler: Tupler.Aux[HR, T],
    generic: Generic.Aux[T, HR]
  ): Aux[H, T] = new SimpleArgumentLister[H] {
    type PlainType = T

    override def lift(t: T): H = {
      reverse2(generic.to(t))
    }

    override def unlift(t: H): T = {
      tupler.apply(reverse1(t))
    }
  }
}

