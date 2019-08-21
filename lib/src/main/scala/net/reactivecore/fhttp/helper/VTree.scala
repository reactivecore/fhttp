package net.reactivecore.fhttp.helper

import shapeless._
import shapeless.ops.hlist.Tupler

/**
 * A tree which can contain either tuples ([[VTree.Branch]]), Eithers ([[VTree.ContraBranch]]) or values ([[VTree.Leaf]]).
 *
 * For constructability it can also contain empty values ([[VTree.Empty]])
 *
 * Used for composition of argument lists.
 */
sealed trait VTree

object VTree {

  /** Single value. x should not be a TupleTree by itself. */
  case class Leaf[T](x: T) extends VTree

  /** A left/right pair. */
  case class Branch[L <: VTree, R <: VTree](l: L, r: R) extends VTree

  object Branch {
    def fromLeafs[LT, RT](left: LT, right: RT): Branch[Leaf[LT], Leaf[RT]] = Branch(Leaf(left), Leaf(right))
  }

  type LeafBranch[LT, RT] = Branch[Leaf[LT], Leaf[RT]]

  /** Either a left or a right value. */
  case class ContraBranch[L <: VTree, R <: VTree](v: Either[L, R]) extends VTree

  /** The empty tuple Tree. */
  sealed trait Empty extends VTree
  case object Empty extends Empty

  /** Converts VTree to flat HLists. */
  trait HListConversion[T <: VTree] {
    type Result <: HList

    def toHList(value: T): Result

    def fromHList(result: Result): T
  }

  object HListConversion {
    type Aux[TT <: VTree, Result0] = HListConversion[TT] {
      type Result = Result0
    }

    private def make[V <: VTree, R <: HList](f: V => R, b: R => V): Aux[V, R] = new HListConversion[V] {
      override type Result = R

      override def toHList(value: V): R = f(value)

      override def fromHList(tuple: R): V = b(tuple)
    }

    implicit val empty: Aux[Empty, HNil] = make[Empty, HNil](
      { _ => HNil },
      { _ => Empty }
    )

    // TODO: Why is this method necessary?
    implicit val empty2: Aux[Empty.type, HNil] = make[Empty.type, HNil](
      { _ => HNil },
      { _ => Empty }
    )

    implicit def leaf[T] = make[Leaf[T], T :: HNil](
      v => v.x :: HNil,
      v => Leaf(v.head)
    )

    implicit def pair[L <: VTree, LR <: HList, R <: VTree, RR <: HList, Result <: HList](
      implicit
      lc: HListConversion.Aux[L, LR],
      rc: HListConversion.Aux[R, RR],
      concatAndSplit: HListConcatAndSplit.Aux[LR, RR, Result]
    ) = make[Branch[L, R], Result](
      v => concatAndSplit.concat(lc.toHList(v.l), rc.toHList(v.r)),
      v => {
        val (l, r) = concatAndSplit.split(v)
        Branch(lc.fromHList(l), rc.fromHList(r))
      }
    )

    implicit def contra[L <: VTree, LR, R <: VTree, RR](
      implicit
      lc: HListConversion.Aux[L, LR],
      rc: HListConversion.Aux[R, RR]
    ) = make[ContraBranch[L, R], Either[LR, RR] :: HNil](
      v => {
        v.v match {
          case Left(left)   => Left(lc.toHList(left)) :: HNil
          case Right(right) => Right(rc.toHList(right)) :: HNil
        }
      },
      v => v.head match {
        case Left(left) =>
          ContraBranch(Left(lc.fromHList(left)))
        case Right(right) =>
          ContraBranch(Right(rc.fromHList(right)))
      }
    )
  }

  /** Converts tuple trees to values. */
  trait TupleConversion[T <: VTree] {
    type Result

    def toTuple(value: T): Result

    def fromTuple(tuple: Result): T
  }

  object TupleConversion {
    type Aux[TT <: VTree, Result0] = TupleConversion[TT] {
      type Result = Result0
    }

    def apply[TT <: VTree](implicit tc: TupleConversion[TT]): Aux[TT, tc.Result] = tc

    private def make[TT <: VTree, R](f: TT => R, b: R => TT): Aux[TT, R] = new TupleConversion[TT] {
      override type Result = R

      override def toTuple(value: TT): R = f(value)

      override def fromTuple(tuple: R): TT = b(tuple)
    }

    // TODO: It would be great if tuple conversion is based upon HListConversion (as it internally converts quite a lot)
    // However this is tricky because of embedded either types.

    implicit val empty: Aux[Empty, Unit] = make[Empty, Unit](
      { _ => () },
      { _ => Empty }
    )

    // TODO: Why is this method necessary?
    implicit val empty2: Aux[Empty.type, Unit] = make[Empty.type, Unit](
      { _ => () },
      { _ => Empty }
    )

    implicit def leaf[T] = make[Leaf[T], T](
      v => v.x,
      v => Leaf(v)
    )

    // Shortcut
    implicit def valuePair[L, R] = make[Branch[Leaf[L], Leaf[R]], (L, R)](
      v => (v.l.x, v.r.x),
      v => Branch.fromLeafs(v._1, v._2)
    )

    implicit def pair[L <: VTree, LR, R <: VTree, RR, Result](
      implicit
      lc: TupleConversion.Aux[L, LR],
      rc: TupleConversion.Aux[R, RR],
      concatAndSplit: TupleConcatAndSplit.Aux[LR, RR, Result]
    ) = make[Branch[L, R], Result](
      v => concatAndSplit.concat(lc.toTuple(v.l), rc.toTuple(v.r)),
      v => {
        val (l, r) = concatAndSplit.split(v)
        Branch(lc.fromTuple(l), rc.fromTuple(r))
      }
    )

    implicit def contra[L <: VTree, LR, R <: VTree, RR](
      implicit
      lc: TupleConversion.Aux[L, LR],
      rc: TupleConversion.Aux[R, RR]
    ) = make[ContraBranch[L, R], Either[LR, RR]](
      v => {
        v.v match {
          case Left(left)   => Left(lc.toTuple(left))
          case Right(right) => Right(rc.toTuple(right))
        }
      },
      {
        case Left(left) =>
          ContraBranch(Left(lc.fromTuple(left)))
        case Right(right) =>
          ContraBranch(Right(rc.fromTuple(right)))
      }
    )
  }
}
