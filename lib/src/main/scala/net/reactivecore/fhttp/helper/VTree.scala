package net.reactivecore.fhttp.helper

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
