package net.reactivecore.fhttp.helper

/**
 * A tree which can contain either tuples, eithers or values.
 * Used for composition of argument lists.
 */
sealed trait TupleTree

object TupleTree {

  /** Single value. x should not be a TupleTree by itself. */
  case class Leaf[T](x: T) extends TupleTree

  /** A left/right pair. */
  case class Branch[L <: TupleTree, R <: TupleTree](l: L, r: R) extends TupleTree

  object Branch {
    def fromLeafs[LT, RT](left: LT, right: RT): Branch[Leaf[LT], Leaf[RT]] = Branch(Leaf(left), Leaf(right))
  }

  /** Either a left or a right value. */
  case class ContraBranch[L <: TupleTree, R <: TupleTree](v: Either[L, R]) extends TupleTree

  /** The empty tuple Tree. */
  sealed trait Empty extends TupleTree
  case object Empty extends Empty

  /** Converts tuple trees to values. */
  trait TupleConversion[T <: TupleTree] {
    type Result

    def toTuple(value: T): Result

    def fromTuple(tuple: Result): T
  }

  object TupleConversion {
    type Aux[TT <: TupleTree, Result0] = TupleConversion[TT] {
      type Result = Result0
    }

    def apply[TT <: TupleTree](implicit tc: TupleConversion[TT]): Aux[TT, tc.Result] = tc

    private def make[TT <: TupleTree, R](f: TT => R, b: R => TT): Aux[TT, R] = new TupleConversion[TT] {
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

    implicit def pair[L <: TupleTree, LR, R <: TupleTree, RR, Result](
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

    implicit def contra[L <: TupleTree, LR, R <: TupleTree, RR](
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
