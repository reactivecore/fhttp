package net.reactivecore.fhttp.helper

import net.reactivecore.fhttp.helper.TupleTree._

class TupleTreeTest extends TestBase {

  def test[TT <: TupleTree, Tuple](
    tupleTree: TT,
    tuple: Tuple
  )(implicit tupleConversion: TupleConversion.Aux[TT, Tuple], again: TupleConversion[TT]): Unit = {
    tupleConversion.toTuple(tupleTree) shouldBe tuple
    tupleConversion.fromTuple(tuple) shouldBe tupleTree
  }

  it should "work with simple trees" in {
    TupleConversion[TupleTree.Empty]
    test(Empty: Empty, ()) // TODO: Why the empty deduction?
    test(Leaf(5), 5)
    test(Branch(Leaf(3), Leaf("H")), (3, "H"))
    test(Branch(Empty, Leaf("H")), "H")
    test(Branch.fromLeafs(4, 5), (4, 5))
    test(Branch(Branch.fromLeafs(1, 2), Branch.fromLeafs(3, 4)), (1, 2, 3, 4))
    test(Branch(Branch.fromLeafs(1, 2), Leaf(3)), (1, 2, 3))
    test(Branch(Leaf(0), Branch.fromLeafs(1, 2)), (0, 1, 2))
  }

  it should "work with contra branches" in {
    TupleConversion.contra[Leaf[Int], Int, Leaf[String], String]
    TupleConversion[ContraBranch[Leaf[Int], Leaf[String]]]
    test(
      ContraBranch(Left(Leaf(1))): ContraBranch[Leaf[Int], Leaf[String]], Left(1): Either[Int, String])
  }

  it should "work with a complex example" in {

    val tree = Branch(
      Branch(
        Leaf(100), Branch(Leaf("Hello World"), Empty)
      ),
      ContraBranch(
        Left(Branch(
          Leaf(500), Branch(Leaf("Something Crashed"), Empty)
        ))
      ): ContraBranch[Branch[Leaf[Int], Branch[Leaf[String], Empty]], Leaf[String]]
    )

    val value = (100, "Hello World",
      (Left((500, "Something Crashed")): Either[(Int, String), String])
    )

    test(tree.l, (100, "Hello World"))
    test(tree.r, value._3)

    test(tree, value)
  }
}
