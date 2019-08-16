package net.reactivecore.fhttp.helper

import net.reactivecore.fhttp.helper.VTree._
import shapeless._

class VTreeTest extends TestBase {

  def testTupleConversion[V <: VTree, Tuple](
    vTree: V,
    tuple: Tuple
  )(implicit tupleConversion: TupleConversion.Aux[V, Tuple], again: TupleConversion[V]): Unit = {
    tupleConversion.toTuple(vTree) shouldBe tuple
    tupleConversion.fromTuple(tuple) shouldBe vTree
  }

  def testHListConversion[V <: VTree, H <: HList](
    vTree: V,
    hList: H
  )(implicit hlistConversion: HListConversion.Aux[V, H], again: HListConversion[V]): Unit = {
    hlistConversion.toHList(vTree) shouldBe hList
    hlistConversion.fromHList(hList) shouldBe vTree
  }

  def testBoth[V <: VTree, H <: HList, T](
    vTree: V,
    hList: H,
    tuple: T
  )(
    implicit tupleConversion: TupleConversion.Aux[V, T],
    hListConversion: HListConversion.Aux[V, H]
  ): Unit = {
    testTupleConversion(vTree, tuple)
    testHListConversion(vTree, hList)
  }

  it should "work with simple trees" in {
    TupleConversion[VTree.Empty]
    testBoth(Empty: Empty, HNil: HNil, ()) // TODO: Why the empty deduction?
    testBoth(Leaf(5), 5 :: HNil, 5)
    testBoth(Branch(Leaf(3), Leaf("H")), 3 :: "H" :: HNil, (3, "H"))
    testBoth(Branch(Empty, Leaf("H")),
      "H" :: HNil,
      "H"
    )
    testBoth(Branch.fromLeafs(4, 5),
      4 :: 5 :: HNil,
      (4, 5))
    testBoth(Branch(Branch.fromLeafs(1, 2), Branch.fromLeafs(3, 4)),
      1 :: 2 :: 3 :: 4 :: HNil,
      (1, 2, 3, 4))
    testBoth(Branch(Branch.fromLeafs(1, 2), Leaf(3)),
      1 :: 2 :: 3 :: HNil,
      (1, 2, 3))
    testBoth(Branch(Leaf(0), Branch.fromLeafs(1, 2)),
      0 :: 1 :: 2 :: HNil,
      (0, 1, 2))
  }

  it should "work with contra branches" in {
    TupleConversion.contra[Leaf[Int], Int, Leaf[String], String]
    TupleConversion[ContraBranch[Leaf[Int], Leaf[String]]]
    testBoth(
      ContraBranch(Left(Leaf(1))): ContraBranch[Leaf[Int], Leaf[String]],
      (Left(1 :: HNil) :: HNil) : Either[Int :: HNil, String :: HNil] :: HNil,
      Left(1): Either[Int, String])
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

    testTupleConversion(tree.l, (100, "Hello World"))
    testTupleConversion(tree.r, value._3)

    testTupleConversion(tree, value)
  }
}
