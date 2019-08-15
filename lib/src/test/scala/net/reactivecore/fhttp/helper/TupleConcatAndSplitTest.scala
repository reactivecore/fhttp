package net.reactivecore.fhttp.helper

class TupleConcatAndSplitTest extends TestBase {

  def test[L,R,Result](l: L, r: R, result: Result)(implicit t: TupleConcatAndSplit.Aux[L, R, Result], alternative: TupleConcatAndSplit[L, R]): Unit = {
    t.concat(l, r) shouldBe result
    t.split(result) shouldBe (l,r)
  }

  it should "work" in {
    test((), (), ())
    test((), ("a", 2), ("a", 2))
    test(("a", 2), (), ("a", 2))
    test(("a", 2), (4.0, "b"), ("a", 2, 4.0, "b"))

    test(("a"), (), ("a"))
    test((), ("b"), ("b"))
    TupleConcatAndSplit.prefixValue[Int, (String, Int), (Int, String, Int)]
    test("a", ("b", 3), ("a", "b", 3))
    test(("a", "b"), 3, ("a", "b", 3))
  }
}
