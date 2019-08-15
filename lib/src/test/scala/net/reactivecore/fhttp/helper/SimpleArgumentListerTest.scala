package net.reactivecore.fhttp.helper

import shapeless._

class SimpleArgumentListerTest extends TestBase {

  def makeLifter[H <: HList, T, R](f: H => R)(implicit aux: SimpleArgumentLister.Aux[H, T]): T => R = {
    i => f(aux.lift(i))
  }

  def makeUnlifter[H <: HList, T, I](f: I => H)(implicit aux: SimpleArgumentLister.Aux[H, T]): I => T = {
    i => aux.unlift(f(i))
  }

  it should "work" in {

    def acceptor(x: String :: HNil): Int = {
      x.head.length
    }

    def giver(i: Int): String :: Int :: HNil = (i.toString :: i + 1 :: HNil)

    val lifted = makeLifter(acceptor)
    lifted("Hello") shouldBe 5

    val unlifted = makeUnlifter(giver)
    unlifted(5) shouldBe (6, "5")
  }
}
