package net.reactivecore.fhttp.helper

/** Fake type class to check if a value is not a tuple. */
trait IsNotTuple[T] {

}

object IsNotTuple {
  implicit def regularTypes[T]: IsNotTuple[T] = null
  // For Tuples this will result in a ambiguous value
  // For the scala compiler also Shapeless' IsTuple worked, however IntelliJ bails out.

  /*
  Dummy generation script.sc

  for (i <- 2 to 22) {
    val values = for (j <- 0 until i) yield {
      s"T${j}"
    }

    val templateList = values.mkString("[",",","]")
    val tupleName = values.mkString("(", ",", ")")

    val badMethod = s"$templateList: IsNotTuple[$tupleName] = null"
    println(s"implicit def boomTuple${i}_a$badMethod")
    println(s"implicit def boomTuple${i}_b$badMethod")

  }
   */

  implicit def boomTuple1_a[T0]: IsNotTuple[Tuple1[T0]] = null
  implicit def boomTuple1_b[T0]: IsNotTuple[Tuple1[T0]] = null

  implicit def boomTuple2_a[T0, T1]: IsNotTuple[(T0, T1)] = null
  implicit def boomTuple2_b[T0, T1]: IsNotTuple[(T0, T1)] = null
  implicit def boomTuple3_a[T0, T1, T2]: IsNotTuple[(T0, T1, T2)] = null
  implicit def boomTuple3_b[T0, T1, T2]: IsNotTuple[(T0, T1, T2)] = null
  implicit def boomTuple4_a[T0, T1, T2, T3]: IsNotTuple[(T0, T1, T2, T3)] = null
  implicit def boomTuple4_b[T0, T1, T2, T3]: IsNotTuple[(T0, T1, T2, T3)] = null
  implicit def boomTuple5_a[T0, T1, T2, T3, T4]: IsNotTuple[(T0, T1, T2, T3, T4)] = null
  implicit def boomTuple5_b[T0, T1, T2, T3, T4]: IsNotTuple[(T0, T1, T2, T3, T4)] = null
  implicit def boomTuple6_a[T0, T1, T2, T3, T4, T5]: IsNotTuple[(T0, T1, T2, T3, T4, T5)] = null
  implicit def boomTuple6_b[T0, T1, T2, T3, T4, T5]: IsNotTuple[(T0, T1, T2, T3, T4, T5)] = null
  implicit def boomTuple7_a[T0, T1, T2, T3, T4, T5, T6]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6)] = null
  implicit def boomTuple7_b[T0, T1, T2, T3, T4, T5, T6]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6)] = null
  implicit def boomTuple8_a[T0, T1, T2, T3, T4, T5, T6, T7]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7)] = null
  implicit def boomTuple8_b[T0, T1, T2, T3, T4, T5, T6, T7]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7)] = null
  implicit def boomTuple9_a[T0, T1, T2, T3, T4, T5, T6, T7, T8]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8)] = null
  implicit def boomTuple9_b[T0, T1, T2, T3, T4, T5, T6, T7, T8]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8)] = null
  implicit def boomTuple10_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] = null
  implicit def boomTuple10_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] = null
  implicit def boomTuple11_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = null
  implicit def boomTuple11_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = null
  implicit def boomTuple12_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = null
  implicit def boomTuple12_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = null
  implicit def boomTuple13_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = null
  implicit def boomTuple13_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = null
  implicit def boomTuple14_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = null
  implicit def boomTuple14_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = null
  implicit def boomTuple15_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = null
  implicit def boomTuple15_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = null
  implicit def boomTuple16_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = null
  implicit def boomTuple16_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = null
  implicit def boomTuple17_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = null
  implicit def boomTuple17_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = null
  implicit def boomTuple18_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = null
  implicit def boomTuple18_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = null
  implicit def boomTuple19_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = null
  implicit def boomTuple19_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = null
  implicit def boomTuple20_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = null
  implicit def boomTuple20_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = null
  implicit def boomTuple21_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = null
  implicit def boomTuple21_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = null
  implicit def boomTuple22_a[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = null
  implicit def boomTuple22_b[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]: IsNotTuple[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = null
}
