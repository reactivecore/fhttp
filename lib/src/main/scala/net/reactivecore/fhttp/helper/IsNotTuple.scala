package net.reactivecore.fhttp.helper

import shapeless.IsTuple

/** Fake type class to check if a value is not a tuple. */
trait IsNotTuple[T] {

}

object IsNotTuple {
  implicit def regularTypes[T]: IsNotTuple[T] = null
  // For Tuples this will result in a ambiguous value
  implicit def tuples[T: IsTuple]: IsNotTuple[T] = null
}
