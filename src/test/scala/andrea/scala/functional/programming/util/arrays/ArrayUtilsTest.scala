package andrea.scala.functional.programming.util.arrays

import ArrayUtils.isSorted
/**
  * Created by andreapasqua on 10/01/2016.
  */
object ArrayUtilsTest extends App {

  println("Test isSorted")
  val arrEmpty = Array()
  val arrOnlyOne = Array(1)
  val arrOrdered = Array(1, 2, 3, 4, 5)
  val arrUnordered = Array(2, 1, 3, 4, 5)
  val ordered = (n: Int, m: Int) => n <= m
  assert(isSorted(arrEmpty, ordered))
  assert(isSorted(arrOnlyOne, ordered))
  assert(isSorted(arrOrdered, ordered))
  assert(!isSorted(arrUnordered, ordered))
}
