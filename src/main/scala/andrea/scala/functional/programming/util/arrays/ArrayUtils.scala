package andrea.scala.functional.programming.util.arrays

import scala.annotation.tailrec

/**
  * Created by andreapasqua on 10/01/2016.
  */
object ArrayUtils {

  /**
    * Checks if an array is sorted. Exercise 2.2
    */
  def isSorted[T](arr: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @tailrec
    def go(arr: Array[T], orderedSoFar: Boolean): Boolean =
      if (!orderedSoFar) false
      else if (arr.length < 2) true
      else go(arr.tail, ordered(arr.head, arr.tail.head))
    go(arr, orderedSoFar = true)
    }

}
