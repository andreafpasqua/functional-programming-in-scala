package andrea.scala.functional.programming.math

import scala.annotation.tailrec

/**
  * Created by andreapasqua on 10/01/2016.
  */
object Math {

  // Exercise 2.1
  /**
    * Computes the nth Fibonacci number.
    * @param n an integer, with at least value 1
    * @return
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, last: Int, previous: Int): Int =
      if (n < 1) throw new IllegalArgumentException(s"fib called with parameter $n, which is less than 1")
      else if (n == 1) previous
      else if (n == 2) last
      else go(n - 1, last + previous, last)
    go(n, 1, 0)
  }

}
