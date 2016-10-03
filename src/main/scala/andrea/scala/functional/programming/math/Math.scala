package andrea.scala.functional.programming.math

import scala.annotation.tailrec
import andrea.scala.functional.programming.option.{Option, None, Some}

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

  /**
    * Computes the mean of a sequence. If the sequence is empty returns None
    * Exercise 4.2
    */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
    * Computes the mean of a sequence. If the sequence is empty returns None
    * Exercise 4.2
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(mu => mean(xs.map(x => math.pow(x - mu, 2))))

}
