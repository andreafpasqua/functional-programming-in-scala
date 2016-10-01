package andrea.scala.functional.programming.math

import andrea.scala.functional.programming.math.Math.fib

/**
  *
  * Created by andreapasqua on 10/01/2016.
  */
object MathTest extends App {
  println("Testing the implementation of fib")
  assert(fib(1) == 0)
  assert(fib(2) == 1)
  assert(fib(10) == 34)
}
