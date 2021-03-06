package andrea.scala.functional.programming.math

import andrea.scala.functional.programming.math.Math._
import andrea.scala.functional.programming.option.{None, Some}

/**
  *
  * Created by andreapasqua on 10/01/2016.
  */
object MathTest extends App {
  println("Testing getFib")
  assert(getFib(1) == 0)
  assert(getFib(2) == 1)
  assert(getFib(10) == 34)

  println("Testing mean and variance")
  val emptyVector: Vector[Double] = Vector()
  val vectorOne = Vector(1D)
  val vectorThree = Vector(1D, 2D, 3D)
  assert(mean(emptyVector) == None)
  assert(mean(vectorOne) == Some(1D))
  assert(mean(vectorThree) == Some(2D))
  assert(variance(emptyVector) == None)
  assert(variance(vectorOne) == Some(0D))
  assert(variance(vectorThree) == Some(2D / 3D))

}
