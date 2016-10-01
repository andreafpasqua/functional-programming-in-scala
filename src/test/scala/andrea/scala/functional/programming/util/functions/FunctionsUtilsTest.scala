package andrea.scala.functional.programming.util.functions
import andrea.scala.functional.programming.util.functions.FunctionsUtils.{curry, uncurry, compose}

/**
  * Copyright 2016, Radius Intelligence, Inc.
  * All Rights Reserved
  * Created by andreapasqua on 10/01/2016.
  */
object FunctionsUtilsTest extends App {

  println("Testing curry")
  def uncurriedFunction = (n: Int, s: String) => s + n.toString
  assert(curry(uncurriedFunction)(1)("ciao") == uncurriedFunction(1, "ciao"))
  println("Testing uncurry")
  def curriedFunction = (n: Int) => (s: String) => s + n.toString
  assert(uncurry(curriedFunction)(1, "ciao") == curriedFunction(1)("ciao"))
  println("Testing compose")
  def f = (n: Int) => n.toString
  def g = (s: String) => s.length
  assert(compose(f, g)(1) == 1)
  assert(compose(f, g)(25) == 2)

}
