package andrea.scala.functional.programming.util.functions

/**
  * Copyright 2016, Radius Intelligence, Inc.
  * All Rights Reserved
  * Created by andreapasqua on 10/01/2016.
  */
object FunctionsUtils {

  /**
    * A utility to curry functions. Exercise 2.3
    *
    * @param f a function of two arguments of generic types A, B onto a generic type C
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => b: B => f(a, b)
  }

  /**
    * A utility to uncurry functions. Exercise 2.4
    */
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /**
    * A utility to compose functions. Exercise 2.5
    */
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))

}
