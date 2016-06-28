package andrea.scala.functional.programming.monoid

import andrea.scala.functional.programming.testing.{Prop, Sampler}

/**
  * Created by andrea on 11/16/16.
  */

trait Monoid[T] {

  def op(t1: T, t2: T): T
  def zero: T

}

object Monoid {

  /**
    * Given a monoid m of type T and a sampler s for T, it constructs
    * a Proposition to test associativity and the existence of a unit
    * Exercise 10.4
    */
  def monoidLaws[T](m: Monoid[T], p: Sampler[T]): Prop = {

    val associativity: Prop = Sampler.listOfN(p, 3).forall {
      case List(t1, t2, t3) => m.op(m.op(t1, t2), t3) == m.op(t1, m.op(t2, t3))
    }
    val zeroExistence: Prop = p.forall (
      t => m.op(t, m.zero) == t && m.op(m.zero, t) == t
      )
    associativity && zeroExistence
  }

  def stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    def zero: String = ""
  }

  def listMonoid[T] = new Monoid[List[T]] {
    def op(l1: List[T], l2: List[T]): List[T] = l1 ++ l2
    def zero: List[T] = List.empty[T]
  }

  /**
    * A monoid for integers under addition
    * Exercise 10.1
    */
  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero: Int = 0
  }

  /**
    * A monoid for integers under product
    * Exercise 10.1
    */
  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero: Int = 1
  }

  /**
    * A monoid for Booleans under or
    * Exercise 10.1
    */
  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero: Boolean = false
  }

  /**
    * A monoid for Booleans under and
    * Exercise 10.1
    */
  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero: Boolean = true
  }

  /**
    * A monoid for options
    * Exercise 10.2
    */
  def optionMonoid[T] = new Monoid[Option[T]] {
    def op(o1: Option[T], o2: Option[T]): Option[T] = o1.orElse(o2)
    def zero: Option[T] = None
  }

  /**
    * A monoid for endofunctions
    * Exercise 10.3
    */
  def endoMonoid[T] = new Monoid[T => T] {
    def op(f1: T => T, f2: T => T): T => T = (t: T) => f1(f2(t))
    def zero: T => T = (t: T) => t
  }

  /**
    * Maps the elements of the list ts to elements of a monoid m using
    * f and then folds the transformed elements.
    * Exercise 10.5
    */
  def foldMap[T, S](ts: List[T], m: Monoid[S])(f: T => S): S =
    ts.foldLeft(m.zero)((s, t) => m.op(s, f(t)))

  /**
    * Implements foldLeft as the foldMap on a certain monoid.
    * The monoid are the endofunctions of S and the foldMap returns
    * one such function. The output of the foldLeft is then obtained
    * by applying the resulting endofunction to the fold's zero
    * element.
    * Exercise 10.6
    */
  def foldLeftViaFoldMap[T, S](ts: List[T])(z: S)(g: (S, T) => S): S = {
    val m = Monoid.endoMonoid[S]
    val f = (t: T) => (s: S) => g(s, t)
    foldMap(ts.reverse, m)(f)(z)
  }

}
