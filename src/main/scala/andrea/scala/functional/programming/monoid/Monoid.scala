package andrea.scala.functional.programming.monoid

import andrea.scala.functional.programming.testing.{Prop, Sampler}

/**
  * Created by andreapasqua on 11/07/2016.
  */

trait Monoid[T] {

  /**
    * An associative operation
    */
  def op(t1: T, t2: T): T

  /**
    * A neutral element with respect to op
    */
  def zero: T

}

object Monoid {

  object MonoidLaws {

    /**
      * A proposition satisfied by the op operation of a monoid. It expresses associativity.
      * Exercise 10.4
      */
    def opLaw[T](tSampler: Sampler[T])(m: Monoid[T]): Prop = {

      val ts = for {
        t1 <- tSampler
        t2 <- tSampler
        t3 <- tSampler
      } yield (t1, t2, t3)

      ts.forall {
        case (t1, t2, t3) => m.op(m.op(t1, t2), t3) == m.op(t1, m.op(t2, t3))
      }

    }

    def zeroLaw[T](tSampler: Sampler[T])(m: Monoid[T]): Prop =
      tSampler.forall (
        t => m.op(m.zero, t) == t && m.op(t, m.zero) == t
      )

  }

  object MonoidExamples {

    val stringMonoid = new Monoid[String] {
      def op(s1: String, s2: String): String = s1 + s2
      def zero: String = ""
    }

    /**
      * Exercise 10.1
      */
    val intAddition = new Monoid[Int] {
      def op(i1: Int, i2: Int): Int = i1 + i2
      def zero: Int = 0
    }

    /**
      * Exercise 10.1
      */
    val intMultiplication = new Monoid[Int] {
      def op(i1: Int, i2: Int): Int = i1 * i2
      def zero: Int = 1
    }

    def listMonoid[T] = new Monoid[List[T]] {
      def op(l1: List[T], l2: List[T]): List[T] = l1 ++ l2
      def zero: List[T] = Nil
    }

    /**
      * Exercise 10.1
      */
    val booleanOr = new Monoid[Boolean] {
      def op(b1: Boolean, b2: Boolean): Boolean = b1 | b2
      def zero: Boolean = false
    }

    /**
      * Exercise 10.1
      */
    val booleanAnd = new Monoid[Boolean] {
      def op(b1: Boolean, b2: Boolean): Boolean = b1 & b2
      def zero: Boolean = true
    }

    /**
      * Exercise 10.2
      */
    def optionMonoid[T] = new Monoid[Option[T]] {
      def op(op1: Option[T], op2: Option[T]): Option[T] = op1.orElse(op2)
      def zero: Option[T] = None
    }

    /**
      * Exercise 10.3
      */
    def endoMonoid[T] = new Monoid[(T) => T] {
      def op(f1: (T) => T, f2: (T) => T): (T) => T = (t: T) => f1(f2(t))
      def zero: (T) => T = (t: T) => t
    }
  }

}