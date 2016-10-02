package andrea.scala.functional.programming.list

import scala.annotation.tailrec

/**
  * Created by andreapasqua on 10/01/2016.
  */
sealed trait List[+A] {

  /**
    * sums the elements of the list using the implicit zeros and plus methods of Numeric[B]
    */
  def sum[B >: A](implicit num: Numeric[B]): B = foldRight(num.zero)(num.plus)

  /**
    * sums the elements of the list using the implicit zeros and plus methods of Numeric[B]
    */
  def product[B >: A](implicit num: Numeric[B]): B = foldRight(num.one)(num.times)

  /**
    * Returns the length of a list
    * Exercise 3.9
    */
  def length: Int = foldRight(0)((a: A, count: Int) => count + 1)

  /**
    * Returns true if the list is Nil, else returns false
    */
  def isEmpty: Boolean = this match {
    case Nil => true
    case Cons(_, _) => false
  }

  /**
    * extracts the tail of a list. Raises an exception if the list is empty
    * Exercise 3.2
    */
  def tail: List[A] = this match {
    case Nil => throw new NoSuchMethodException("Tail called on an empty list")
    case Cons(head, tail) => tail
  }

  /**
    * Drops the first n elements of a list. if n is greater than the length of the list returns
    * an empty list
    * Exercise 3.4
    */
  def drop(n: Int): List[A] =
    if ((n == 0) || this.isEmpty) this
    else this.tail.drop(n - 1)

  /**
    * Drops elements from the head of a list until the predicate p is violated
    * Exercise 3.5
    */
  def dropWhile(p: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) if p(h) => t.dropWhile(p)
    case _ => this
  }

  /**
    * returns a new list with the head replaced by the argument a.
    * Exercise 3.3
    */
  def setHead[B >: A](b: B): List[B] = this match {
    case Nil => List(b)
    case Cons(head, tail) => Cons(b, tail)
  }

  /**
    * Returns a list consisting of the current list followed by other
    */
  def append[B >: A](other: List[B]): List[B] = this match {
    case Nil => other
    case Cons(h, t) => Cons(h, t.append(other))
  }

  /**
    * Drops the last element of a list. If the list is empty, it remains empty
    * Exercise 3.6
    */
  def init: List[A] = this match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, t.init)
  }

  /**
    * Implements a non tail recursive foldRight, which given a List(a_1, ... , a_n) returns
    * f(f(...(f(a_n-1, f(a_n, z)))...))
    * @param z the zero element
    * @param f the folding function
    */
  final def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(h, t) => f(h, t.foldRight(z)(f))
  }

  /**
    * Implements a tail recursive foldLeft, which given a List(a_1, ... , a_n) returns
    * f(f(...f(f(z, a_1), a_2), ..., a_n))
    * Exercise 3.10
    * @param z the zero element
    * @param f the folding function
    */
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
  }

  /**************** OLDER VERSIONS OF ROUTINES
    *
    */
  def sumNoFold[B >: A](implicit num: Numeric[B]): B =
    this match {
      case Nil => num.zero
      case Cons(head, tail) => num.plus(head, tail.sumNoFold(num))
    }

  def productNoFold[B >: A](implicit num: Numeric[B]): B =
    this match {
      case Nil => num.one
      case Cons(x, _) if x == num.zero => num.zero
      case Cons(head, tail) => num.times(head, tail.productNoFold(num))
    }
}

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fill[A](n: Int, a: A): List[A] =
    if (n == 0) Nil
    else Cons(a, fill(n - 1, a))

}