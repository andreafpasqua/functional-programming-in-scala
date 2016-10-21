package andrea.scala.functional.programming.list

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by andreapasqua on 10/01/2016.
  */
sealed trait List[+A] {

  /**
    * A right associative version of Cons
    */
  def ::[B >: A](b: B) = Cons(b, this)

  /**
    * Converts a list into a Vector
    */
  def toSeq: Vector[A] = foldLeft(Vector[A]())((vec, a) => vec :+ a)

  /**
    * An equality method for lists. Should be tail-recursive.
    * Notice that overriding the equals method is tricky because it
    * is used in pattern matching so you will encounter infinite loops
    * if the equals uses pattern matching itself. The solution was to
    * define an isEmpty method to decide equality of Nil and make sure
    * that isEmpty is defined at the level of Nil and Cons, not at the
    * level of List
    */
  override def equals(other: Any): Boolean = other match {
    case l: List[_] => toSeq == l.toSeq
    case _ => false
  }

  /**
    * sums the elements of the list using the implicit zeros and plus methods of Numeric[B]
    */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /**
    * sums the elements of the list using the implicit zeros and plus methods of Numeric[B]
    */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /**
    * Returns the length of a list
    * Exercise 3.11
    */
  def length: Int = foldLeft(0)((len: Int, a: A) => len + 1)

  /**
    * Returns a list with the members in reverse order. This gives a stack-overflow
    * for large lists
    * Exercise 3.12
    */
  def reverse: List[A] = foldLeft(Nil: List[A])((list, a: A) => Cons(a, list))

  /**
    * Returns true if the list is Nil, else returns false
    */
  def isEmpty: Boolean

  /**
    * Returns the head of a list. Throws an exception if the list is empty
    */
  def head: A = this match {
    case Nil => throw new NoSuchMethodException("Tail called on an empty list")
    case Cons(head, tail) => head
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
  @tailrec
  final def drop(n: Int): List[A] =
    if ((n == 0) || isEmpty) this
    else tail.drop(n - 1)

  /**
    * Drops elements from the head of a list until the predicate p is violated
    * Exercise 3.5
    */
  @tailrec
  final def dropWhile(p: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(h, t) if p(h) => t.dropWhile(p)
    case _ => this
  }

  /**
    * Returns the list with the first n elements of the original list, or the same
    * list if it has length less than n.
    */
  def take(n: Int): List[A] = foldLeft((Nil: List[A], n)) {
    case ((soFar, i), a)  if i > 0 => (Cons(a, soFar), i - 1)
    case ((soFar, i), _) => (soFar, 0)
  }._1.reverse

  /**
    * Returns all the elements of the first list up until the first that fails the predicate
    * p.
    */
  def takeWhile(p: A => Boolean): List[A] = foldLeft((Nil: List[A], true)) {
    case ((soFar, true), a)  if p(a) => (Cons(a, soFar), true)
    case ((soFar, _), _) => (soFar, false)
  }._1.reverse

  /**
    * Returns true if all the elements of the list satisfy the predicate,
    * false otherwise.
    * A tail recursive implementation but it has no stopping conditions.
    *
    * @return
    */
  def forall(p: A => Boolean): Boolean =
    foldLeft(true)(_ && p(_))

  /**
    * Returns true if at list one of the elements of the list satisfy the predicate,
    * false otherwise
    * A tail recursive implementation but it has no stopping conditions.
    *
    * @return
    */
  def exists(p: A => Boolean): Boolean =
    foldLeft(false)(_ || p(_))

  /**
    * Returns a list of partial results for the foldLeft with the same zero z and the same
    * function f. Note that the list starts with z.
    */
  def scanLeft[B](z: B)(f: (B, A) => B): List[B] = foldLeft((List(z), z)) {
    case ((l, last), a) =>
      val curr = f(last, a)
      (Cons(curr, l), curr)
  }._1.reverse

  /**
    * Returns a list of partial results for the foldRight with the same zero z and the same
    * function f. Note that the list starts with z.
    */
  def scanRight[B](z: B)(f: (A, B) => B): List[B] =
    foldRight((List(z), z)) {
      case (a, (l, last)) =>
        val curr = f(a, last)
        (Cons(curr, l), curr)
    }._1
        /**
    * returns a new list with the head replaced by the argument a.
    * Exercise 3.3
    */
  def setHead[B >: A](b: B): List[B] = this match {
    case Nil => List(b)
    case Cons(head, tail) => Cons(b, tail)
  }

  /**
    * Returns a list consisting of the current list followed by other.
    * Exercise 3.14
    */
  def append[B >: A](other: List[B]): List[B] = foldRight(other)(Cons(_, _))

  /**
    * Drops the last element of a list. If the list is empty, it remains empty
    */
  def init: List[A] = reverse.drop(1).reverse

  /**
    * Implements a tail recursive foldLeft, which given a List(a_1, ... , a_n) returns
    * f(f(...f(f(z, a_1), a_2), ..., a_n))
    * Exercise 3.10
    *
    * @param z the zero element
    * @param f the folding function
    */
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Nil => z
    case Cons(h, t) => t.foldLeft(f(z, h))(f)
  }

  /**
    * Implements a non tail recursive foldRight, which given a List(a_1, ... , a_n) returns
    * f(a1, f(a2, ...(f(a_n-1, f(a_n, z)))...))
    *
    * @param z the zero element
    * @param f the folding function
    */
  def foldRight[B](z: B)(f: (A, B) => B): B = reverse.foldLeft(z)((b, a) => f(a, b))

  /**
    * Returns a list where the elements of the old list are transformed by applying the function f.
    * Stack over-flow on long list since it relies on foldRight
    * Exercise 3.16, 3.17, 3.18
    */
  def map[B](f: (A) => B): List[B] = this.foldRight(Nil: List[B])((a, l) => Cons(f(a), l))

  /**
    * Returns a list with only the elements for which the predict p is true. The elements are in the
    * same order.
    * Stack over-flow on long list since it relies on foldRight
    * Exercise 3.19
    */
  def filter(p: (A) => Boolean): List[A] = this.foldRight(Nil: List[A]){
    (a, l) =>
      if (p(a)) Cons(a, l)
      else l
  }

  /**
    * Returns a list that concatenates all the sublists obtained by applying f to each
    * element of the given list.
    * Stack over-flow on long list since it relies on foldRight
    * Exercise 3.20
    */
  def flatMap[B](f: (A) => List[B]): List[B] = this.foldRight(Nil: List[B])(f(_).append(_))

  /**
    * given two lists and a function that takes an element in the first list and an element
    * in the second list, it constructs a list with the transformed elements. If the two
    * lists have different dimensions the zipping ends when either one of the two lists is
    * exhausted. Note that this is not tail-recursive.
    * Exercise 3.22, 3.23
    */
  def zipWith1[B, C](other: List[B])(f: (A, B) => C): List[C] = (this, other) match {
    case (Nil, _) | (_, Nil) => Nil: List[C]
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), t1.zipWith(t2)(f))
  }

  def zipWith[B, C](other: List[B])(f: (A, B) => C): List[C] = foldLeft((List[C](), other)) {
    case ((soFar, Nil), _) => (soFar, Nil)
    case ((soFar, Cons(h, t)), a) => (Cons(f(a, h), soFar), t)
  }._1.reverse

  /**
    * Given two lists constructs a list for which the elements are tuples of corresponding
    * elements in the two lists. If the two lists have different dimensions the zipping ends
    * when either one of the two lists is exhausted.
    */
  def zip[B](other: List[B]): List[(A, B)] = zipWith(other)((a, b) => (a, b))

  /**
    * Returns true if the list contains subList as a sublist. False otherwise.
    * It's a tail recursive.
    * Exercise 3.24
    */
  def hasSubsequence[B >: A](subList: List[B]): Boolean = {
    @tailrec
    def beginsWith(l1: List[A], l2: List[B]): Boolean =
      (l1, l2) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) => (h1 == h2) && beginsWith(t1, t2)
      }
    beginsWith(this, subList) ||
      (!isEmpty && tail.hasSubsequence(subList))
  }


  /**************** OLDER VERSIONS OF ROUTINES
    *
    */

  /**
    * Returns the list with the first n elements of the original list, or the same
    * list if it has length less than n.
    * A non tail recursive implementation. It fails when both the list and n are large
    */
  def takeNoTailRec(n: Int): List[A] =
    if ((n == 0) || isEmpty) Nil
    else Cons(head, tail.takeNoTailRec(n - 1))


  /**
    * Returns all the elements of the first list up until the first that fails the predicate
    * p.
    * A non tail recursive implementation. It fails when the list is large and the predicate is
    * true for a while
    */
  def takeWhileNoTailRec(p: A => Boolean): List[A] =
    if (isEmpty || !p(head)) Nil
    else Cons(head, tail.takeWhileNoTailRec(p))

  /**
    * Drops the last element of a list. If the list is empty, it remains empty
    * Implemented using ListBuffers. It gives a stack overflow error on large lists.
    */
  def initListBuffer: List[A] = {
    val buf = new ListBuffer[A]
    @tailrec
    def go(l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) =>
        buf += h
        go(t)
    }
    go(this)
  }

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

  /**
    * Returns a list consisting of the current list followed by other
    */
  def appendNoFold[B >: A](other: List[B]): List[B] = this match {
    case Nil => other
    case Cons(h, t) => Cons(h, t.appendNoFold(other))
  }
  /**
    * Returns the length of a list
    * Exercise 3.9
    */
  def lengthRight: Int = foldRight(0)((a: A, count: Int) => count + 1)

  /**
    * Implements a non tail recursive foldRight, which given a List(a_1, ... , a_n) returns
    * f(a1, f(a2, ...(f(a_n-1, f(a_n, z)))...))
    *
    * @param z the zero element
    * @param f the folding function
    */
  def foldRightNotTailRec[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(h, t) => f(h, t.foldRightNotTailRec(z)(f))
  }

  /**
    * Implements a tail recursive foldRight, which given a List(a_1, ... , a_n) returns
    * f(a1, f(a2, ...(f(a_n-1, f(a_n, z)))...)).
    * The idea behind the implementation is that f(a_1, f(a_2, ..., f(a_n-1, f(a_n, z)) ... )
    * can be rewritten as op_1(op_2(...(op_n-1(op_n)...) (z)
    * where op_i = f(a_i, z) is a transformation from a z to a new z
    * But functions are associative under compositions, so you can
    * also write them as (...((op_1.op_2).op_3),...op_n).Id, which is
    * a foldLeft. Then apply the resulting function to z.
    * Note that while this is tail-recursive, it still gives a stack overflow, probably
    * because of the function composition.
    * Exercise 3.13
    * Note that this is also not tail recursive, because of the nested function composition
    *
    * @param z the zero element
    * @param f the folding function
    */
  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B = {
    this.reverse.foldLeft(z)((B, A) => f(A, B))
    val generator: (B) => B = this.foldLeft((b: B) => b)((g: (B) => B, a: A) => (b: B) => g(f(a, b)))
    generator(z)
  }

  /**
    * Returns a list with only the elements for which the predict p is true. The elements are in the
    * same order
    * Exercise 3.21
    */
  def filterWithFlatMap(p: (A) => Boolean): List[A] = this.flatMap(a => if (p(a)) List(a) else Nil)

  /**
    * Drops the last element of a list. If the list is empty, it remains empty
    * Exercise 3.6
    */
  def initNotTailRec: List[A] = this match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, t.init)
  }
}

case object Nil extends List[Nothing] {
  override val isEmpty: Boolean = true
  override def equals(other: Any): Boolean = other match {
    case l: List[_] => l.isEmpty
    case _ => false
  }
}

case class Cons[+A](h: A, t: List[A]) extends List[A] {
  val isEmpty: Boolean = false

}

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Constructs a list consisting of n as. This is a tail-recursive version
    */
  def fill[A](n: Int)(a: A): List[A] = {
    @tailrec
    def go(n: Int, soFar: List[A]): List[A] =
      if (n == 0) soFar
      else go(n - 1, Cons(a, soFar))
    go(n, Nil)
  }

  /****************
    * Old versions of routines
    */

  /**
    * Constructs a list consisting of n as
    */
  def fillNoTailRec[A](n: Int)(a: A): List[A] =
    if (n == 0) Nil
    else Cons(a, fillNoTailRec(n - 1)(a))

  /**
    * Appends all lists in a list of lists in the order they are presented.
    * Since it relies on foldRight this gives a stack overflow on large
    * constituent lists
    * Exercise 3.15
    */
  def concatenate[A](lists: List[List[A]]): List[A] = lists.foldRight(Nil: List[A])(_.append(_))

}