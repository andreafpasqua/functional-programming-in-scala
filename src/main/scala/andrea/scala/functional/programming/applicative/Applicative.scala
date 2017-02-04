package andrea.scala.functional.programming.applicative

import andrea.scala.functional.programming.functor.Functor
import andrea.scala.functional.programming.list.List

/**
  * Created by andrea on 1/4/17.
  */



/**
  * The primitive methods of an Applicative are either
  *   1. unit and map2(UM)
  *   2. unit and apply (UA)
  * We created ApplicativeBase to hold all other methods and then we extend it with
  * Applicative and ApplicativeUA with the implementation according to UM and UA respectively
  */
trait ApplicativeBase[A[_]] extends Functor[A]{
  def unit[T](t: T): A[T]
  def map[T, S](at: A[T])(f: T => S): A[S]
  def map2[T, S, U](at: A[T], as: A[S])(f: (T, S) => U): A[U]
  def apply[T, S](at: A[T])(af: A[T => S]): A[S]

  /**
    * Turns a list of Applicatives as into a single Applicative for the list of values (of type T)
    * Exercise 12.1
    */
  def sequence[T](as: List[A[T]]): A[List[T]] = as.foldRight(unit(List.empty[T])) (
    (monad, resultSoFar) => map2(monad, resultSoFar)(_ :: _)
  )

  /**
    * Turns a list ts of elements of type T into a single applicative using a function f that turns each
    * element of a list in an applicative of type S
    * Exercise 12.1
    */
  def traverse[T, S](ts: List[T])(f: T => A[S]): A[List[S]] = ts.foldRight(unit(List.empty[S])) (
    (t, resultSoFar) => map2(f(t), resultSoFar)(_ :: _)
  )

  /**
    * Given an applicative of type T, it constructs an applicative with a list of size n of elements of type T
    * inside
    * Exercise 12.1
    */
  def replicateM[T](m: A[T])(n: Int): A[List[T]] = sequence(List.fill(n)(m))

  /**
    * builds the product applicative. The elements inside are tuples of elements from a1 and a2.
    * Exercise 12.1
    */
  def product[T, S](a1: A[T], a2: A[S]): A[(T, S)] = map2(a1, a2)((_, _))

  /**
    * Filters a list ts on the basis of a predicate p that returns an applicative with booleans inside. The result
    * is an applicative with a sublist inside. In the sublist(s) elements of ts appear only if there is a true boolean.
    * Exercise 12.1
    */
  def filterM[T](ts: List[T])(p: T => A[Boolean]): A[List[T]] = ts.foldRight(unit(List.empty[T])) (
    (t, resultSoFar) => map2(p(t), resultSoFar)( (bool, list) => if (bool) t :: list else list)
  )

  /**
    * Clear from the types
    * Exercise 12.3
    */
  def map3[T, S, U, V](at: A[T], as: A[S], au: A[U])(f: (T, S, U) => V): A[V] =
    apply(au)(apply(as)(apply(at)(unit(f.curried))))

  /**
    * Clear from the types
    * Exercise 12.3
    */
  def map4[T, S, U, V, W](at: A[T], as: A[S], au: A[U], av: A[V])(f: (T, S, U, V) => W): A[W] =
    apply(av)(apply(au)(apply(as)(apply(at)(unit(f.curried)))))

}

/**
  * UM implementation of Applicative
  */
trait Applicative[A[_]] extends ApplicativeBase[A] {

  /**
    * Implements apply in terms of map2
    * Exercise 12.2
    */
  def apply[T, S](at: A[T])(af: A[T => S]): A[S] = map2(at, af)( (t, f) => f(t) )

  /**
    * Implements map in terms of unit and map2
    * Exercise 12.2
    */
  def map[T, S](at: A[T])(f: T => S): A[S] = map2(at, unit(f))( (t, f) => f(t))

}

/**
  * UA implementation of Applicative
  */
trait ApplicativeUA[A[_]] extends ApplicativeBase[A] {

  /**
    * Implements map2 in terms of unit and apply
    * Exercise 12.2
    */

  def map2[T, S, U](at: A[T], as: A[S])(f: (T, S) => U): A[U] = apply(as)(apply(at)(unit(f.curried)))

  /**
    * Implements map in terms of unit and apply
    * Exercise 12.2
    */
  def map[T, S](at: A[T])(f: T => S): A[S] = apply(at)(unit(f))

}