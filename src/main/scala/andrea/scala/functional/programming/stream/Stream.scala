package andrea.scala.functional.programming.stream

/**
  * Created by andrea on 10/6/16.
  */

sealed trait Stream[+T] {

  /**
    * true if the Stream is empty, else false.
    */
  def isEmpty: Boolean

  /**
    * return the head (and evaluates it)
    */
  def head: T

  /**
    * the head wrapped in an option, or none if the stream is empty.
    * Exercise 5.6
    */
  def headOption: Option[T] = foldRight(None: Option[T])((t, _) => Some(t))

  /**
    * returns the tail, itself a lazily evaluated stream
    */
  def tail: Stream[T]

  /**
    * A method to turn a stream into a list
    * Exercise 5.1
    */
  def toList: List[T] = this match {
    case Cons(hd, tl) => hd() :: tl().toList
    case Empty => Nil
  }

  /**
    * A non tail-recursive version of foldRight. It constructs
    * the value f(a1, f(a2, ..., f(an, z)...) where we associate
    * to the right. Note however that the function f takes the
    * second argument by name, so that the right substream
    * can be ignored if f doesn't need it
    * @return
    */
  def foldRight[Z](z: => Z)(f: (T, => Z) => Z): Z = this match {
    case Empty => z
    case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
  }

  def map[S](f: T => S): Stream[S] = foldRight(Stream.empty[S])(
    (t, ss) => Stream.cons(f(t), ss)
  )

  def filter(p: T => Boolean): Stream[T] = foldRight(Stream.empty[T])(
    (t, tt) => if (p(t)) Stream.cons(t, tt) else tt
  )

  def append[TT >: T](other: => Stream[TT]): Stream[TT] = foldRight(other)(
    (t, tt) => Stream.cons(t, tt)
  )

  def flatMap[S](f: T => Stream[S]) = foldRight(Stream.empty[S])(
    (t, ss) => f(t).append(ss)
  )

  /**
    * A method to extract the first n elements of a stream, without
    * evaluating them. if the stream is longer than n, it is returned
    * in its entirety. If n < 1, an empty stream is returned. Not that
    * the unapply method returns Func0 parameters, so no evaluation
    * takes place upon unapplying.
    * Exercise 5.2
    */
  def take(n: Int): Stream[T] = this match {
    case Cons(hd, tl) if n > 0 => Stream.cons(hd(), tail.take(n - 1))
    case _ => Stream.empty
  }

  /**
    * Extracts a substream consisting of all the elements of a stream
    * up until and excluding the first element for which the predicate
    * p is false. It forces evaluation of the elements, in order to
    * compute the predicate.
    * Exercise 5.5
    */
  def takeWhile(p: T => Boolean): Stream[T] = foldRight(Empty: Stream[T])(
    (t, s) => if (p(t)) Stream.cons(t, s) else Stream.empty
  )

  /**
    * A method to drop the first n elements of a stream, without
    * evaluating them. if the stream is shorter than n, an empty
    * stream is returned
    * Exercise 5.2
    */
  def drop(n: Int): Stream[T] = this match {
    case Cons(_, tl) if n > 0 => tl().drop(n - 1)
    case _ => this
  }

  /**
    * Returns true if at least one element in the stream satisfies
    * the predicate p. It evaluates subsequent elements only if
    * necessary
    */
  def exists(p: T => Boolean): Boolean = foldRight(false)((t, bool) => p(t) || bool)

  /**
    * Returns true if all elements in the stream satisfy
    * the predicate p. It evaluates subsequent elements only if
    * necessary.
    * Exercise 5.4
    */
  def forall(p: T => Boolean): Boolean = foldRight(true)((t, bool) => p(t) && bool)

  /**************************** OLD IMPLEMENTATIONS OF ROUTINES
    *
    */
  /**
    * the head wrapped in an option, or none if the stream is empty
    */
  def headOptionNoFoldRight: Option[T] =
    if (isEmpty) None
    else Some(head)

  /**
    * Extracts a substream consisting of all the elements of a stream
    * up until and excluding the first element for which the predicate
    * p is false. It forces evaluation of the elements, in order to
    * compute the predicate.
    * Exercise 5.3
    */
  def takeWhileNoFoldRight(p: T => Boolean): Stream[T] = this match {
    case Cons(hd, tl) if p(hd()) => Stream.cons(hd(), tl().takeWhileNoFoldRight(p))
    case _ => Stream.empty
  }

}

case object Empty extends Stream[Nothing] {

  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("head called on empty stream")
  def tail: Stream[Nothing] = throw new NoSuchElementException("tail called on empty stream")

}

final case class Cons[+T](hd: () => T, tl: () => Stream[T]) extends Stream[T] {

  def isEmpty: Boolean = false
  def head: T = hd() // this forces evaluation of the hd thunk
  def tail: Stream[T] = tl() // this forces evaluation of the tl thunk

}

object Stream {

  /**
    * A variadic apply method to create a stream. Note that the apply method,
    * being variadic, cannot be by name, so the parameters will get evaluated
    * prior to being passed. If you need, to create a stream without evaluating
    * you need to use a non-variadic method, such as cons or Cons directly.
    */
  def apply[T](ts: T*): Stream[T] =
    if (ts.isEmpty) empty
    else cons(ts.head, apply(ts.tail: _*))

  /**
    * Smart constructor. An auxiliary method for the main constructor.
    * Nothing smart in this case really. Just the companion of cons
    */
  def empty[T]: Stream[T] = Empty

  /**
    * Smart constructor. An auxiliary method for the main constructor.
    * It is smart in that it caches values whose evaluation is forced.
    * The head val is lazy and it is passed by name to Cons. This means
    * that head is only evaluated when a method in Cons actually requires
    * head. When that happens head is evaluated and cached and for that
    * the thunk hd has to be computed for the first time. Likewise for tail
    *
    * @param hd head passed by name
    * @param tl tail passed by name
    */
  def cons[T](hd: => T, tl: => Stream[T]): Stream[T] = {
    lazy val head = hd
    lazy val tail = tl
    new Cons(() => head, () => tail)
  }

  /**
    * Smart constructor. An auxiliary method for the main constructor.
    * It is smart in that it caches values whose evaluation is forced.
    * The head val is lazy and it is passed by name to Cons. This means
    * that head is only evaluated when a method in Cons actually requires
    * head. When that happens head is evaluated and cached and for that
    * the thunk hd has to be computed for the first time. Likewise for tail
    *
    * @param hd head passed by name
    * @param tl tail passed by name
    */
  def consWithLogs[T](hd: => T, tl: => Stream[T]): Stream[T] = {
//  def cons[T](hd: => T, tl: => Stream[T]): Stream[T] = {
    println("cons called")
    lazy val head = {println(s"cons: head evaluated=$hd"); hd}
    lazy val tail = {println(s"cons: tail evaluated=$tl"); tl}
    new Cons(() => head, () => tail)
  }

}