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
    */
  def foldRight[Z](z: => Z)(f: (T, => Z) => Z): Z = this match {
    case Empty => z
    case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
  }

  /**
    * Maps all the elements of a stream using the function f. It
    * uses foldRight so applies the function only as the next element
    * is needed. However the first element is always needed.
    * Exercise 5.7
    */
  def map[S](f: T => S): Stream[S] = foldRight(Stream.empty[S])(
    (t, ss) => Stream.cons(f(t), ss)
  )

  /**
    * Filters the elements of a stream according to the predicate p. It
    * uses foldRight so applies the filter only as the next element
    * is needed. The first element however needs to be evaluated.
    * Exercise 5.7
    */
  def filter(p: T => Boolean): Stream[T] = foldRight(Stream.empty[T])(
    (t, tt) => if (p(t)) Stream.cons(t, tt) else tt
  )

  /**
    * Appends two streams. It uses foldRight so elements of either
    * stream are accesses only as they become necessary.
    * Exercise 5.7
    */
  def append[TT >: T](other: => Stream[TT]): Stream[TT] = foldRight(other)(
    (t, tt) => Stream.cons(t, tt)
  )

  /**
    * Maps all the elements of a stream using the function f (which itself
    * returns a stream) and then flattens together the various elements
    * into a single of the stream. It uses foldRight so it applies the function
    * only as needed.
    * Exercise 5.7
    */
  def flatMap[S](f: T => Stream[S]) = foldRight(Stream.empty[S])(
    (t, ss) => f(t).append(ss)
  )

  /**
    * Optionally returns the first element that satisfies the
    * predicate p or None
    */
  def find(p: T => Boolean): Option[T] = filter(p).headOption

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
    * Exercise 5.13
    */
  def takeWhile(p: T => Boolean): Stream[T] = Stream.unfold(this) {
    case Cons(hd, tl) if p(hd()) => Some((hd(), tl()))
    case _ => None
  }

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

  /**
    * given two stream and a function f that takes an element in the first stream and an element
    * in the second stream, it constructs a stream with the transformed elements. If the two
    * streams have different lengths the zipping ends when either one of the two lists is
    * exhausted.
    * Exercise 5.13
    */
  def zipWith[S, U](other: Stream[S])(f: (T, S) => U): Stream[U] = Stream.unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  /**
    * given two stream and a function f that takes optionally an element in the first stream and optionally
    * an element in the second stream, it constructs a stream with the transformed elements. If the two
    * streams have different lengths the zipping continues until the longer list is exhausted
    * Exercise 5.13
    */
  def zipWithAll[S, U](other: Stream[S])(f: (Option[T], Option[S]) => U): Stream[U] = Stream.unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h, t), _) => Some( f(Some(h()), None), (t(), Empty))
    case (_, Cons(h, t)) => Some( f(None, Some(h())), (Empty, t()))
    case (Empty, Empty) => None
  }

  /**
    * Returns true of the stream begins with subStream, else it returns false
    * Exercise 5.14
    */
  def startsWith[TT >: T](subStream: Stream[TT]): Boolean = zipAll(subStream).takeWhile(_._2.nonEmpty)
    .forall(tuple => tuple._1 == tuple._2)

  /**
    * Returns a stream containing all the tails of the given stream. A tail of a stream would be the
    * stream obtained by dropping some elements at the front
    * Exercise 5.15
    */
  def tails: Stream[Stream[T]] = Stream.cons(this, Stream.unfold(this)(
    stream => if (stream.isEmpty) None else Some((stream.tail, stream.tail))))

  /**
    * Returns true if subStream is a substream of the main stream, else it returns false
    */
  def hasSubsequence[TT >: T](subStream: Stream[TT]): Boolean = tails.exists(_.startsWith(subStream))


  def scanRight[Z](z: Z)(f: (T, => Z) => Z): Stream[Z] = foldRight((z, Stream(z))){
    case (t, tuple) => {
      lazy val tupleCached = tuple
      val newZ = f(t, tupleCached._1)
      (newZ, Stream.cons(newZ, tupleCached._2))
    }
  }._2

  /**
    * Given two streams it constructs a new stream with the elements of the original stream tupled.
    * If the two streams have different lengths the zipping ends when either one of the two lists is
    * exhausted.
    */
  def zip[S](other: Stream[S]): Stream[(T, S)] = zipWith(other)((_, _))

  /**
    * given two streams, it constructs a new stream with the elements of the original stream tupled.
    * If the two streams have different lengths the zipping continues but with Nones on the shorter
    * side
    * Exercise 5.13
    */
  def zipAll[S](other: Stream[S]): Stream[(Option[T], Option[S])] = zipWithAll(other)((op1, op2) => (op1, op2))

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


  /**
    * Extracts a substream consisting of all the elements of a stream
    * up until and excluding the first element for which the predicate
    * p is false. It forces evaluation of the elements, in order to
    * compute the predicate.
    * Exercise 5.5
    */
  def takeWhileFoldRight(p: T => Boolean): Stream[T] = foldRight(Empty: Stream[T])(
    (t, s) => if (p(t)) Stream.cons(t, s) else Stream.empty
  )

  /**
    * A method to extract the first n elements of a stream, without
    * evaluating them. if the stream is longer than n, it is returned
    * in its entirety. If n < 1, an empty stream is returned. Not that
    * the unapply method returns Func0 parameters, so no evaluation
    * takes place upon unapplying.
    * Exercise 5.13
    */
  def takeUnfold(n: Int): Stream[T] = Stream.unfold((n, this)) {
    case (i, Cons(hd, tl)) if i > 0 => Some((hd(), (i - 1, tl())))
    case _ => None
  }

  /**
    * Maps all the elements of a stream using the function f. It
    * uses foldRight so applies the function only as the next element
    * is needed. However the first element is always needed.
    * Exercise 5.13
    */
  def mapUnfold[S](f: T => S): Stream[S] = Stream.unfold(this){
    case Cons(hd, tl) => Some((f(hd()), tl()))
    case _ => None
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
    * Generates an infinite stream of identical values t. Note
    * that this works better than cons(a, constant(a)) because
    * instead of keeping calling the same function, it calls a
    * lazily evaluated val.
    * Exercise 5.8
    */
  def constant[T](t: T): Stream[T] = {
    lazy val stream: Stream[T] = Cons(() => t, () => stream)
    stream
  }

  /**
    * Generates an infinite stream as follows n, n+1, n+2 and so on, starting
    * at the parameter n.
    * Exercise 5.12
    */
  def from(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  /**
    * Generates a stream by starting with a state s, applying to it
    * a function f that generates a value and a new state and iterate
    * the process. If the function returns None, then the stream terminates
    *
    * @return
    */
  def unfold[T, S](s: S)(f: S => Option[(T, S)]): Stream[T] =
    f(s).map {
      case (t, newS) => cons(t, unfold(newS)(f))
    }.getOrElse(empty)

  /**************************** OLD IMPLEMENTATIONS OF ROUTINES
    *
    */
  /**
    * Generates an infinite stream of identical values t. Note
    * that this works better than cons(a, constant(a)) because
    * instead of keeping calling the same function, it calls a
    * lazily evaluated val.
    * Exercise 5.12
    */
  def constantOld[T](t: T): Stream[T] = unfold[T, T](t)(s => Some((s, s)))

  /**
    * Generates an infinite stream as follows n, n+1, n+2 and so on, starting
    * at the parameter n.
    * Exercise 5.9
    */
  def fromOld(n: Int): Stream[Int] = cons(n, fromOld(n + 1))

}