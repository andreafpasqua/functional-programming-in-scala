package andrea.scala.functional.programming

import java.util.NoSuchElementException

/**
  * Created by andrea on 6/28/16.
  */
object Chapter5 {

  sealed trait Stream[+T] {
    def head: T

    def tail: Stream[T]

    def isEmpty: Boolean

    def foldRight[Z](z: => Z)(op: (T, => Z) => Z): Z = this match {
      case Cons(hd, tl) => op(hd(), tl().foldRight(z)(op))
      case _ => z
    }

    def map[S](op: T => S): Stream[S] =
      this.foldRight(Empty: Stream[S]) {
        (t, stream) => new Cons(() => op(t), () => stream)
      }

    def map2[S](op: T => S): Stream[S] =
      Stream.unfold(this) {
        stream => stream.headOption.map {
          h => (op(h), stream.tail)
        }
      }

    def filter(p: T => Boolean): Stream[T] =
      this.foldRight(Empty: Stream[T]) {
        (t, stream) =>
          if (p(t)) new Cons(() => t, () => stream)
          else stream
      }

    def append[S >: T](s2: Stream[S]): Stream[S] =
      this.foldRight(s2) {
        (t, stream) => new Cons(() => t, () => stream)
      }

    def flatMap[S](op: T => Stream[S]): Stream[S] =
      this.foldRight(Empty: Stream[S]) {
        (t, stream) =>
          op(t).append(stream)
      }

    def toList: List[T] =
      this.foldRight(Nil: List[T]) {
        (t, list) => t :: list
      }

    def take(n: Int): Stream[T] =
      Stream.unfold((n, this)) {
        case (i, stream) =>
          if ((i <= 0) || stream.isEmpty) None
          else Some((stream.head, (i - 1, stream.tail)))
      }

    def drop(n: Int): Stream[T] =
      if ((n <= 0) || isEmpty) this
      else this.tail.drop(n - 1)

    def dropWhile(p: T => Boolean): Stream[T] =
      this.foldRight(Empty: Stream[T]) {
        (t, stream) => if (p(t)) stream else new Cons(() => t, () => stream)
      }

    def takeWhile(p: T => Boolean): Stream[T] = {
      this.foldRight {
        Empty: Stream[T]
      } {
        (t, z) =>
          if (p(t)) new Cons(() => t, () => z)
          else Empty
      }
    }

    def takeWhile2(p: T => Boolean): Stream[T] =
      Stream.unfold(this) {
        stream => stream.headOption.flatMap {
          h =>
            if (p(h)) Some((h, stream.tail))
            else None
        }
      }

    def headOption: Option[T] = this.foldRight(None: Option[T]) {
      (t, z) => Some(t)
    }

    def forAll(p: T => Boolean): Boolean =
      this.foldRight(true) {
        (t, bool) => p(t) && bool
      }

    def zip[S](other: Stream[S]): Stream[(T, S)] =
      zipAll(other).flatMap{
        case (Some(t), Some(s)) => Stream.cons((t, s), Empty)
        case _ => Empty
      }


    def zipAll[S](other: Stream[S]): Stream[(Option[T], Option[S])] =
      Stream.unfold((this, other)) {
        case (s1, s2) =>
          val h1 = s1.headOption
          val h2 = s2.headOption
          val newStatus1 = if (h1.isEmpty) this else this.tail
          val newStatus2 = if (h2.isEmpty) other else other.tail
          if (h1.isEmpty && h2.isEmpty) None
          else Some(((h1, h2), (newStatus1, newStatus2)))
      }

  }

  final class Cons[+T](val hd: () => T, val tl: () => Stream[T]) extends Stream[T] {
    def head: T = hd()
    def tail: Stream[T] = tl()
    def isEmpty: Boolean = false
  }

  object Empty extends Stream[Nothing] {
    def head: Nothing = throw new NoSuchElementException("head of empty stream")
    def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    def isEmpty: Boolean = true
  }

  object Cons {
    def unapply[T](ob: Cons[T]): Option[(() => T, () => Stream[T])] = Some(ob.hd, ob.tl)
  }

  object Stream {

    def cons[T](hd: => T, tl: => Stream[T]): Stream[T] = {
      lazy val head = hd
      lazy val tail = tl
      new Cons(() => head, () => tail)
    }

    def empty[T]: Stream[T] = Empty

    def apply[T](ts: T*): Stream[T] =
      if (ts.isEmpty) empty else cons(ts.head, apply(ts.tail: _*))

    def unfold[T, S](s: S)(op: S => Option[(T, S)]): Stream[T] =
      op(s) match {
        case None => Empty
        case Some((t, newS)) => new Cons(() => t, () => unfold(newS)(op))
      }

    val ones = constant(1)

    def constant[T](t: T): Stream[T] = unfold(Unit)(Unit => Some((t, Unit)))

    def from(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

    val fibs: Stream[Int] = unfold((0, 1)) {
      case (i, j) => Some((i, (j, i + j)))
    }
  }

}

object TestChapter5 extends App {

  val stream1 = Chapter5.Stream(1, 1, 1, 2, 3, 4, 5)
  val stream2 = Chapter5.Stream[Int]()
  val stream3 = Chapter5.Stream.constant(5)
  val stream4 = Chapter5.Stream.from(5)
  val stream5 = Chapter5.Stream.fibs
  val stream6 = Chapter5.Stream.ones
  println(s"Stream1 = ${stream1.toList}")
  println(s"Stream2 = ${stream2.toList}")
  println(s"Stream3 = ${stream3.take(5).toList}...")
  println(s"Stream4 = ${stream4.take(5).toList}...")
  println(s"Stream5 = ${stream5.take(5).toList}...")
  println(s"Stream6 = ${stream6.take(5).toList}...")
  println(s"The head of Stream1 is ${stream1.head}")
  println(s"The tail of Stream1 is ${stream1.tail.toList}")
  println(s"stream1.take(4) is ${stream1.take(4).toList}")
  println(s"stream1.drop(3) is ${stream1.drop(3).toList}")
  println(s"stream2.drop(3) is ${stream2.drop(3).toList}")
  println(s"stream1.dropWhile(_ <= 3) is ${stream1.dropWhile(_ <= 3).toList}")
  println(s"stream1.forAll(_ <= 3) is ${stream1.forAll(_ <= 3)}")
  println(s"stream1.takeWhile(_ <= 3) is ${stream1.takeWhile(_ <= 3).toList}")
  println(s"stream1.takeWhile2(_ <= 3) is ${stream1.takeWhile2(_ <= 3).toList}")
  println(s"stream1.headOption is ${stream1.headOption}")
  println(s"stream2.headOption is ${stream2.headOption}")
  println(s"stream1.map(_ * 2) is ${stream1.map(_ * 2).toList}")
  println(s"stream2.map(_ * 2) is ${stream2.map(_ * 2).toList}")
  println(s"stream1.map2(_ * 2) is ${stream1.map2(_ * 2).toList}")
  println(s"stream2.map2(_ * 2) is ${stream2.map2(_ * 2).toList}")
  println(s"stream1.filter(_ % 2 == 0) is ${stream1.filter(_ % 2 == 0).toList}")
  println(s"stream2.filter(_ % 2 == 0) is ${stream2.filter(_ % 2 == 0).toList}")
  println(s"stream1.append(stream1) is ${stream1.append(stream1).toList}")
  println(s"stream2.append(stream1) is ${stream2.append(stream1).toList}")
  val printVal = stream1.flatMap(i => Chapter5.Stream(i, i, i)).toList
  println(s"stream1.flatMap(i => Chapter5.Stream(i, i, i)) is $printVal")
  println(s"stream3.take(10) is ${stream3.take(10).toList}")
  println(s"stream4.take(10) is ${stream3.take(10).toList}")
  println(s"stream5.take(10) is ${stream3.take(10).toList}")
  println(s"stream6.take(6) is ${stream3.take(10).toList}")

  val x = List(1, 2)
}