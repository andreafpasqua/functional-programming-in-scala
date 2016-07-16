package andrea.scala.functional.programming.list

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by andrea on 7/15/16.
  */
sealed trait List[+A] {
    def tail: List[A]

    def setHead[B >: A](a: B): List[B]

    def head: A

    def ::[B >: A](a: B): List[B] = Cons(a, this)

    def :+[B >: A](a: B): List[B] = append(Cons(a, Nil))

    def drop(n: Int): List[A] =
      if (n <= 0) this
      else this match {
        case Nil => throw new NoSuchMethodException(s"calling drop($n) on empty list.")
        case Cons(h, t) => t.drop(n - 1)
      }

    def dropWhile(p: A => Boolean): List[A] = this match {
      case Cons(h, t) if p(h) => t.dropWhile(p)
      case _ => this
    }

    def append[B >: A](other: List[B]): List[B] = this.foldRight(other)(Cons(_, _))

    def initSimple: List[A] = this match {
      case Nil => throw new NoSuchMethodException(s"calling init on empty list.")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, t.init)
    }

    // using ListBuffers
    def init: List[A] = {
      val buf = new ListBuffer[A]
      @tailrec
      def iterInit(l: List[A]): List[A] = l match {
        case Nil => throw new NoSuchMethodException(s"calling init on empty list.")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) =>
          buf += h
          iterInit(t)
      }
      iterInit(this)
    }

    def foldRightNonTailRecursive[B](z: B)(op: (A, B) => B): B = this match {
      case Nil => z
      case Cons(h, t) => op(h, t.foldRight(z)(op))
    }

    /** The idea is that (a_1, (a_2, ..., (a_n-1, (an, z)) ... )
      * can be rewritten as op_1(op_2(...(op_n-1(op_n)...) (z)
      * where op_i = (a_i, z) is a transformation from a z to a new z
      * But functions are associative under compositions, so you can
      * also write them as (...((op_1.op_2).op_3),...op_n).Id, which is
      * a foldLeft. Then apply the resulting function to z.
      */
    def foldRight[B](z: B)(op: (A, B) => B): B = {
      def func = this.foldLeft((b: B) => b) {
        (currOp, a) => b1 => currOp(op(a, b1))
      }
      func(z)
    }

    @tailrec
    final def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
      case Nil => z
      case Cons(h, t) => t.foldLeft(op(z, h))(op)
    }

    def length: Int = this.foldLeft(0)((n, a) => n + 1)

    def reverse: List[A] = this.foldLeft(Nil: List[A]) {
      (listSoFar, a) => Cons(a, listSoFar)
    }

    def map[B](op: A => B): List[B] =
      this.foldRight(Nil: List[B]) {
        (a, list) => Cons(op(a), list)
      }

    def filter(predicate: A => Boolean): List[A] =
      this.foldRight(Nil: List[A]) {
        (a, list) => if (predicate(a)) Cons(a, list) else list
      }

    def filterWithFlatMap(predicate: A => Boolean): List[A] =
      this.flatMap(a => if (predicate(a)) List(a) else Nil)

    def flatMap[B](op: A => List[B]): List[B] = this.foldRight(Nil: List[B])(
      (a, list) => op(a).append(list)
    )

    // Exercise 3.23
    def zipWith[B, C](other: List[B])(op: (A, B) => C): List[C] =
      (this, other) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(op(h1, h2), t1.zipWith(t2)(op))
        case _ => Nil
      }

    def takeSimple(n: Int): List[A] =
      if (n <= 0) Nil
      else this match {
        case Nil => Nil
        case Cons(h, t) => Cons(h, t.take(n - 1))
      }

    // using list buffers
    def take(n: Int): List[A] =
      if (n <= 0) Nil
      else {
        val buf = new ListBuffer[A]
        @tailrec
        def iteratingTake(n: Int, l: List[A]): List[A] =
          (n, l) match {
            case (0, _) | (_, Nil) => List(buf.toList: _*)
            case (i, Cons(h, t)) =>
              buf += h
              iteratingTake(i - 1, t)
          }
        iteratingTake(n, this)
      }

    def takeWhile(predicate: A => Boolean): List[A] = {
      val buf = new ListBuffer[A]
      @tailrec
      def iteratingTake(l: List[A]): List[A] =
        l match {
          case Nil => List(buf.toList: _*)
          case Cons(h, t) if !predicate(h) => List(buf.toList: _*)
          case Cons(h, t) =>
            buf += h
            iteratingTake(t)
        }
      iteratingTake(this)
    }

    def forall(predicate: A => Boolean) = this.foldLeft(true)((bool, a) => bool && predicate(a))

    def exists(predicate: A => Boolean) = this.foldLeft(false)((bool, a) => bool || predicate(a))

    override def toString: String =
      this match {
        case Nil => "List()"
        case _ => this.foldLeft("List(")((soFar, a) => soFar + s"$a, ").dropRight(2) + ")"
      }

    def scanRight[B](z: B)(op: (A, B) => B): List[B] =
      this.foldRight(List(z))(
        (a, list) => {
          val b = op(a, list.head)
          Cons(b, list)
        }
      )


    // should probably implement it with listbuffers
    def scanLeft[B](z: B)(op: (B, A) => B): List[B] = {
      val buf = new ListBuffer[B]
      buf += z
      this.foldLeft(z) {
        case (b, a) =>
          val b1 = op(b, a)
          buf += b1
          b1
      }
      List(buf.toList: _*)
    }

    @tailrec
    final def startsWith[B >: A](sub: List[B]): Boolean = (this, sub) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => t1.startsWith(t2)
        case _ => false
      }

    def hasSubsequence[B >: A](sub: List[B]): Boolean =
      startsWith(sub) || (this match {
        case Cons(_, t) => t.hasSubsequence(sub)
        case Nil => false
      })

  def foldRightWithStopValue[B >: A, C](z: C, stopper: B, stopValue: C)
                                       (op: (B, C) => C): C =
    this match {
      case Nil => z
      case Cons(h, _) if h == stopper => stopValue
      case Cons(h, t) =>
          val allTheRest = t.foldRightWithStopValue(z, stopper, stopValue)(op)
          if (allTheRest == stopper) stopValue
          else op(h, allTheRest)
    }

    def sum[B >: A](implicit num: Numeric[B]) = foldLeft(num.zero)(num.plus)
    def product[B >: A](implicit num: Numeric[B]) = foldLeft(num.one)(num.times)
  }

case object Nil extends List[Nothing] {
  def tail = throw new NoSuchMethodException("calling tail on empty list.")
  def setHead[A](a: A): List[A] = throw new NoSuchMethodException("calling setHead on empty list.")
  def head = throw new NoSuchMethodException("calling head on empty list.")
}

case class Cons[+A](head: A, override val tail: List[A]) extends List[A] {
  def setHead[B >: A](a: B): List[B] = Cons(a, tail)
}

object List {

  def concatenate[A](lists: List[List[A]]): List[A] =
    lists.foldRight(Nil: List[A])(_.append(_))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def add1(list: List[Int]): List[Int] = list.map(_ + 1)

  def toString(list: List[Double]): List[String] = list.map(_.toString)

  // Exercise 3.22
  def sumLists(l1: List[Int], l2: List[Int]) = l1.zipWith(l2)(_ + _)
}

object ListTest extends App {
  import List._

  val list1 = List(1, 2, 3, 4, 5)
  println(s"list1 = $list1")
  val list2= List(1, 2, 0, 4)
  println(s"list2 = $list2")

  val res1 = list1 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + t.sum
    case _ => 101
  }
  assert(res1 == 1 + 2, s"$res1 not equal to 1 + 2")

  println(s"list1.init = ${list1.init}")

  println(s"List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)) = ${List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _))}")
  println(s"Nil.length = ${Nil.length}")
  println(s"List(1, 2, 3).length = ${List(1, 2, 3).length}")
  println(s"List(1, 2, 3).reverse = ${List(1, 2, 3).reverse}")
  println(s"list1.append(list2) = ${list1.append(list2)}")
  println(s"concatenate(List(list1, list2, list1)) = ${concatenate(List(list1, list2, list1))}")
  println(s"list1.map(_ * 2) = ${list1.map(_ * 2)}")
  println(s"list1.filter(_<=3) = ${list1.filter(_<=3)}")
  println(s"list1.flatMap(i => List(i, i)) = ${list1.flatMap(i => List(i, i))}")
  println(s"list1.zipWith(list1)(_ + _) = ${list1.zipWith(list1)(_ + _)}")
  println(s"List(1, 2, 3, 4, 5).startsWith(List(1, 2, 3, 4)) = ${List(1, 2, 3, 4, 5).startsWith(List(1, 2, 3, 4))}")
  println(s"list1.take(3) = ${list1.take(3)}")
  println(s"list1.scanRight(0)(_ + _) = ${list1.scanRight(0)(_ + _)}")
  println(s"list1.scanLeft(0)(_ + _) = ${list1.scanLeft(0)(_ + _)}")
  println(s"hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)) = ${List(1, 2, 3, 4, 5).hasSubsequence(List(2, 3, 4))}")
  println(s"hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 5)) = ${List(1, 2, 3, 4, 5).hasSubsequence(List(2, 3, 5))}")
  println(s"hasSubsequence(List(), List(2, 3, 4)) = ${List().hasSubsequence(List(2, 3, 4))}")
  println(s"hasSubsequence(List(), List()) = ${List().hasSubsequence(List())}")
  println(s"list1.foldRightWithStopValue(1, 0, 0)(_ * _) = ${list1.foldRightWithStopValue(1, 0, 0)(_ * _)}")
  println(s"list2.foldRightWithStopValue(1, 0, 0)(_ * _) = ${list2.foldRightWithStopValue(1, 0, 0)(_ * _)}")
}

