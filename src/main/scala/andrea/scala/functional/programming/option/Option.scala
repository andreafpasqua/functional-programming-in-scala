package andrea.scala.functional.programming.option

import andrea.scala.functional.programming.list.{List, Nil}

/**
  * Created by andrea on 7/16/16.
  */
trait Option[+A] {
  // Exercise 4.1
  def map[B](op: A => B): Option[B] = flatMap(a => Some(op(a)))
  def flatMap[B](op: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => op(a)
    }
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }
  def orElse[B >: A](other: => Option[B]): Option[B] =
    this match {
      case None => other
      case _ => this
    }
  def filter(predicate: A => Boolean): Option[A] =
    flatMap(a => if (predicate(a)) Some(a) else None)

  // Exercise 4.3
  def map2[B, C](other: Option[B])(op: (A, B) => C): Option[C] =
    flatMap(a => other.flatMap(b => Some(op(a, b))))
}

object Option {

  // Exercise 4.2
  implicit class VarianceSeqExt(xs: Seq[Double]) {
    def mean: Option[Double] = {
      val xsLen = xs.length
      xsLen match {
        case 0 => None
        case len => Some(xs.sum / len)
      }
    }

    def variance: Option[Double] =
      mean.flatMap(m => xs.map(x => math.pow(x - m, 2)).mean)
  }
    // Exercise 4.4
    def sequenceOld[A](l: List[Option[A]]): Option[List[A]] =
      l.foldRight(Some(Nil): Option[List[A]])(
        (aOpt, listOpt) => aOpt.map2(listOpt)(_ :: _)
      )

    // Exercise 4.5
    def sequence[A](l: List[Option[A]]): Option[List[A]] =
      traverse(l)(opt => opt)
    def traverse[A, B](l: List[A])(op: A => Option[B]): Option[List[B]] =
      l.foldRight(Some(Nil): Option[List[B]])(
        (a, listOpt) => op(a).map2(listOpt)(_ :: _)
      )

}

case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

