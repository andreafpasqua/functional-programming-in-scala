package andrea.scala.functional.programming.either

import andrea.scala.functional.programming.list.Cons
/**
  * Created by andrea on 7/16/16.
  */
trait Either[+A, +B] {
  // Exercise 4.6
  def map[C](op: B => C): Either[A, C] =
    this match {
      case Left(a) => Left(a)
      case Right(b) => Right(op(b))
    }
  def flatMap[AA >: A, C](op: B => Either[AA, C]): Either[AA, C] =
    this match {
      case Left(a) => Left(a)
      case Right(b) => op(b)
    }
  def orElse[AA >: A, BB >: B](default: Either[AA, BB]): Either[AA, BB] =
    this match {
      case Left(_) => default
      case Right(b) => Right(b)
    }
  // notice that e1 is taken from the right side because flatMap acts on the right side.
  // for is just syntactic sugar for flatMap
  def map2[AA >: A, C, D](other: Either[AA, C])(op: (B, C) => D): Either[AA, D] =
    for {
      e1 <- this
      e2 <- other
    } yield op(e1, e2)

}
case class Left[A, B](get: A) extends Either[A, B]
case class Right[A, B](get: B) extends Either[A, B]