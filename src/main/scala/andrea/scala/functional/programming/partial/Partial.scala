package andrea.scala.functional.programming.partial

/**
  * Created by andreapasqua on 10/04/2016.
  */
sealed trait Partial[+E, +A] {

  /**
    * Maps the success value using a function f. If there were errors returns the errors
    * Exercise 4.8
    */
  def map[B](f: A => B): Partial[E, B] = flatMap(a => Success(f(a)))


  /**
    * Maps the success with a function that itself returns a partial and returns the result.
    * If there were errors returns the errors without change.
    * Exercise 4.8
    */
  def flatMap[EE >: E, B](f: A => Partial[EE, B]): Partial[EE, B] = this match {
    case Success(a) => f(a)
    case Errors(es) => Errors(es)
  }

  /**
    * If a success returns this, else returns the default
    * Exercise 4.8
    * @return
    */
  def orElse[EE >: E, AA >: A](default: => Partial[EE, AA]): Partial[EE, AA] = this match {
    case Errors(_) => default
    case _ => this
  }

  /**
    * it return the success value. If there are errors it returns the default
    * Exercise 4.8
    */
  def getOrElse[AA >: A](default: AA): AA = this match {
    case Errors(_) => default
    case Success(a) => a
  }

  /**
    * it combines two success values using the function f and returns a success. If there were errors
    * it combines the sequences of errors and returns those.
    * Exercise 4.8
    */
  def map2[EE >: E, B, C](other: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] = (this, other) match {
    case (Errors(seq1), Errors(seq2)) => Errors(seq1 ++ seq2)
    case _ => flatMap(a => other.map(b => f(a, b)))
  }

}

object Partial {

  /**
    * Takes a list of Partials and if there are errors returns them all as Errors, else it returns the list
    * of values as a success.
    * Exercise 4.8
    */
  def sequence[E, A](ps: List[Partial[E, A]]): Partial[E, List[A]] =
    traverse(ps)(p => p)

  /**
    * Traverses a list of values and applies the function f to each. if there are no errors, it collects
    * the values generated and returns them as a success. If there are errors, returns the errors as Errors
    * Exercise 4.8
    */
  def traverse[E, A, B](as: List[A])(f: A => Partial[E, B]): Partial[E, List[B]] =
    as.foldRight(Success(Nil): Partial[E, List[B]])(
      (a, p) => f(a).map2(p)(_ :: _)
    )
}

case class Errors[+E](get: Seq[E]) extends Partial[E, Nothing]
case class Success[+A](get: A) extends Partial[Nothing, A]