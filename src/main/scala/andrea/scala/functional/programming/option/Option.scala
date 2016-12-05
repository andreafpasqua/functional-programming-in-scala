package andrea.scala.functional.programming.option


/**
  * Created by andrea on 10/2/16.
  */
sealed trait Option[+A] {

  /**
    * Maps the content of the option if any using f.
    * Exercise 4.1
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }


  /**
    * Extracts the content of the option. If there is no content it
    * returns an exception
    */
  def get: A = this match {
    case Some(a) => a
    case None => throw new NoSuchElementException("get called on empty Option")
  }

  /**
    * Extracts the content of the option if any. If there is no content it
    * returns the specified (and lazily evaluated) default value default
    * Exercise 4.1
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  /**
    * maps the content of the option if any using f. f returns an
    * optional value, so even if the original option had content, the
    * application of f could result in None.
    * Exercise 4.1
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  /**
    * Returns the option, but if it is empty it returns the lazily evaluated
    * default option
    * Exercise 4.1
    */
  def orElse[B >: A](op: => Option[B]): Option[B] = map(Some(_)).getOrElse(op)

  /**
    * Filters the content of the option if any using the predicate p.
    * Exercise 4.1
    */
  def filter(p: A => Boolean) = flatMap(v => if (p(v)) Some(v) else None)

  /**
    * Combines two options using the function f that takes the possible content of
    * the first option and the possible content of the other option. If either of them
    * is empty, it returns None.
    * Exercise 4.3
    */
  def map2[B, C](other: Option[B])(f: (A, B) => C): Option[C] =
    flatMap(a => other.map(b => f(a, b)))

  /**
    * true if the Option is None else false
    */
  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

}

object Option {

  /**
    * Traverse a list and applies a function f to each value resulting in
    * an optional list. If f returns None on any of the values in the list,
    * then the method returns None
    * Exercise 4.5
    */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil):Option[List[B]])(
      (a, op) => f(a).map2(op)((b, l) => b :: l)
    )

  /**
    * Given a list of options, it returns an optional list of the values inside
    * the options. If even one of the options is empty, it returns None.
    * Exercise 4.5
    */
  def sequence[A](os: List[Option[A]]): Option[List[A]] =
    traverse(os)(a => a)

  /*************************OLDER IMPLEMENTATIONS OF ROUTINES
    */

  /**
    * Given a list of options, it returns an optional list of the values inside
    * the options. If even one of the options is empty, it returns None.
    * Exercise 4.4
    */
  def sequenceNoTraverse[A](os: List[Option[A]]): Option[List[A]] =
    os.foldRight(Some(Nil): Option[List[A]]){
      (o, soFar) => o.map2(soFar)((a, l) => a :: l)
    }

}

case object None extends Option[Nothing] {
  def isEmpty: Boolean = true
}
case class Some[A](x: A) extends Option[A] {
  def isEmpty: Boolean = false
}