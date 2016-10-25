package andrea.scala.functional.programming.either

/**
  * Created by andreapasqua on 10/03/2016.
  */
sealed trait Either[+L, +R] {

  /**
    * Generates a LeftProjection object, which specialized map, flatMap, etc. to act on the
    * left value only, if any
    */
  def left: LeftProjection[L, R] = new LeftProjection(this)

  /**
    * Generates a RightProjection object, which specialized map, flatMap, etc. to act on the
    * right value only, if any
    */
  def right: RightProjection[L, R] = new RightProjection(this)

  /**
    * Applies one of two functions fl or fr to either the left value or the right value
    * depending on which is present and returns the value of the function
    */
  def map[S](fl: L => S, fr: R => S): S = this match {
    case Left(v) => fl(v)
    case Right(v) => fr(v)
  }

  /**
    * True if it's a Left
    */
  def isLeft: Boolean

  /**
    * True if it's a Right
    */
  def isRight: Boolean = !isLeft
}
case class Left[+L](value: L) extends Either[L, Nothing] {
  def isLeft: Boolean = true
}
case class Right[+R](value: R) extends Either[Nothing, R] {
  def isLeft: Boolean = false
}

object Either {

  /**
    * Given a list of Either, constructs Left of the list of right values in the same order.
    * If even one of the Either is a Right, then it returns the leftmost Right it encounters.
    */
  def sequenceLeft[L, R](es: List[Either[L, R]]): Either[List[L], R] =
    traverseLeft(es)(e => e)

  /**
    * Given a list of Either, constructs Right of the list of right values in the same order.
    * If even one of the Either is a Left, then it returns the leftmost Left it encounters.
    * Exercise 4.7
    */
  def sequenceRight[L, R](es: List[Either[L, R]]): Either[L, List[R]] =
    traverseRight(es)((e) => e)

  /**
    * Given a list of Either, it traverses it acting on any left values with f. As long as it
    * encounters only left values and as long as the application of f returns a left value,
    * it returns a Left containing the list of the lfet values. If either it encounters a rigjt value
    * or if the application of f returns a right value, then it returns that leftmost right value.
    */
  def traverseLeft[L, R, S](es: List[L])(f: L => Either[S, R]): Either[List[S], R] =
    es.foldRight(Left(List()): Either[List[S], R])(
      (e, eList) => f(e).left.map2(eList)(_ :: _)
    )


  /**
    * Given a list of Either, it traverses it acting on any right values with f. As long as it
    * encounters only right values and as long as the application of f returns a right value,
    * it returns a Right containing the list of the right values. If either it encounters a left value
    * or if the application of f returns a left value, then it returns the leftmost left value.
    * Exercise 4.7
    */
  def traverseRight[L, R, S](es: List[R])(f: R => Either[L, S]): Either[L, List[S]] =
    es.foldRight(Right(List()): Either[L, List[S]])(
      (e, eList) => f(e).right.map2(eList)(_ :: _)
    )

}

case class LeftProjection[+L, +R](either: Either[L, R]) {

  /**
    * Maps the left value if any and encloses the result in a Left. If there is no left value,
    * then it returns this
    */
  def map[S](f: L => S): Either[S, R] = flatMap(l => Left(f(l)))

  /**
    * Returns the Either object obtained by applying f to the left value if any. If there is
    * a right value it returns this
    */
  def flatMap[RR >: R, S](f: L => Either[S, RR]): Either[S, RR] = this.either match {
    case Right(v) => Right(v)
    case Left(v) => f(v)
  }

  /**
    * Returns the left value if any. If there is a right value, then it returns the default.
    */
  def getOrElse[LL >: L](default: LL) = this.either match {
    case Right(_) => default
    case Left(v) => v
  }

  /**
    * Returns the left value if any. If there is a right value, then it throws an exception.
    */
  def get: L = this.either match {
    case Right(_) => throw new NoSuchElementException("get called on an empty LeftProjection")
    case Left(v) => v
  }

  /**
    * Returns this if there is a left value, else it returns the default.
    */
  def orElse[LL >: L, RR >: R](default: => Either[LL, RR]): Either[LL, RR] = this.either match {
    case Right(_) => default
    case Left(_) => this.either
  }

  /**
    * Returns the Left that contains the value obtained by applying f to the left values of this
    * and the left value of other. If this is a Right it returns this,
    * instead if other is a Right, then it returns other
    */
  def map2[RR >: R, S, T](other: Either[S, RR])(f: (L, S) => T): Either[T, RR] =
    flatMap(l => other.left.map(s => f(l, s)))


  /**
    * Returns an option containing the Left if there is a left value and it satisfies
    * the predicate p. If there is no left value or if the left value does not satisfy the
    * predicate, then it return an empty option
    */
  def filter(p: L => Boolean): Option[Either[L, R]] = this.either match {
    case Left(v) if p(v) => Some(this.either)
    case _ => None
  }
}

case class RightProjection[+L, +R](either: Either[L, R]) {


  /**
    * Maps the right value if any and encloses the result in a Right. If there is no right value,
    * then it returns this.
    * Exercise 4.6
    */
  def map[S](f: R => S): Either[L, S] = flatMap(r => Right(f(r)))

  /**
    * Returns the Either object obtained by applying f to the right value if any. If there is
    * a left value it returns this.
    * Exercise 4.6
    */
  def flatMap[LL >: L, S](f: R => Either[LL, S]): Either[LL, S] = this.either match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  /**
    * Returns the right value if any. If there is a left value, then it returns the default.
    */
  def getOrElse[RR >: R](default: RR) = this.either match {
    case Left(_) => default
    case Right(v) => v
  }

  /**
    * Returns the right value if any. If there is a left value, then it throws an exception.
    */
  def get: R = this.either match {
    case Left(_) => throw new NoSuchElementException("get called on an empty RightProjection")
    case Right(v) => v
  }

  /**
    * Returns this if there is a right value, else it returns the default.
    * Exercise 4.6
    */
  def orElse[LL >: L, RR >: R](default: => Either[LL, RR]): Either[LL, RR] = this.either match {
    case Left(_) => default
    case Right(_) => this.either
  }

  /**
    * Returns the Right that contains the value obtained by applying f to the right values of this
    * and the right value of other. If this is a Left it returns this,
    * instead if other is a Left, then it returns other.
    * Exercise 4.6
    */
  def map2[LL >: L, S, T](other: Either[LL, S])(f: (R, S) => T): Either[LL, T] =
    flatMap(r => other.right.map(s => f(r, s)))

  /**
    * Returns an option containing the Right if there is a left value and it satisfies
    * the predicate p. If there is no left value or if the left value does not satisfy the
    * predicate, then it return an empty option
    */
  def filter(p: R => Boolean): Option[Either[L, R]] = this.either match {
    case Right(v) if p(v) => Some(this.either)
    case _ => None
  }

}