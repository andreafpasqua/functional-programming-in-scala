package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.Either
import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

trait Parsers[+T, Parser[+_]] {

  /**
    * ************ Primitives *********************
    */

  def run(s: ParserState): (Either[ParserError, T], ParserState)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => Parser[S]): Parser[S]

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: Parser[String]

  /**
    * Attaches a specified error message s to this
    */
  def label(s: String): Parser[T]

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    */
  def scope(s: String): Parser[T]

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: Parser[T]

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    */
  def latest: Parser[T]

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    */
  def furthest: Parser[T]

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    */
  def or[TT >: T](left: Parser[TT], right: => Parser[TT]): Parser[TT]

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. This doesn't use this but we put it here
    * so it can be used in other
    */
  def succeed[TT >: T](t: TT): Parser[TT]
  /**
    * ************ Derived *********************
    */

  /**
    * Same as or
    */
  def |[TT >: T](left: Parser[TT], right: => Parser[TT]): Parser[TT] = or(left, right)

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many: Parser[List[T]] = or(map2[T, List[T], List[T]](many)(_ :: _), succeed[List[T]](Nil))

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1[TT >: T]: Parser[List[TT]] = map2(many) { case (t, list) => t :: list}

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN[TT >: T](n: Int): Parser[List[TT]] =
    if (n > 0) map2(listOfN(n - 1))(_ :: _) else succeed[List[TT]](Nil)

  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[S](f: T => S): Parser[S] = flatMap(t => succeed[S](f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[TT >: T, S, U](left: Parser[TT], right: => Parser[S])(f: (TT, S) => U): Parser[U] =
    for {
      t <- left
      s <- right
    } yield f(t, s)

  def map2[TT >: T, S, U](right: => Parser[S])(f: (TT, S) => U): Parser[U] =
    for {
      t <- this
      s <- right
    } yield f(t, s)

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[TT >: T, S](left: Parser[TT], right: => Parser[S]): Parser[(TT, S)] = map2(left, right)((_, _))

  /**
    * Same as product
    */
  def **[TT >: T, S](left: Parser[TT], right: => Parser[S]): Parser[(TT, S)] = product(left, right)

  /**
    * It uses this to delete the corresponding portion of the input and
    * then parse what is left with other
    */
//  def >>[S](other: Parser[S]): Parser[S] = **(other).map(_._2)

  /**
    * It uses this to parse and other to delete its corresponding portion of the input
    * from what is left after this
    */
//  def <<[TT >: T, S](other: Parser[S]): Parser[TT] = **(other).map(_._1)

  def parse(s: ParserState): Either[ParserError, T] = run(s)._1

}