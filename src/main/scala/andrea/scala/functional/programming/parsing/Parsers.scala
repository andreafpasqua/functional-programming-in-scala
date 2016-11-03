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

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => Parsers[S, Parser[S]]): Parsers[S, Parser[S]]

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: Parsers[String, Parser[String]]

  /**
    * Attaches a specified error message s to this
    */
  def label(s: String): Parsers

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    */
  def scope(s: String): Parsers

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: Parsers

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    */
  def latest: Parsers

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    */
  def furthest: Parsers

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    */
  def or[TT >: T](other: => Parsers[TT, Parser[TT]]): Parsers[TT, Parser[TT]]

  /**
    * ************ Derived *********************
    */

  /**
    * Same as or
    */
  def |[TT >: T](other: Parser[TT]): Parsers[TT, Parser[TT]] = or(other)

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many[TT >: T]: Parsers[List[TT], Parser[List[TT]]] = map2(many)(_ :: _) or
    Parsers.succeed[List[TT], Parser[List[TT]]](Nil)

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1[TT >: T]: Parsers[List[TT], Parser[List[TT]]] =
    map2(many) { case (t, list) => t :: list}

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN[TT >: T](n: Int): Parsers[List[TT], Parser[List[TT]]] =
    if (n > 0) map2(listOfN(n - 1))(_ :: _) else Parsers.succeed[List[TT], Parser[List[TT]]](Nil)

  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[S](f: T => S): Parsers[S, Parser[S]] = flatMap(t => Parsers.succeed(f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[TT >: T, S, U](other: => Parsers[S, Parser[S]])(f: (TT, S) => U): Parsers[U, Parser[U]] =
    for {
      t <- this
      s <- other
    } yield f(t, s)

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[TT >: T, S](other: => Parsers[S, Parser[S]]): Parsers[(TT, S), Parser[(TT, S)]] =
    map2(other)((_, _))

  /**
    * Same as product
    */
  def **[TT >: T, S](other: => Parsers[S, Parser[S]]): Parsers[(TT, S), Parser[(TT, S)]] =
    product(other)

  /**
    * It uses this to delete the corresponding portion of the input and
    * then parse what is left with other
    */
  def >>[S](other: Parsers[S, Parser[S]]): Parsers[S, Parser[S]] = **(other).map(_._2)

  /**
    * It uses this to parse and other to delete its corresponding portion of the input
    * from what is left after this
    */
  def <<[TT >: T, S](other: Parsers[S, Parser[S]]): Parsers[TT, Parser[TT]] = **(other).map(_._1)

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with this
    */
  def trimmed[TT >: T]: Parsers[TT, Parser[TT]] = Parsers.char[Parser](' ').many >>
    this.asInstanceOf[Parsers[TT, Parser[TT]]] << Parsers.char[Parser](' ').many
}

object Parsers {

  /**
    * ************ Primitives *********************
    */

  /**
    * A parser that returns a string when fed to it.
    */
  def string[Parser[_]](s: String): Parsers[String, Parser[String]] = ???

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    */
  def regex[Parser[_]](r: Regex): Parsers[String, Parser[String]] = ???

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. Note you cannot implement it using map if map uses it
    * (through flatMap)
    */
  def succeed[T, Parser[_]](t: T): Parsers[T, Parser[T]] = ???

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T, Parser[_]](p: => Parsers[T, Parser[T]]): Parsers[T, Parser[T]] = ???

  /**
    * ************ Derived *********************
    */

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    */
  def sequence[T, Parser[_]](parsers: List[Parsers[T, Parser[T]]]): Parsers[List[T], Parser[List[T]]] =
    parsers.foldRight(succeed[List[T], Parser[List[T]]](Nil)) {
      case (p, soFar) => p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char[Parser[_]](c: Char): Parsers[Char, Parser[Char]] = string[Parser[String]](c.toString).map(_.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar[Parser[_]](c: Char): Parsers[Int, Parser[Int]] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1[Parser[_]](c: Char): Parsers[Int, Parser[Int]] = char(c).many1.slice.map(_.length)

  /**
    * Counts how many times c1 occurs at the beginning of the string, and how many times
    * c2 occurs at the beginning of what is left. But it fails if the second count is zero.
    * In case of success it presents the two results as a tuple.
    */
  def countTwoChars(c1: Char, c2: Char) = countChar(c1) ** countChar1(c2)

  /**
    * Parses a single digit if it is followed by as many instances of the
    * character c
    */
  def numFollowedByAsMany[Parser[_]](c: Char): Parsers[Int, Parser[Int]] =
    for {
      digit <- regex[Parser[String]]("[0-9]".r)
      _ <- char(c).listOfN(digit.toInt)
    } yield digit.toInt

  implicit def stringToParser[Parser[_]](s: String): Parsers[String, Parser[String]] = string(s)

  implicit def regexToParser[Parser[_]](r: Regex): Parsers[String, Parser[String]] = regex(r)

  implicit def charToParser[Parser[_]](c: Char): Parsers[Char, Parser[Char]] = char(c)

}