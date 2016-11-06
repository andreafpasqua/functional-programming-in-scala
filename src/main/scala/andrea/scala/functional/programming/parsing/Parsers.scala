package andrea.scala.functional.programming.parsing

import scala.util.matching.Regex
import andrea.scala.functional.programming.either.Either

/**
  * Created by andreapasqua on 10/24/2016.
  */

trait Parsers[Parser[+_]] {
  self =>

  /**
    * ************ Primitives *********************
    */
  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[T, S](p: Parser[T], f: T => Parser[S]): Parser[S]

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice[T](p: Parser[T]): Parser[String]

  /**
    * Attaches a specified error message s to this
    */
  def label[T](p: Parser[T])(s: String): Parser[T]

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    */
  def scope[T](p: Parser[T])(s: String): Parser[T]

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt[T](p: Parser[T]): Parser[T]

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    */
  def latest[T](p: Parser[T]): Parser[T]

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    */
  def furthest[T](p: Parser[T]): Parser[T]

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    */
  def or[T](p: Parser[T], other: => Parser[T]): Parser[T]

  /**
    * Delays evaluation of the parser p
    */
  def delay[T](p: => Parser[T]): Parser[T]

  /**
    * A parser that returns a string when fed to it.
    */
  def string(s: String): Parser[String]

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    */
  def regex(r: Regex): Parser[String]

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string.
    */
  def succeed[T](t: T): Parser[T]

  /**
    * ************ Derived *********************
    */

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many[T](p: Parser[T]): Parser[List[T]] =
    or(map2(p, many(p))(_ :: _), succeed(Nil))

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1[T](p: Parser[T]): Parser[List[T]] = map2(p, many(p)) {
    case (t, list) => t :: list
  }

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN[T](p: Parser[T], n: Int): Parser[List[T]] =
    if (n > 0) map2(p, listOfN(p, n - 1))(_ :: _) else succeed(Nil)

  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[T, S](p: Parser[T], f: T => S): Parser[S] =
    flatMap(p, (t: T) => succeed(f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[T, S, U](p: Parser[T], other: => Parser[S])(f: (T, S) => U): Parser[U] =
    for {
      t <- p
      s <- other
    } yield f(t, s)

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[T, S](p: Parser[T], other: => Parser[S]): Parser[(T, S)] =
    map2(p, other)((_, _))

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    */
  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] =
    parsers.foldRight(succeed[List[T]](Nil)) {
      case (p, soFar) =>
        map2(p, soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char(c: Char): Parser[Char] = map[String, Char](string(c.toString), _.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar(c: Char): Parser[Int] = map[String, Int](slice(many(char(c))), _.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1(c: Char): Parser[Int] = map[String, Int](slice(many1(char(c))), _.length)

  /**
    * Counts how many times c1 occurs at the beginning of the string, and how many times
    * c2 occurs at the beginning of what is left. But it fails if the second count is zero.
    * In case of success it presents the two results as a tuple.
    */
  def countTwoChars(c1: Char, c2: Char) = product(countChar(c1), countChar1(c2))

  /**
    * Parses a single digit if it is followed by as many instances of the
    * character c
    */
  def numFollowedByAsMany(c: Char): Parser[Int] =
    for {
      digit <- regex("[0-9]".r)
      _ <- listOfN(char(c), digit.toInt)
    } yield digit.toInt


  /**
    * ************ Implicits *********************
    */

  implicit def stringToParser(s: String): Parser[String] = string(s)

  implicit def regexToParser(r: Regex): Parser[String] = regex(r)

  implicit def charToParser(c: Char): Parser[Char] = char(c)

  implicit def operators[T](p: Parser[T]): ParserOps[T] = ParserOps[T](p)

  case class ParserOps[T](p: Parser[T]) {

    def flatMap[S](f: T => Parser[S]): Parser[S] = self.flatMap[T, S](p, f)

    def slice: Parser[String] = self.slice(p)

    def label(s: String): Parser[T] = self.label[T](p)(s)

    def scope(s: String): Parser[T] = self.scope[T](p)(s)

    def attempt: Parser[T] = self.attempt[T](p)

    def latest: Parser[T] = self.latest[T](p)

    def furthest: Parser[T] = self.furthest[T](p)

    def or(other: => Parser[T]) = self.or(p, other)

    def |(other: => Parser[T]) = self.or(p, other)

    def map[S](f: T => S): Parser[S] = self.map(p, f)

    def **[S](other: => Parser[S]): Parser[(T, S)] = self.product(p, other)

    /**
      * It uses this to delete the corresponding portion of the input and
      * then parses what is left with other
      */
    def >*[S](other: Parser[S]): Parser[S] = self.map2(p, other) { case (t, s) => s}

    /**
      * It uses this to parse and other to delete its corresponding portion of the input
      * from what is left after this
      */
    def *<[S](other: Parser[S]): Parser[T] = self.map2(p, other){ case (t, s) => t}

  }

}