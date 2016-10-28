package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.testing.{Prop, Sampler}
import andrea.scala.functional.programming.either.{Either, Left}

import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class Parser[+T](run: String => Either[ParserError, T]) {

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => Parser[S]): Parser[S] = ???

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: Parser[String] = ???

  /**
    * Attaches a specified error message s to this
    */
  def label(s: String): Parser[T] = ???

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same imput like other
    */
  def or[TT >: T](other: => Parser[TT]): Parser[TT] = Parser(
    string => run(string).left.orElse(other.run(string))
  )

  /**
    * Same as or
    */
  def |[TT >: T](other: Parser[TT]): Parser[TT] = or(other)

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many: Parser[List[T]] = map2(many)(_ :: _) or Parser.succeed(Nil)

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1: Parser[List[T]] = map2(many) { case (t, list) => t :: list}

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN(n: Int): Parser[List[T]] =
    if (n > 0) map2(listOfN(n - 1))(_ :: _) else Parser.succeed(Nil)


  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[S](f: T => S): Parser[S] = flatMap(t => Parser.succeed(f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[S, U](other: => Parser[S])(f: (T, S) => U): Parser[U] =
    for {
      t <- this
      s <- other
    } yield f(t, s)

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[S](other: => Parser[S]): Parser[(T, S)] = map2(other)((_, _))

  /**
    * Same as product
    */
  def **[S](other: => Parser[S]): Parser[(T, S)] = product(other)

  /**
    * It uses this to delete the corresponding portion of the input and
    * then parse what is left with other
    */
  def >[S](other: Parser[S]): Parser[S] = **(other).map(_._2)

  /**
    * It uses this to parse and other to delete its corresponding portion of the input
    * from what is left after this
    */
  def <[S](other: Parser[S]): Parser[T] = **(other).map(_._1)

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with this
    */
  def trimmed: Parser[T] = Parser.char(' ').many > this < Parser.char(' ')
}

object Parser {

  /**
    * A parser that returns a string when fed to it.
    */
  def string(s: String): Parser[String] = ???

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    *
    * @return
    */
  def regex(r: Regex): Parser[String] = ???

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string
    */
  def succeed[T](t: T): Parser[T] = string("").map(_ => t)

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => Parser[T]): Parser[T] = p

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    *
    */
  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] =
    parsers.foldRight(succeed[List[T]](Nil)) {
      case (p, soFar) => p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar(c: Char): Parser[Int] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1(c: Char): Parser[Int] = char(c).many1.slice.map(_.length)

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
  def numFollowedByAsMany(c: Char): Parser[Int] =
    for {
      digit <- regex("[0-9]".r)
      _ <- char(c).listOfN(digit.toInt)
    } yield digit.toInt

  implicit def stringToParser(s: String): Parser[String] = string(s)

  implicit def regexToParser(r: Regex): Parser[String] = regex(r)

  implicit def charToParser(c: Char): Parser[Char] = char(c)

}
