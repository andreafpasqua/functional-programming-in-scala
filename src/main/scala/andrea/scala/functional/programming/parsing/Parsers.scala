package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction

import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

sealed trait Parsers[+T] {

  def run(s: String): Either[ParserError, T]

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => Parsers[S]): Parsers[S]

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: Parsers[String]

  /**
    * Attaches a specified error message s to this
    */
  def label(s: String): Parsers[T]

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    */
  def scope(s: String): Parsers[T]

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: Parsers[T]

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    */
  def latest: Parsers[T]

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    */
  def furthest: Parsers[T]

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same imput like other
    */
  def or[TT >: T](other: => Parsers[TT]): Parsers[TT]

  /**
    * Same as or
    */
  def |[TT >: T](other: Parsers[TT]): Parsers[TT] = or(other)

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many: Parsers[List[T]] = map2(many)(_ :: _) or Parsers.succeed(Nil)

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1: Parsers[List[T]] = map2(many) { case (t, list) => t :: list}

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN(n: Int): Parsers[List[T]] =
    if (n > 0) map2(listOfN(n - 1))(_ :: _) else Parsers.succeed(Nil)


  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[S](f: T => S): Parsers[S] = flatMap(t => Parsers.succeed(f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[S, U](other: => Parsers[S])(f: (T, S) => U): Parsers[U] =
    for {
      t <- this
      s <- other
    } yield f(t, s)

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[S](other: => Parsers[S]): Parsers[(T, S)] = map2(other)((_, _))

  /**
    * Same as product
    */
  def **[S](other: => Parsers[S]): Parsers[(T, S)] = product(other)

  /**
    * It uses this to delete the corresponding portion of the input and
    * then parse what is left with other
    */
  def >>[S](other: Parsers[S]): Parsers[S] = **(other).map(_._2)

  /**
    * It uses this to parse and other to delete its corresponding portion of the input
    * from what is left after this
    */
  def <<[S](other: Parsers[S]): Parsers[T] = **(other).map(_._1)

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with this
    */
  def trimmed: Parsers[T] = Parsers.char(' ').many >> this << Parsers.char(' ')
}

object Parsers {

  /**
    * A parser that returns a string when fed to it.
    * Exercise 9.9
    */
  def string(s: String): Parser[String] = {
    val action = StateAction(
      (input: String) => input.zip(s).indexWhere{ case (c1, c2) => c1 != c2} match {
        case -1 if s.length <= input.length => // parsed successfully
          (Right(s), input.drop(s.length))
        case index => //input differs
          val offset = if (index == -1) input.length else index
          val error = (Location(input, offset), s"input differs from expected string $s")
          (Left(ParserError(List(error))), "")
      }
    )
    Parser(action)
  }

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    * Exercise 9.9
    */
  def regex(r: Regex): Parser[String] = {
    val action = StateAction(
      (input: String) =>
        {
          val firstMatch = r.findPrefixMatchOf(input)
          firstMatch match {
            case None =>
              val error = (Location(input, 0), s"input does not match regular expression $r")
              (Left(ParserError(List(error))), "")
            case Some(s) => (Right(s.toString), s.after.toString)
          }
        }
    )
    Parser(action)
  }

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string
    */
  def succeed[T](t: T): Parsers[T] = string("").map(_ => t)

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => Parsers[T]): Parsers[T] = p

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    *
    */
  def sequence[T](parsers: List[Parsers[T]]): Parsers[List[T]] =
    parsers.foldRight(succeed[List[T]](Nil)) {
      case (p, soFar) => p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char(c: Char): Parsers[Char] = string(c.toString).map(_.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar(c: Char): Parsers[Int] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1(c: Char): Parsers[Int] = char(c).many1.slice.map(_.length)

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
  def numFollowedByAsMany(c: Char): Parsers[Int] =
    for {
      digit <- regex("[0-9]".r)
      _ <- char(c).listOfN(digit.toInt)
    } yield digit.toInt

  implicit def stringToParser(s: String): Parsers[String] = string(s)

  implicit def regexToParser(r: Regex): Parsers[String] = regex(r)

  implicit def charToParser(c: Char): Parsers[Char] = char(c)

}


case class Parser[+T](parse: StateAction[String, Either[ParserError, T]]
                       ) extends Parsers[T] {

  def run(s: String): Either[ParserError, T] = parse.runAndGetValue(s)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[S](f: T => Parsers[S]): Parser[S] = Parser(
    parse.flatMap {
      case l @ Left(_) => StateAction.unit(l)
      case Right(t) => f(t).asInstanceOf[Parser[S]].parse
    }
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    * Exercise 9.9
    */
  def slice: Parser[String] = Parser(
    StateAction.getState.both(parse).both(StateAction.getState).map {
      case ((input, result), unParsed) =>
        result.right.map(_ => input.dropRight(unParsed.length))
    }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    * Exercise 9.9
    */
  private def modifyStack(
                           f: List[(Location, String)] => List[(Location, String)])
  : Parser[T] = Parser(
    parse.map(_.left.map(_.map(f)))
  )

  /**
    * Attaches a specified error message s to this
    * Exercise 9.9
    */
  def label(s: String): Parser[T] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: tail
  }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    * Exercise 9.9
    */
  def scope(s: String): Parser[T] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: Parsers[T] = ???

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    * Exercise 9.9
    */
  def latest: Parser[T] = modifyStack(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    * Exercise 9.9
    */
  def furthest: Parser[T] = modifyStack {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.9
    */
  def or[TT >: T](other: => Parsers[TT]): Parser[TT] = Parser(
    StateAction.getState.both(parse).flatMap {
      case (input, Left(_)) => StateAction.setState(input) >> other.asInstanceOf[Parser[TT]].parse
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

}
