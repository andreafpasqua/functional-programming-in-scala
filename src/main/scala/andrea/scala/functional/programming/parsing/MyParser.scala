package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction
import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class MyParser[+T](parse: StateAction[Location, Either[ParserError, T]]) extends Parsers[T, MyParser[T]] {

  def run(s: Location): Either[ParserError, T] = parse.runAndGetValue(s)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[S](f: T => Parsers[S, MyParser[S]]): MyParser[S] = MyParser(
    parse.flatMap {
      case l @ Left(_) => StateAction.unit(l)
      case Right(t) => f(t).parse
    }
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    * Exercise 9.9
    */
  def slice: MyParser[String] = MyParser(
    StateAction.getState.both(parse).both(StateAction.getState).map {
      case ((_, result), loc) =>
        result.right.map(_ => loc.parsed)
    }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    * Exercise 9.9
    */
  private def modifyStack(f: List[(Location, String)] => List[(Location, String)]) =
    MyParser(parse.map(_.left.map(_.map(f))))

  /**
    * Attaches a specified error message s to this
    * Exercise 9.9
    */
  def label(s: String): MyParser[T] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: tail
  }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    * Exercise 9.9
    */
  def scope(s: String): MyParser[T] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: MyParser[T] = ???

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    * Exercise 9.9
    */
  def latest: MyParser[T] = modifyStack(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    * Exercise 9.9
    */
  def furthest: MyParser[T] = modifyStack {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.9
    */
  def or[TT >: T](other: => MyParser[TT]): MyParser[TT] = MyParser(
    StateAction.getState.both(parse).flatMap {
      case (loc, Left(_)) => StateAction.setState(loc) >> other.parse
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

}

object MyParser {

  /**
    * A parser that returns a string when fed to it.
    * Exercise 9.9
    */
  def string(s: String): MyParser[String] = {
    val action = StateAction(
      (loc: Location) => loc.unParsed.zip(s).indexWhere{ case (c1, c2) => c1 != c2} match {
        case -1 if s.length <= loc.unParsed.length => // parsed successfully
          (Right(s), loc.copy(offset = loc.offset + s.length))
        case index => //input differs
          val offset = if (index == -1) loc.input.length else index + s.length
          val newLoc = loc.copy(offset = offset)
          val error = (newLoc, s"input differs from expected string $s")
          (Left(ParserError(List(error))), newLoc)
      }
    )
    MyParser(action)
  }

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    * Exercise 9.9
    */
  def regex(r: Regex): MyParser[String] = {
    val action = StateAction(
      (loc: Location) =>
      {
        val firstMatch = r.findPrefixMatchOf(loc.unParsed)
        firstMatch match {
          case None =>
            val error = (loc, s"input does not match regular expression $r")
            (Left(ParserError(List(error))), loc)
          case Some(s) => (Right(s.toString), loc.copy(offset = loc.offset + s.toString.length))
        }
      }
    )
    MyParser(action)
  }

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. Note you cannot implement it using map if map uses it
    * (through flatMap)
    */
  def succeed[T](t: T): MyParser[T] = MyParser(StateAction(input => (Right(t), input)))

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => MyParser[T]): MyParser[T] = {
    val action = StateAction((input: String) => p.parse.run(input))
    MyParser(action)
  }

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    */
  def sequence[T](parsers: List[MyParser[T]]): MyParser[List[T]] =
    parsers.foldRight(succeed[List[T]](Nil)) {
      case (p, soFar) => p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char(c: Char): MyParser[Char] = string(c.toString).map(_.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar(c: Char): MyParser[Int] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1(c: Char): MyParser[Int] = char(c).many1.slice.map(_.length)

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
  def numFollowedByAsMany(c: Char): MyParser[Int] =
    for {
      digit <- regex("[0-9]".r)
      _ <- char(c).listOfN(digit.toInt)
    } yield digit.toInt

  implicit def stringToParser(s: String): MyParser[String] = string(s)

  implicit def regexToParser(r: Regex): MyParser[String] = regex(r)

  implicit def charToParser(c: Char): MyParser[Char] = char(c)

}