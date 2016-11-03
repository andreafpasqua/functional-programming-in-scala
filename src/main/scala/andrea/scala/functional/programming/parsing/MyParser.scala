package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction
import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class MyParser[+T](action: StateAction[ParserState, Either[ParserError, T]]) extends Parsers[T, MyParser[T]] {


  def run(s: ParserState): (Either[ParserError, T], ParserState) = action.run(s)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[S](f: T => Parsers[S, MyParser[S]]): MyParser[S] = MyParser(
    action.flatMap {
      case l @ Left(_) => StateAction.unit(l)
      case Right(t) => StateAction(f(t).run)
    }
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    * Exercise 9.9
    */
  def slice: MyParser[String] = MyParser(
    action.both(StateAction.getState).map {
      case (result, state) =>
        result.right.map(_ => state.location.parsed)
    }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    * Exercise 9.9
    */
  private def modifyStack(f: List[(Location, String)] => List[(Location, String)]) =
    MyParser(action.map(_.left.map(_.map(f))))

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
  def or[TT >: T](other: => Parsers[TT, MyParser[TT]]): MyParser[TT] = MyParser(
    StateAction.getState.both(action).flatMap {
      case (s, Left(_)) => StateAction.setState(s) >> StateAction(other.run)
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. This doesn't use this but we put it here
    * so it can be used in other
    */
  def succeed[TT >: T](t: TT): MyParser[TT] = MyParser.succeed(t)

}

object MyParser {

  /**
    * A parser that returns a string when fed to it.
    * Exercise 9.9
    */
  def string(s: String): MyParser[String] = {
    val action = StateAction(
      (state: ParserState) => {
        val loc = state.location
        val unParsed = loc.unParsed
        unParsed.zip(s).indexWhere { case (c1, c2) => c1 != c2 } match {
          case -1 if s.length <= unParsed.length => // parsed successfully
            (Right(s), state + s.length)
          case index => //input differs
            val errorLoc = if (index == -1) unParsed.length else index + s.length
            val error = (loc + errorLoc, s"input differs from expected string $s")
            (Left(ParserError(List(error))), state + errorLoc)
        }
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
      (state: ParserState) =>
      {
        val firstMatch = r.findPrefixMatchOf(state.location.unParsed)
        firstMatch match {
          case None =>
            val error = (state.location, s"input does not match regular expression $r")
            (Left(ParserError(List(error))), state)
          case Some(s) => (Right(s.toString), state + s.toString.length)
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
  def succeed[T](t: T): MyParser[T] = MyParser(StateAction((Right(t), _)))

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => Parsers[T, MyParser[T]]): MyParser[T] = MyParser(StateAction(p.run))

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    */
  def sequence[T](parsers: List[MyParser[T]]): MyParser[List[T]] =
    parsers.foldRight(succeed[List[T]](Nil)) {
      case (p, soFar) =>
        p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char(c: Char): MyParser[Char] = string(c.toString).map(_.charAt(0))

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar(c: Char): Parsers[Int, MyParser[Int]] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1(c: Char): Parsers[Int, MyParser[Int]] = char(c).many1.slice.map(_.length)

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