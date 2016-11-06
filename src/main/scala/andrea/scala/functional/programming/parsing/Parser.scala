package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction
import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class Parser[+T](action: StateAction[ParserState, Either[ParserError, T]]) {

  def run(s: String): Either[ParserError, T] = {
    val input = ParserState(Location(s, 0))
    action.runAndGetValue(input)
  }
}

object Parser extends Parsers[Parser] {

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[T, S](p: Parser[T], f: T => Parser[S]): Parser[S] = Parser(
    p.action.flatMap {
      case l @ Left(_) => StateAction.unit(l)
      case Right(t) => f(t).action
    }
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    * Exercise 9.9
    */
  def slice[T](p: Parser[T]): Parser[String] = Parser(
    p.action.both(StateAction.getState).map {
      case (result, state) =>
        result.right.map(_ => state.location.parsed)
    }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    * Exercise 9.9
    */
  private def modifyStack[T](p: Parser[T])
                            (f: List[(Location, String)] => List[(Location, String)])
  : Parser[T] = Parser(p.action.map(_.left.map(_.map(f))))

  /**
    * Attaches a specified error message s to this
    * Exercise 9.9
    */
  def label[T](p: Parser[T])(s: String): Parser[T] = modifyStack(p) {
    case (loc, msg) :: tail => (loc, s) :: tail
  }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    * Exercise 9.9
    */
  def scope[T](p: Parser[T])(s: String): Parser[T] = modifyStack(p) {
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt[T](p: Parser[T]): Parser[T] = ???

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    * Exercise 9.9
    */
  def latest[T](p: Parser[T]): Parser[T] = modifyStack(p)(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    * Exercise 9.9
    */
  def furthest[T](p: Parser[T]): Parser[T] = modifyStack(p) {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.9
    */
  def or[T](p: Parser[T], other: => Parser[T]): Parser[T] = Parser(
    StateAction.getState.both(p.action).flatMap {
      case (s, Left(_)) => StateAction.setState(s) >> other.action
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => Parser[T]): Parser[T] = Parser(p.action)

  /**
    * A parser that returns a string when fed to it.
    * Exercise 9.9
    */
  def string(s: String): Parser[String] = {
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
    Parser(action)
  }

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    * Exercise 9.9
    */
  def regex(r: Regex): Parser[String] = {
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
    Parser(action)
  }

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. Note you cannot implement it using map if map uses it
    * (through flatMap)
    */
  def succeed[T](t: T): Parser[T] = Parser(StateAction((Right(t), _)))

}
