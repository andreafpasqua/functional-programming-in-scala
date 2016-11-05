package andrea.scala.functional.programming.parsing2

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction
import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class MyParser[+T](
  action: StateAction[ParserState, Either[ParserError, T]]) extends Parsers[MyParser[T]] {

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[TT >: T, S](p: MyParser[T], f: T => MyParser[S]): MyParser[S] = MyParser(
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
  def slice[TT >: T](p: MyParser[TT]): MyParser[String] = MyParser(
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
  private def modifyStack(f: List[(Location, String)] => List[(Location, String)]) =
    MyParser(action.map(_.left.map(_.map(f))))

  /**
    * Attaches a specified error message s to this
    * Exercise 9.9
    */
  def label[TT >: T](p: MyParser[TT])(s: String): MyParser[TT] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: tail
  }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    * Exercise 9.9
    */
  def scope[TT >: T](p: MyParser[TT])(s: String): MyParser[TT] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt[TT >: T](p: MyParser[TT]): MyParser[TT] = ???

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    * Exercise 9.9
    */
  def latest[TT >: T](p: MyParser[TT]): MyParser[TT] = modifyStack(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    * Exercise 9.9
    */
  def furthest[TT >: T](p: MyParser[TT]): MyParser[TT] = modifyStack {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[TT >: T](p: => MyParser[TT]): MyParser[TT] = MyParser(p.action)

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.9
    */
  def or[TT >: T](p: MyParser[TT], other: => MyParser[TT]): MyParser[TT] = MyParser(
    StateAction.getState.both(action).flatMap {
      case (s, Left(_)) => StateAction.setState(s) >> other.action
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

}
