package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.parsing.ParserResult.Stack
import andrea.scala.functional.programming.state.StateAction

import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class Parser[+T](action: StateAction[ParserState, ParserResult[T]]) {

  /**
    * Given a string s, it uses it as an input to the parser. Note
    * that the state is set as committed by default.
    */
  def run(s: String): ParserResult[T] = {
    val input = ParserState(Location(s, 0), isCommitted=true)
    action.runAndGetValue(input)
  }

}

/**
  * An implementation of Parsers
  * Exercise 9.12
  */
object Parser extends Parsers[Parser] {

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    * Exercise 9.9
    */
  def flatMap[T, S](p: Parser[T], f: T => Parser[S]): Parser[S] = Parser(
    p.action.flatMap(
      _.get match {
        case l @ Left(_) => StateAction.unit(ParserResult(l))
        case Right(t) => f(t).action
      }
    )
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    * Exercise 9.13
    */
  def slice[T](p: Parser[T]): Parser[String] = Parser(
    p.action.both(StateAction.getState).map {
      case (result, state) =>
        result.mapValue(_ => state.location.parsed)
    }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    * Exercise 9.10
    */
  private def modifyStack[T](p: Parser[T])(f: Stack => Stack): Parser[T] =
    Parser(p.action.map(_.mapError(f)))

  /**
    * Attaches a specified error message s to this
    * Exercise 9.10
    */
  def label[T](s: String)(p: Parser[T]): Parser[T] = modifyStack(p) {
    case Nil => Nil
    case (loc, msg) :: tail => (loc, s) :: tail
  }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    * Exercise 9.10
    */
  def scope[T](s: String)(p: Parser[T]): Parser[T] =
    p.action.flatMap(result => result.push()

    modifyStack(p) {
    case Nil => Nil
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * When branching occurs，i.e when there is an or, it marks p
    * as an attempt meaning that if the parsing fails it switches
    * to the other branch, otherwise it returns a result. Anything built
    * from p without branching, inherits the attempt.
    * Exercise 9.10
    */
  def attempt[T](p: Parser[T]): Parser[T] = {
    val action =
      StateAction.modifyState[ParserState](state => state.copy(isCommitted = false)) >*
        p.action
    Parser(action)
  }

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    * Exercise 9.11
    */
  def latest[T](p: Parser[T]): Parser[T] = modifyStack(p)(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    * Exercise 9.11
    */
  def furthest[T](p: Parser[T]): Parser[T] = modifyStack(p) {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.10
    */
  def or[T](p: Parser[T], other: => Parser[T]): Parser[T] = Parser(
    (StateAction.getState ** p.action ** StateAction.getState).flatMap {
      case ((before, ParserResult(Left(_))), after) if !after.isCommitted => // left branch failed uncommitted
        StateAction.setState(before) >* other.action
      case ((_, result), _) => // left branch succeeded or failed committed
        StateAction.unit(result)
    }
  )

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    * Exercise 9.9
    */
  def orUncommitted[T](p: Parser[T], other: => Parser[T]): Parser[T] = Parser(
    StateAction.getState.both(p.action).flatMap {
      case (s, res) if res.isSuccess => StateAction.setState(s) >* other.action
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

  /**
    * Delays evaluation of the parser p
    * Exercise 9.5
    */
  def delay[T](p: => Parser[T]): Parser[T] = Parser(
    StateAction(
      (state: ParserState) => p.action.run(state)
    )
  )

  /**
    * A parser that returns a string when fed to it.
    * Exercise 9.13
    */
  def string(s: String): Parser[String] = {
    val action = StateAction(
      (state: ParserState) => {
        val loc = state.location
        val unParsed = loc.unParsed
        unParsed.zip(s).indexWhere { case (c1, c2) => c1 != c2 } match {
          case -1 if s.length <= unParsed.length => // parsed successfully
            (ParserResult.result(s), state + s.length)
          case index => //input differs
            val errorLoc = if (index == -1) unParsed.length else index
            (ParserResult.error(loc + errorLoc,
              s"input differs from expected string $s")
              , state + errorLoc)
        }
      }
    )
    Parser(action)
  }

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    * Exercise 9.13
    */
  def regex(r: Regex): Parser[String] = {
    val action = StateAction(
      (state: ParserState) =>
      {
        val firstMatch = r.findPrefixMatchOf(state.location.unParsed)
        firstMatch match {
          case None =>
            (ParserResult.error(state.location, s"input does not match regular expression $r"),
              state)
          case Some(s) => (ParserResult.result(s.toString),
            state + s.toString.length)
        }
      }
    )
    Parser(action)
  }

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string. Note you cannot implement it using map if map uses it
    * (through flatMap)
    * Exercise 9.13
    */
  def succeed[T](t: T): Parser[T] = Parser(StateAction.unit(ParserResult.result(t)))

}
