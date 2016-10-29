package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.either.{Either, Left, Right}
import andrea.scala.functional.programming.state.StateAction

/**
  * Created by andrea on 10/28/16.
  */

case class ParserSA[+T](parse: StateAction[String, Either[ParserError, T]]
                       ) extends Parser[T] {

  def run(s: String): Either[ParserError, T] = parse.runAndGetValue(s)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => ParserSA[S]): ParserSA[S] = ParserSA(
    parse.flatMap {
      case l @ Left(_) => StateAction.unit(l)
      case Right(t) => f(t).parse
    }
  )

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: ParserSA[String] = ParserSA(
    StateAction.getState.both(parse).both(StateAction.getState).map {
        case ((input, result), unParsed) =>
          result.right.map(_ => input.dropRight(unParsed.length))
      }
  )

  /**
    * Auxiliary functions for modifying the error stack
    * of a parser remapping it with f
    */
  private def modifyStack(f: List[(Location, String)] => List[(Location, String)]): ParserSA[T] = ParserSA(
    parse.map(_.left.map(_.map(f)))
  )

  /**
    * Attaches a specified error message s to this
    */
  def label(s: String): ParserSA[T] = modifyStack {
      case (loc, msg) :: tail => (loc, s) :: tail
    }

  /**
    * Adds a specified error message s to to the stack of this without
    * erasing previous labels
    */
  def scope(s: String): ParserSA[T] = modifyStack {
    case (loc, msg) :: tail => (loc, s) :: (loc, msg) :: tail
  }

  /**
    * Marks this (and anything built from this without branching, e.g.
    * without or) as an attempt, meaning that if the parsing fails it
    * switches to the other branch without executing the remainder
    */
  def attempt: Parser[T] = ???

  /**
    * Reports only the error that occurred last,
    * i.e. the top of the stack
    */
  def latest: ParserSA[T] = modifyStack(_.take(1))

  /**
    * Returns the error with the furthest position,
    * i.e. the one with the largest position
    */
  def furthest: ParserSA[T] = modifyStack {
    stack => List(stack.maxBy {case (location, _) => location.offset})
  }

  /**
    * Construct a parser that acts like this but if this fails acts on
    * the same input like other
    */
  def or[TT >: T](other: => ParserSA[TT]): ParserSA[TT] = ParserSA(
    StateAction.getState.both(parse).flatMap {
      case (input, Left(_)) => StateAction.setState(input) >> other.parse
      case (_, t@Right(_)) => StateAction.unit(t)
    }
  )

}
