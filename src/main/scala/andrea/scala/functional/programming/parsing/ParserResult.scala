package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.parsing.ParserResult.Stack


/**
  * Created by andreapasqua on 10/27/2016.
  */

sealed trait ParserResult[+T] {
  def isSuccess: Boolean
  def isError: Boolean
  def mapError(f: ParserError => ParserError): ParserResult[T] = this match {
    case e @ParserError(_) => f(e)
    case _ => this
  }
  def getResult: T = this match {
    case Success(t) => t
    case _ => throw new NoSuchElementException("getResult called on ParserError")
  }

  def getStack: Stack = this match {
    case ParserError(stack) => stack
    case _ => throw new NoSuchElementException("getStack called on Success")
  }
}

case class Success[+T](result: T) extends ParserResult[T]{
  def isSuccess: Boolean = true
  def isError: Boolean = false
}

case class ParserError(stack: Stack) extends ParserResult[Nothing]{
  def isSuccess: Boolean = false
  def isError: Boolean = true

  /**
    * Replaces the top error message with msg. If the stack is empty,
    * it leaves it empty
    */
  def changeTopMessage(msg: String): ParserError = ParserError(
    stack match {
        case Nil => Nil
        case (loc, _) :: rest => (loc, msg) :: rest
      }
  )

  /**
    * Adds a new error at the top of the stack,
    * consisting of the same location as the top error but with new
    * error message msg. If the stack is empty, it leaves it empty
    */
  def push(msg: String): ParserError = ParserError(
    stack match {
      case Nil => Nil
      case (loc, str) :: rest => (loc, msg) :: (loc,str) :: rest
    }
  )

}

case class Location(input: String, offset: Int) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case n => offset - n
  }
  def unParsed: String = input.drop(offset)
  def parsed: String = input.take(offset)

  def +(n: Int) = copy(offset = offset + n)
}

object ParserResult {
  type Stack = List[(Location, String)]

  def error(loc: Location, msg: String): ParserResult[Nothing] =
    ParserError(List((loc, msg)))

  def success[T](t: T): ParserResult[T] = Success(t)
}


case class ParserState(location: Location, isCommitted: Boolean = true) {

  def +(n: Int): ParserState = this.copy(location = location + n)

  def commit: ParserState = this.copy(isCommitted = true)
  def unCommit: ParserState = this.copy(isCommitted = false)

}