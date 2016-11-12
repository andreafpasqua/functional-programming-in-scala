package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.parsing.ParserResult.Stack
import andrea.scala.functional.programming.either.{Either, Left, Right}


/**
  * Created by andreapasqua on 10/27/2016.
  */

case class ParserResult[+T](result: Either[ParserError, T]) {

  /**
    * Returns a new Result by mapping the error stack (if any) using f
    */
  def mapError(f: Stack => Stack): ParserResult[T] =
    ParserResult(result.left.map(e => ParserError(f(e.stack))))

  /**
    * Returns a new Result by mapping the value (if any) using f
    */
  def mapValue[S](f: T => S): ParserResult[S] =
    ParserResult(result.right.map(f))

  /**
    * Adds a new error at the top of the stack,
    * consisting of the same location as the top error but with new
    * error message msg
    */
  def push(msg: String): ParserResult[T] = mapError {
    case Nil => Nil
    case (loc, s) :: list => (loc, msg) :: (loc, s) :: list
  }

  def changeTopMessage(msg: String): ParserResult[T] = mapError {
    case Nil => Nil
    case (loc, _) :: list => (loc, msg) :: list
  }

  def isSuccess = result.isRight
  def isFailure = result.isLeft

  

}

case class ParserError(stack: Stack)

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

  def error[T](loc: Location, msg: String) =
    ParserResult[T](Left(ParserError(List((loc, msg)))))

  def result[T](t: T) = ParserResult(Right(t))
}


case class ParserState(location: Location, isCommitted: Boolean = true) {

  def +(n: Int) = this.copy(location = location + n)

}