package andrea.scala.functional.programming.parsing

/**
  * Created by andreapasqua on 10/27/2016.
  */


case class ParserError(stack: List[(Location, String)]) {

  /**
    * Modifies the stack by mapping it through f
    */
  def map(f: List[(Location, String)] => List[(Location, String)]): ParserError =
    ParserError(f(stack))

}

case class Location(input: String, offset: Int) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case n => offset - n
  }
  def unParsed: String = input.drop(offset - 1)
  def parsed: String = input.take(offset - 1)

  def +(n: Int) = copy(offset = offset + n)
}

case class ParserState(location: Location) {

  def +(n: Int) = ParserState(location + n)

}