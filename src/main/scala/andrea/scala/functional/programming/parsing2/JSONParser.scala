package andrea.scala.functional.programming.parsing2

import andrea.scala.functional.programming.either.{Either, Right}
import andrea.scala.functional.programming.state.StateAction

/**
  * Created by andreapasqua on 10/27/2016.
  */

/**
  * Exercise 9.9
  */
object JSONParser {

  /**
    * Notice that you are treating MyParser both as a templated object and as the main type.
    * To treat it as a templated object you have to instantiate it once with a dummy
    * action and then imports everything from it, including the implicits. This is the role of P,
    * and that is why you name it with a capital letter
    */
  val P = MyParser(StateAction.unit[ParserState, Either[ParserError, Unit]](Right()))
  import P._


  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  val jParser: MyParser[JSON] =
    jNull | jNumber | jString | jBool | jArray | jObject

  val jNull: MyParser[JSON] = string("null").map((s: String) => JNull)

  val jNumber: MyParser[JSON] = many("[0-9].".r).map((d: List[String]) => JNumber(d.reduce(_ + _).toDouble))

  val jString: MyParser[JSON] = (char('"') >> """\w"&&[^"]""".r << char('"')).map((s: String) => JString(s))

  val jBool: MyParser[JSON] = regex("[true, false]".r).map(
    (s: String) => if (s == "true") JBool(true) else JBool(false)
  )

  /**
    * Splits a c separated sequence of string that can be parsed by p into a list of parsed objects
    */
  def splitIn[T](p: MyParser[T], c: Char = ','): MyParser[List[T]] =
    (p ** many( char(c) >> p)).map((x: (T, List[T])) => x._1 :: x._2)

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with p
    */
  def trim[T](p: MyParser[T]): MyParser[T] = many(" ") >> p << many(" ")

  val jArray: MyParser[JSON] = {
    val jsonList = char('[') >> splitIn(trim(jParser)) << ']'
    jsonList.map((list: List[JSON]) => JArray(list.toVector))
  }

  val jObject: MyParser[JSON] = {
    val kVPair = trim(jString).slice ** (char(':') >> trim(jParser))
    val jsonList = char('{') >> splitIn(kVPair) << '}'
    jsonList.map((list: List[(String, JSON)]) => JObject(list.toMap))
  }

}