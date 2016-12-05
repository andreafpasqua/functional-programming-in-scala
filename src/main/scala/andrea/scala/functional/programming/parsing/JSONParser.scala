package andrea.scala.functional.programming.parsing

/**
  * Created by andreapasqua on 10/27/2016.
  */

/**
  * Exercise 9.9
  */
object JSONParser {

  import andrea.scala.functional.programming.parsing.Parser._


  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // you need to use the constructor because strings have another implicit map
  val jNull: Parser[JSON] = string("null").map(_ => JNull)

  val jNumber: Parser[JNumber] =
    regex("""^[+-]?(\d*\.)?\d+""".r).map(d => JNumber(d.toDouble))

  val jString: Parser[JString] = ((char('"') >* """^[a-zA-Z0-9_\s]*""".r) *< '"').map(s => JString(s))

  val jBool: Parser[JBool] = (attempt(string("true")) | string("false")).map(
    s => if (s == "true") JBool(true) else JBool(false)
  )

  val jArray: Parser[JArray] = {
    val jsonList = char('[') >* splitIn(trim(jParser)) *< ']'
    jsonList.map(list => JArray(list.toVector))
  }

  val jObject: Parser[JObject] = {
    val kVPair = trim(jString).map{case JString(s) => s} ** (char(':') >* trim(jParser))
    val jsonList = char('{') >* splitIn(kVPair) *< '}'
    jsonList.map(list => JObject(list.toMap))
  }

  /**
    * Splits a c separated sequence of string that can be parsed by p into a list of parsed objects
    */
  def splitIn[T](p: Parser[T], c: Char = ','): Parser[List[T]] =
    (p ** many[T]( char(c) >* p)).map {case (t, l) => t :: l}

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with p
    */
  def trim[T](p: Parser[T]): Parser[T] = many(" ") >* p *< many(" ")


  def jParser: Parser[JSON] = attempt(jNull) | attempt(jNumber) | attempt(jString) |
    attempt(jBool) | attempt(jArray) | jObject

  val topParser: Parser[JSON] = topLevel(jParser)

}