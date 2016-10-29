package andrea.scala.functional.programming.parsing

/**
  * Created by andreapasqua on 10/27/2016.
  */

/**
  * Exercise 9.9
  */
object JSONParser {

  import Parser._

  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jParser: Parser[JSON] =
    jNull | jNumber | jString | jBool | jArray | jObject

  def jNull: Parser[JSON] = string("null").map(_ => JNull)

  def jNumber: Parser[JSON] = "[0-9].".r.many.map(d => JNumber(d.reduce(_ + _).toDouble))

  def jString: Parser[JSON] = (char('"') ** """\w"&&[^"]""".r ** char('"')).map {
    case ((_, w), _) => JString(w)
  }

  def jBool: Parser[JSON] = "[true, false]".r.map(
    s => if (s == "true") JBool(true) else JBool(false))

  def splitIn[T](p: Parser[T], c: Char = ','): Parser[List[T]] =
    (p ** ( char(c) >> p).many).map {case (t, l) => t :: l}

  def jArray: Parser[JSON] = {
    val jsonList = '[' >> splitIn(jParser.trimmed) << ']'
    jsonList.map(list => JArray(list.toVector))
  }

  def jObject: Parser[JSON] = {
    val kVpair = jString.trimmed.slice ** (char(':') >> jParser.trimmed)
    val jsonList = '{' >> splitIn(kVpair) << '}'
    jsonList.map(list => JObject(list.toMap))
  }

}