package andrea.scala.functional.programming.parsing

/**
  * Created by andreapasqua on 10/27/2016.
  */

/**
  * Exercise 9.9
  */
object JSONParser {

  import MyParser._

  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jParser: MyParser[JSON] =
    jNull | jNumber | jString | jBool | jArray | jObject

  def jNull: MyParser[JSON] = string("null").map(_ => JNull)

  def jNumber: MyParser[JSON] = "[0-9].".r.many.map(d => JNumber(d.reduce(_ + _).toDouble))

  def jString: MyParser[JSON] = (char('"') ** """\w"&&[^"]""".r ** char('"')).map {
    case ((_, w), _) => JString(w)
  }

  def jBool: MyParser[JSON] = "[true, false]".r.map(
    s => if (s == "true") JBool(true) else JBool(false))

  def splitIn[T](p: MyParser[T], c: Char = ','): MyParser[List[T]] =
    (p ** ( char(c) >> p).many).map {case (t, l) => t :: l}

  def jArray: MyParser[JSON] = {
    val jsonList = '[' >> splitIn(jParser.trimmed) << ']'
    jsonList.map(list => JArray(list.toVector))
  }

  def jObject: MyParser[JSON] = {
    val kVpair = jString.trimmed.slice ** (char(':') >> jParser.trimmed)
    val jsonList = '{' >> splitIn(kVpair) << '}'
    jsonList.map(list => JObject(list.toMap))
  }


  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with this
    */
  def trimmed[TT >: T]: Parsers[TT, MyParser[TT]] = MyParser.char(' ').many >>
    this.asInstanceOf[Parsers[TT, MyParser[TT]]] << MyParser.char(' ').many

}