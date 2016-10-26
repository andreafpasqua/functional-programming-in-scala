package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.testing.{Prop, Sampler}
import andrea.scala.functional.programming.either.{Either, Left, Right}

import scala.util.matching.Regex

/**
  * Created by andreapasqua on 10/24/2016.
  */

case class Parser[E, +T](run: String => Either[E, T]) {

  def or[TT >: T](other: => Parser[E, TT]): Parser[E, TT] = Parser(
    string => run(string).left.orElse(other.run(string))
  )

  def |[TT >: T](other: Parser[E, TT]): Parser[E, TT] = or(other)

  /**
    * a parser that recognizes the same object as this and returns a list of
    * however many copies of that object are at the beginning of the input
    * string.
    * Exercise 9.3
    */
  def many: Parser[E, List[T]] = map2(many)(_ :: _) or Parser.succeed(Nil)

  /**
    * The same as many, but failing if there are no initial substrings of
    * the type this looks for
    * Exercise 9.1
    */
  def many1: Parser[E, List[T]] = map2(many) { case (t, list) => t :: list}

  /**
    * A parser that looks at the beginning of the input string
    * for n subsequent instances of the substring this is sensitive to.
    * Exercise 9.4
    */
  def listOfN(n: Int): Parser[E, List[T]] =
    if (n > 0) map2(listOfN(n - 1))(_ :: _) else Parser.succeed(Nil)


  /**
    * Parses the string with this and then maps the result (if any) with f
    * Exercise 9.8
    */
  def map[S](f: T => S): Parser[E, S] = flatMap(t => Parser.succeed(f(t)))

  /**
    * Parses the string with this and then parses what is left with other
    * and then combines the results using f
    * Exercise 9.7
    */
  def map2[S, U](other: => Parser[E, S])(f: (T, S) => U): Parser[E, U] =
    for {
      t <- this
      s <- other
    } yield f(t, s)

  /**
    * Parses a string and then uses a function f to obtain from the result
    * a new parser that is used to parse the remainder of the string
    */
  def flatMap[S](f: T => Parser[E, S]): Parser[E, S] = ???

  /**
    * Returns a parser for just the part of the input string that
    * this parsed successfully if any
    */
  def slice: Parser[E, String] = ???

  /**
    * Runs this on the input string and then other on what is left. Returns
    * both outputs as a tuple or a failure in all other cases.
    * Exercise 9.1
    */
  def product[S](other: => Parser[E, S]): Parser[E, (T, S)] = map2(other)((_, _))

  /**
    * Same as product
    */
  def **[S](other: => Parser[E, S]): Parser[E, (T, S)] = product(other)

  /**
    * It uses this to delete the corresponding portion of the input and
    * then parse what is left with other
    */
  def >[S](other: Parser[E, S]): Parser[E, S] = **(other).map(_._2)

  /**
    * It uses this to parse and other to delete its corresponding portion of the input
    * from what is left after this
    */
  def <[S](other: Parser[E, S]): Parser[E, T] = **(other).map(_._1)

  /**
    * Removes leading and trailing spaces from the string input before parsing it
    * with this
    */
  def trimmed: Parser[E, T] = Parser.char[E](' ').many > this < Parser.char[E](' ')
}

object Parser {

  trait ParseError

  /**
    * A parser that always fails with error e irrespectively of the
    * input string
    */
  def fail[E, T](e: E): Parser[E, T] = Parser(_ => Left(e))

  /**
    * a parser that always returns successfully the value t irrespectively of
    * the input string
    */
  def succeed[E, T](t: T): Parser[E, T] = string("").map(_ => t)

  /**
    * it combines a list of parser parsers into a parser that looks for the
    * target of the first parser at the beginning of the input list,
    * then the target of the second at the beginning of what is left
    * and so on. Finally it puts the outputs in a list that preserves
    * the order.
    *
    */
  def sequence[E, T](parsers: List[Parser[E, T]]): Parser[E, List[T]] =
    parsers.foldRight(succeed[E, List[T]](Nil)) {
      case (p, soFar) => p.map2(soFar){ case (t, list) => t :: list}
    }

  /**
    * A parser that returns a character when fed to it as a string
    */
  def char[E](c: Char): Parser[E, Char] = string(c.toString).map(_.charAt(0))

  /**
    * A parser that returns a string when fed to it.
    */
  def string[E](s: String): Parser[E, String] = ???

  /**
    * A parser that returns a string fed to it when it matches
    * the regular expression r
    *
    * @return
    */
  def regex[E](r: Regex): Parser[E, String] = ???

  /**
    * A parser that recognizes character c and counts how many times in a row it
    * is present at the beginning of the input string
    */
  def countChar[E](c: Char): Parser[E, Int] = char(c).many.slice.map(_.length)

  /**
    * Same as countChar, but it fails if there is not at least one instance of c
    */
  def countChar1[E](c: Char): Parser[E, Int] = char(c).many1.slice.map(_.length)

  /**
    * Counts how many times c1 occurs at the beginning of the string, and how many times
    * c2 occurs at the beginning of what is left. But it fails if the second count is zero.
    * In case of success it presents the two results as a tuple.
    */
  def countTwoChars[E](c1: Char, c2: Char) = countChar[E](c1) ** countChar1[E](c2)

  /**
    * Parses a single digit if it is followed by as many instances of the
    * character c
    */
  def numFollowedByAsMany[E](c: Char): Parser[E, Int] =
    for {
      digit <- regex[E]("[0-9]".r)
      _ <- char(c).listOfN(digit.toInt)
    } yield digit.toInt

  implicit def stringToParser(s: String): Parser[ParseError, String] = string[ParseError](s)

  implicit def regexToParser(r: Regex): Parser[ParseError, String] = regex[ParseError](r)

}

object ParserLaws {

  implicit val alphabet = "abcd".toVector

  /**
    * A utility to test that two parsers p1, p2 are equivalent, in the same that
    * they produce the same output on all strings.
    */
  def areEqual[E, T](p1: Parser[E, T], p2: Parser[E, T]): Prop =
    Sampler.string(100).forall(string => p1.run(string) == p2.run(string))

  /**
    * The proposition that a parser p is left unaltered when mapped with the identify
    */
  def mapLaw[E, T](p: Parser[E, T]): Prop = areEqual(p, p.map(identity))

  /**
    * A law that must be satisfied by string
    */
  def stringLaw: Prop =
    Sampler.string(100).forall(s => Parser.string(s).run(s).right.get == s) &&
      Sampler.listOfN(Sampler.string(100), 2).forall {
        case List(s1, s2) if s1 != s2 => Parser.string(s1).run(s2).isLeft
        case _ => true
      }

  /**
    * A law that succeeds must satisfy. Basically the same as for unit.
    */
  def succeedLaw[T](sampler: Sampler[T]): Prop = {
    val stringAndValue = for {
      s <- Sampler.string(100)
      t <- sampler
    } yield (s, t)
    stringAndValue.forall {
      case (s, t) => Parser.succeed(t).run(s) == t
    }
  }

  /**
    * Two law satisfied by the product. Associativity and commutativity with a vector function
    * and its projections.
    * Exercise 9.2
    */
  def productLaw[E, T1, T2, T3, S1, S2]
  (p1: Parser[E, T1], p2: Parser[E, T2], p3: Parser[E, T3])
  (f1: Sampler[T1 => S1], f2: Sampler[T2 => S2]): Prop =
    areEqual(
      ((p1 ** p2) ** p3).map { case ((t1, t2), t3) => (t1, t2, t3)},
      (p1 ** (p2 ** p3)).map { case (t1, (t2, t3)) => (t1, t2, t3)}
    ) && {
      val stringAndFunctionPair = for {
        string <- Sampler.string(100)
        func1 <- f1
        func2 <- f2
      } yield (string, func1, func2)
      stringAndFunctionPair.forall {
        case (s, f, g) =>
          val mapBefore = p1.map(f) ** p2.map(g)
          val mapAfter = (p1 ** p2).map { case (t1, t2) => (f(t1), g(t2)) }
          mapBefore.run(s) == mapAfter.run(s)
      }
    }

}


object JSONParser {

  import Parser._

  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jParser[P]: Parser[P, JSON] =
    jNull[P] | jNumber[P] | jString[P] | jBool[P] | jArray[P] | jObject[P]

  def jNull[P]: Parser[P, JSON] = string("null").map(_ => JNull)

  def jNumber[P]: Parser[P, JSON] = regex[P]("\d".r).map(d => JNumber(d.toDouble))

  def jString[P]: Parser[P, JSON] = (char[P]('"') ** regex[P]("""\w"&&[^"]""".r) ** char[P]('"')).map {
    case ((_, w), _) => JString(w)
  }

  def jBool[P]: Parser[P, JSON] = regex[P]("[true, false]".r)
    .map(s => if (s == "true") JBool(true) else JBool(false))

  def splitIn[P, T](p: Parser[P, T], c: Char = ','): Parser[P, List[T]] =
    (p ** ( char[P](c) > p).many).map {case (t, l) => t :: l}

  def jArray[P]: Parser[P, JSON] = {
    val jsonList = char[P]('[') > splitIn(jParser[P].trimmed) < char[P](']')
    jsonList.map(list => JArray(list.toVector))
  }

  def jObject[P]: Parser[P, JSON] = {
    val kVpair = jString[P].trimmed.slice ** (char[P](':') > jParser[P].trimmed)
    val jsonList = char[P]('{') > splitIn(kVpair) < char[P]('}')
    jsonList.map(list => JObject(list.toMap))
  }

}