package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.testing.{Prop, Sampler}

/**
  * Created by andrea on 10/25/16.
  */

object ParserTest extends App {

  import andrea.scala.functional.programming.parsing.Parser._

  println("* Test Location line, column, parsed and unparsed")
  val locInput = "ciao ciao\n ciao"
  val loc = Location(locInput, 0)
  val loc1 = Location(locInput, 9)
  val loc2 = Location(locInput, 11)
  assert(loc.line == 1 && loc.column == 1)
  assert(loc1.line == 2 && loc1.column == 0)
  assert(loc2.line == 2 && loc2.column == 2)
  assert(loc.parsed == "" && loc.unParsed == locInput)
  assert(loc1.parsed == "ciao ciao" && loc1.unParsed == "\n ciao")
  assert(loc2.parsed == "ciao ciao\n " && loc2.unParsed == "ciao")

  println("* Test Location +")
  assert(loc1 + 2 == loc2)
  assert(loc + 11 == loc2)

  println("* Test string")
  assert("ciao ciao".run("ciao ciao bella").getResult == "ciao ciao")
  assert("ciao ciao".run("cia").getStack.head._1.offset == 3)
  assert("ciao ciao".run("ciao cIao").getStack.head._1.offset == 6)
  val offsetZero = ParserState(Location("ciao ciao", 0))
  val offsetOne = ParserState(Location(" ciao ciao", 1))
  assert(string("ciao ciao").action.runAndGetState(offsetZero).location.offset == 9)
  assert(string("ciao ciao").action.runAndGetState(offsetOne).location.offset == 10)

  println("* Test regex")
  assert("""[a-d][io]g""".r.run("dog").getResult == "dog")
  assert("""[a-d][io]g""".r.run("doc").getStack.head._1.offset == 0)
  assert(regex("ciao ciao".r).action.runAndGetState(offsetZero).location.offset == 9)
  assert(regex("ciao ciao".r).action.runAndGetState(offsetOne).location.offset == 10)

  println("* Test succeed")
  assert(succeed(10).run("").getResult == 10)
  assert(succeed(List(1, 2)).run("ciao").getResult == List(1, 2))

  println("* Test delay")
  lazy val thunk = {println("\tParser evaluated"); succeed(10)}
  println("\tInstantiating delayed parser prints nothing")
  println("\tInstantiating...")
  val delayedSucceed = delay(thunk)
  println("\tNothing printed")
  println("\tNow running it")
  assert(delayedSucceed.run("ciao").getResult == 10)

  println("* Test flatMap")
  def ifZero(c: Char): Parser[String]=
    if (c == '0') string("isZero") else string("isNotZero")
  assert(char('0').flatMap(ifZero).run("0isZero").getResult == "isZero")
  assert(char('1').flatMap(ifZero).run("1isNotZero").getResult == "isNotZero")
  assert(char('0').flatMap(ifZero).run("0isNotZero").getStack.head._1.offset == 3)
  assert(char('1').flatMap(ifZero).run("1isZero").getStack.head._1.offset == 3)

  println("* Test slice")
  assert(string("abc").map(_.length).run("abcdef").getResult ==3)
  assert(string("abc").map(_.length).slice.run("abcdef").getResult == "abc")
  assert(regex("""[a-d][io]g""".r).slice.run("dog and cat").getResult == "dog")
  assert(string("abc").map(_.length).slice.run("abCdef").getStack.head._1.offset == 2)
  assert(product(string("abc"), slice(string("def"))).run("abcdef").getResult == ("abc", "def"))

  println("* Test label")
  assert {
    val relabeledError = label("new label")("abc").run("aBc")
    relabeledError.getStack.length == 1 &&
      relabeledError.getStack.head._2 == "new label"
  }

  println("* Test scope")
  assert {
    val scopedError =
      scope("scope message")(label("new label")("abc")).run("aBc")
    scopedError.getStack.length == 2 &&
      scopedError.getStack.head._2 == "scope message" &&
      scopedError.getStack.tail.head._2 == "new label"
  }
  val scopedParser = scope("scope message")(
    label("first label")(string("ciao")) **
      label("second label")(string(" ciao"))
  )
  assert {
    val error1 = scopedParser.run("cIao ciao")
    val error2 = scopedParser.run("ciao cIao")
    error1.getStack.head._2 == "scope message" &&
      error1.getStack(1)._2 == "first label" &&
      error2.getStack.head._2 == "scope message" &&
      error2.getStack(1)._2 == "second label"
  }

  println("* Test attempt")
  val committed = string("ciao")
  val uncommitted = attempt(string("ciao"))
  val uncommitted2 = attempt(string("ciao")) ** string(" ciao")
  val initialState = ParserState(Location("ciao", 0), isCommitted = true)
  assert(committed.action.runAndGetState(initialState).isCommitted)
  assert(!uncommitted.action.runAndGetState(initialState).isCommitted)
  assert(!uncommitted2.action.runAndGetState(initialState).isCommitted)

  println("* Test or")
  val committedOr = (string("ciao ") >* "ciao") | "hello"
  val uncommittedOr = (attempt("ciao ") >* "ciao") | "hello"
  assert(committedOr.run("ciao ciao").getResult == "ciao")
  assert(committedOr.run("hello").isError)
  assert(committedOr.run("cia0 ciao").isError)
  assert(uncommittedOr.run("ciao ciao").getResult == "ciao")
  assert(uncommittedOr.run("hello").getResult == "hello")
  assert(uncommittedOr.run("cia0 ciao").isError)

  println("* Test latest")
  assert(latest(uncommittedOr).run("cia0 ciao").getStack == uncommittedOr.run("cia0 ciao").getStack.take(1))
  assert(latest(committedOr).run("cia0 ciao").getStack == uncommittedOr.run("cia0 ciao").getStack.take(1))

  println("* Test furthest")
  assert(furthest(uncommittedOr).run("cia0 ciao").getStack.head._1.offset == 3)

  println("* Test many, many1, listOfN")
  assert(many(char('c')).run("dddd").getResult.isEmpty)
  assert(many(char('c')).run("ccd").getResult == List('c', 'c'))
  assert(many1(char('c')).run("cddd").getResult == List('c'))
  assert(many1(char('c')).run("dddd").getStack.head._1.offset == 0)
  assert(listOfN(char('c'), 3).run("cccddd").getResult == List('c', 'c', 'c'))
  assert(listOfN(char('c'), 3).run("ccddd").getStack.head._1.offset == 2)

  println("* Test map")
  assert(string("ciao").map(_.length).run("ciao ciao").getResult == 4)
  assert(string("ciao").map(_.length).run("cia0 ciao").getStack.head._1.offset == 3)

  println("* Test map2")
  val map2Parser = map2(string("ciao"), string(" ciao"))(_.length + _.length)
  assert(map2Parser.run("ciao ciao").getResult == 9)
  assert(map2Parser.run("cia0 ciao").getStack.head._1.offset == 3)
  assert(map2Parser.run("ciao cia0").getStack.head._1.offset == 8)

  println("* Test product")
  val productParser = product(string("ciao"), string(" ciao"))
  assert(productParser.run("ciao ciao").getResult == ("ciao", " ciao"))
  assert(productParser.run("cia0 ciao").getStack.head._1.offset == 3)
  assert(productParser.run("ciao cia0").getStack.head._1.offset == 8)
  assert(productParser.run(" ciaociao").getStack.head._1.offset == 0)

  println("* Test sequence")
  val sequenceParser = sequence(List[Parser[String]]("ciao", " ", "5", " ", "ciao"))
  assert(sequenceParser.run("ciao 5 ciao").getResult == List("ciao", " ", "5", " ", "ciao"))
  assert(sequenceParser.run("ciao 6 ciao").getStack.head._1.offset == 5)

  println("* Test char")
  assert(char('c').run("dddd").getStack.head._1.offset == 0)
  assert(char('c').run("ccd").getResult == 'c')

  println("* Test countChar, countChar1, countTwoChars")
  assert(countChar('c').run("ddddc").getResult == 0)
  assert(countChar1('c').run("ddddc").getStack.head._1.offset == 0)
  assert(countChar('c').run("cccd").getResult == 3)
  assert(countChar1('c').run("cccd").getResult == 3)
  val twoCharsParser = countTwoChars('c', 'd')
  assert(twoCharsParser.run("dddcc").getResult == (0, 3))
  assert(twoCharsParser.run("cccdd").getResult == (3, 2))
  assert(twoCharsParser.run("eeefffcc").getStack.head._1.offset == 0)

  println("** Test JSONParser")
  println("\tOne problem jArray allows different types of elements in the same array")
  import JSONParser._
  println("* Test jNull")
  assert(jNull.run("null").getResult == JNull)

  println("* Test jNumber")
  assert(jNumber.run("103").getResult == JNumber(103D))
  assert(jNumber.run("-103").getResult == JNumber(-103D))
  assert(jNumber.run("1.03").getResult == JNumber(1.03))
  assert(jNumber.run("-1.03").getResult == JNumber(-1.03))
  assert(jNumber.run("0.03").getResult == JNumber(0.03))
  assert(jNumber.run("-0.03").getResult == JNumber(-0.03))
  assert(jNumber.run("-001.03").getResult == JNumber(-1.03))
  println("\tNotice that jNumber will take -001.0.3, but topParser will not")
  assert(jNumber.run("-001.0.3").getResult == JNumber(-1.0))
  assert(jNumber.run("1.0-3").getResult == JNumber(1.0))

  println("* Test jString")
  assert(jString.run("\"ciao\"").getResult == JString("ciao"))
  assert(jString.run("\"ciao").getStack.head._1.offset == 5)
  assert(jString.run("ciao\"").getStack.head._1.offset == 0)
  assert(jString.run("ciao").getStack.head._1.offset == 0)
  assert(jString.run("\"ciao ciao\"").getResult == JString("ciao ciao"))
  assert(jString.run("\"ciao_ciao2\"").getResult == JString("ciao_ciao2"))

  println("* Test jBool")
  assert(jBool.run("true").getResult == JBool(true))
  assert(jBool.run("false").getResult == JBool(false))
  assert(jBool.run("True").getStack.head._1.offset == 0)
  assert(jBool.run(""""true"""").getStack.head._1.offset == 0)

  println("* Test jArray")
  assert(jArray.run("[1.2]").getResult == JArray(Vector(JNumber(1.2))))
  assert(jArray.run("[true,  false,null, false]").getResult ==
    JArray(Vector(JBool(true), JBool(false), JNull, JBool(false))))
  assert(jArray.run("[true, false, false").isError)
  assert(jArray.run("[1, 2 ,  null,  3]").getResult ==
    JArray(Vector(JNumber(1.0), JNumber(2.0), JNull, JNumber(3.0))))
  println("\tNotice that jArray will accept lists of mixed compositions and so does topParser")
  assert(jArray.run("[1, 2 ,  null,  3, false]").getResult ==
    JArray(Vector(JNumber(1.0), JNumber(2.0), JNull, JNumber(3.0), JBool(false))))
  assert(jArray.run("""[ "ciao1",  null,"ciao2","ciao3"]""").getResult ==
    JArray(Vector(JString("ciao1"), JNull, JString("ciao2"), JString("ciao3"))))
  assert(jArray.run("""[ [1, 2, 3], [3, 4, null, 5], null]""").getResult ==
    JArray(Vector(
      JArray(Vector(JNumber(1.0), JNumber(2.0), JNumber(3.0))),
      JArray(Vector(JNumber(3.0), JNumber(4.0), JNull, JNumber(5.0))),
      JNull)))
  assert(jArray.run("""[ [1, 2, 3], ["ciao", "ciao2", null, "ciao3"], null]""").getResult ==
    JArray(Vector(JArray(Vector(JNumber(1.0), JNumber(2.0), JNumber(3.0))),
      JArray(Vector(JString("ciao"), JString("ciao2"), JNull, JString("ciao3"))), JNull)))

  println("* Test jObject")
  assert(jObject.run("""{}""").getStack.head._1.offset == 1)
  assert(jObject.run("""{field1}""").getStack.head._1.offset == 1)
  assert(jObject.run("""{"field1"}""").getStack.head._1.offset == 9)
  assert(jObject.run("""{"field1" : }""").isError)
  assert(jObject.run("""{"field1"  : 1""").getStack.head._1.offset == 14)
  assert(jObject.run("""{"field1"  : 1}""").getResult == JObject(Map("field1" -> JNumber(1.0))))
  assert(jObject.run("""{"field1"  : 1, "field2":   "ciao"}""").getResult ==
    JObject(Map("field1" -> JNumber(1.0), "field2" -> JString("ciao"))))
  assert(jObject.run("""{"field1"  : 1, "field2":   {"field3" : null, "field4": [true, false]}}""").getResult ==
    JObject(
      Map("field1" -> JNumber(1.0),
        "field2" -> JObject(
          Map("field3" -> JNull, "field4" -> JArray(Vector(JBool(true), JBool(false))))))))
  assert(
    jArray.run("""[{"field1"  : 1, "field2":  "ciao"}, {"field3" : null, "field4": [true, false]}]""").getResult ==
      JArray(Vector(
        JObject(Map("field1" -> JNumber(1.0), "field2" -> JString("ciao"))),
        JObject(Map("field3" -> JNull, "field4" -> JArray(Vector(JBool(true), JBool(false))))))))

  println("* Test topParser")
  assert(topParser.run("null").getResult == JNull)
  assert(topParser.run("103").getResult == JNumber(103D))
  assert(topParser.run("-103").getResult == JNumber(-103D))
  assert(topParser.run("1.03").getResult == JNumber(1.03))
  assert(topParser.run("-1.03").getResult == JNumber(-1.03))
  assert(topParser.run("0.03").getResult == JNumber(0.03))
  assert(topParser.run("-0.03").getResult == JNumber(-0.03))
  assert(topParser.run("-001.03").getResult == JNumber(-1.03))
  assert(topParser.run("-001.0.3").getStack.head._1.offset == 6)
  assert(topParser.run("1.0-3").getStack.head._1.offset == 3)
  assert(topParser.run("\"ciao\"").getResult == JString("ciao"))
  assert(topParser.run("\"ciao").isError)
  assert(topParser.run("ciao\"").isError)
  assert(topParser.run("ciao").isError)
  assert(topParser.run("\"ciao ciao\"").getResult == JString("ciao ciao"))
  assert(topParser.run("\"ciao_ciao2\"").getResult == JString("ciao_ciao2"))
  assert(topParser.run("true").getResult == JBool(true))
  assert(topParser.run("false").getResult == JBool(false))
  assert(topParser.run("True").isError)
  assert(topParser.run(""""true"""").getResult == JString("true"))
  assert(topParser.run("[1.2]").getResult == JArray(Vector(JNumber(1.2))))
  assert(topParser.run("[true,  false,null, false]").getResult ==
    JArray(Vector(JBool(true), JBool(false), JNull, JBool(false))))
  assert(topParser.run("[true, false, false").isError)
  assert(topParser.run("[1, 2 ,  null,  3]").getResult ==
    JArray(Vector(JNumber(1.0), JNumber(2.0), JNull, JNumber(3.0))))
  assert(topParser.run("[1, 2 ,  null,  3, false]").getResult ==
    JArray(Vector(JNumber(1.0), JNumber(2.0), JNull, JNumber(3.0), JBool(false))))
  assert(topParser.run("""[ "ciao1",  null,"ciao2","ciao3"]""").getResult ==
    JArray(Vector(JString("ciao1"), JNull, JString("ciao2"), JString("ciao3"))))
  assert(topParser.run("""[ [1, 2, 3], [3, 4, null, 5], null]""").getResult ==
    JArray(Vector(
      JArray(Vector(JNumber(1.0), JNumber(2.0), JNumber(3.0))),
      JArray(Vector(JNumber(3.0), JNumber(4.0), JNull, JNumber(5.0))),
      JNull)))
  assert(topParser.run("""{}""").isError)
  assert(topParser.run("""{field1}""").isError)
  assert(topParser.run("""{"field1"}""").isError)
  assert(topParser.run("""{"field1" : }""").isError)
  assert(topParser.run("""{"field1"  : 1""").isError)
  assert(topParser.run("""{"field1"  : 1}""").getResult == JObject(Map("field1" -> JNumber(1.0))))
  assert(topParser.run("""{"field1"  : 1, "field2":   "ciao"}""").getResult ==
    JObject(Map("field1" -> JNumber(1.0), "field2" -> JString("ciao"))))
  assert(topParser.run("""{"field1"  : 1, "field2":   {"field3" : null, "field4": [true, false]}}""").getResult ==
    JObject(
      Map("field1" -> JNumber(1.0),
        "field2" -> JObject(
          Map("field3" -> JNull, "field4" -> JArray(Vector(JBool(true), JBool(false))))))))
  assert(
    topParser.run("""[{"field1"  : 1, "field2":  "ciao"}, {"field3" : null, "field4": [true, false]}]""").getResult ==
      JArray(Vector(
        JObject(Map("field1" -> JNumber(1.0), "field2" -> JString("ciao"))),
        JObject(Map("field3" -> JNull, "field4" -> JArray(Vector(JBool(true), JBool(false))))))))

  object ParserLaws {
    implicit val alphabet = "abcd".toVector

    /**
      * A utility to test that two parsers p1, p2 are equivalent, in the same that
      * they produce the same output on all strings.
      */
    def areEqual[T](p1: Parser[T], p2: Parser[T]): Prop =
      Sampler.string(100).forall(string =>
        p1.run(string) == p2.run(string)
      )

    /**
      * The proposition that a parser p is left unaltered when mapped with the identify
      */
    def mapLaw[T](p: Parser[T]): Prop = areEqual(p, p.map(identity))

    /**
      * A law that must be satisfied by string
      */
    def stringLaw: Prop =
      Sampler.string(100).forall(s => string(s).run(s).getResult== s) &&
        Sampler.listOfN(Sampler.string(100), 2).forall {
          case List(s1, s2) if s1 != s2 => string(s1).run(s2).isError
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
        case (s, t) => succeed(t).run(s) == t
      }
    }

    /**
      * Two law satisfied by the product. Associativity and commutativity with a vector function
      * and its projections.
      * Exercise 9.2
      */
    def productLaw[T1, T2, T3, S1, S2]
    (p1: Parser[T1], p2: Parser[T2], p3: Parser[T3])
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

    /**
      * Law satisfied by label.
      * Exercise 9.10
      */
    def labelLaw[T](p: Parser[T])
                   (sampler: Sampler[String]): Prop = {
      val pairOfStrings = for {
        s1 <- sampler
        s2 <- sampler
      } yield (s1, s2)
      pairOfStrings.forall {
        case (s1, s2) => p.label(s2).run(s1) match {
          case ParserError(stack) => stack.headOption.forall{ case (_, msg) => msg == s2}
          case _ => true
        }
      }
    }

  }

  println(ParserLaws.mapLaw(uncommittedOr).check(maxSize = 1, numSamples = 100000))

}
