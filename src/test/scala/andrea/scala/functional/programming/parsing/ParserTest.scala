//package andrea.scala.functional.programming.parsing
//
//import andrea.scala.functional.programming.testing.{Prop, Sampler}
//import andrea.scala.functional.programming.either.{Left, Right}
//
///**
//  * Created by andrea on 10/25/16.
//  */
//object ParserTest extends App {
//
//  import andrea.scala.functional.programming.parsing.Parser._
//
//  object ParserLaws {
//    implicit val alphabet = "abcd".toVector
//
//    /**
//      * A utility to test that two parsers p1, p2 are equivalent, in the same that
//      * they produce the same output on all strings.
//      */
//    def areEqual[T](p1: Parser[T], p2: Parser[T]): Prop =
//      Sampler.string(100).forall(string => p1.run(string) == p2.run(string))
//
//    /**
//      * The proposition that a parser p is left unaltered when mapped with the identify
//      */
//    def mapLaw[T](p: Parser[T]): Prop = areEqual(p, p.map(identity))
//
//    /**
//      * A law that must be satisfied by string
//      */
//    def stringLaw: Prop =
//      Sampler.string(100).forall(s => string(s).run(s).right.get == s) &&
//        Sampler.listOfN(Sampler.string(100), 2).forall {
//          case List(s1, s2) if s1 != s2 => string(s1).run(s2).isLeft
//          case _ => true
//        }
//
//    /**
//      * A law that succeeds must satisfy. Basically the same as for unit.
//      */
//    def succeedLaw[T](sampler: Sampler[T]): Prop = {
//      val stringAndValue = for {
//        s <- Sampler.string(100)
//        t <- sampler
//      } yield (s, t)
//      stringAndValue.forall {
//        case (s, t) => succeed(t).run(s) == t
//      }
//    }
//
//    /**
//      * Two law satisfied by the product. Associativity and commutativity with a vector function
//      * and its projections.
//      * Exercise 9.2
//      */
//    def productLaw[T1, T2, T3, S1, S2]
//    (p1: Parser[T1], p2: Parser[T2], p3: Parser[T3])
//    (f1: Sampler[T1 => S1], f2: Sampler[T2 => S2]): Prop =
//      areEqual(
//        ((p1 ** p2) ** p3).map { case ((t1, t2), t3) => (t1, t2, t3)},
//        (p1 ** (p2 ** p3)).map { case (t1, (t2, t3)) => (t1, t2, t3)}
//      ) && {
//        val stringAndFunctionPair = for {
//          string <- Sampler.string(100)
//          func1 <- f1
//          func2 <- f2
//        } yield (string, func1, func2)
//        stringAndFunctionPair.forall {
//          case (s, f, g) =>
//            val mapBefore = p1.map(f) ** p2.map(g)
//            val mapAfter = (p1 ** p2).map { case (t1, t2) => (f(t1), g(t2)) }
//            mapBefore.run(s) == mapAfter.run(s)
//        }
//      }
//
//    /**
//      * Law satisfied by label.
//      * Exercise 9.10
//      */
//    def labelLaw[T](p: Parser[T])
//                   (sampler: Sampler[String]): Prop = {
//      val pairOfStrings = for {
//        s1 <- sampler
//        s2 <- sampler
//      } yield (s1, s2)
//      pairOfStrings.forall {
//        case (s1, s2) => p.label(s2).run(s1) match {
//          case Left(ParserError((loc, msg) :: list)) => msg == s2
//          case _ => true
//        }
//      }
//    }
//
//  }
//
//  println("* Test Location line, column, parsed and unparsed")
//  val locInput = "ciao ciao\n ciao"
//  val loc = Location(locInput, 0)
//  val loc1 = Location(locInput, 9)
//  val loc2 = Location(locInput, 11)
//  assert(loc.line == 1 && loc.column == 1)
//  assert(loc1.line == 2 && loc1.column == 0)
//  assert(loc2.line == 2 && loc2.column == 2)
//  assert(loc.parsed == "" && loc.unParsed == locInput)
//  assert(loc1.parsed == "ciao ciao" && loc1.unParsed == "\n ciao")
//  assert(loc2.parsed == "ciao ciao\n " && loc2.unParsed == "ciao")
//
//  println("* Test Location +")
//  assert(loc1 + 2 == loc2)
//  assert(loc + 11 == loc2)
//
//  println("* Test string")
//  assert("ciao ciao".run("ciao ciao bella") == Right("ciao ciao"))
//  assert("ciao ciao".run("cia").left.get.stack.head._1.offset == 3)
//  assert("ciao ciao".run("ciao cIao").left.get.stack.head._1.offset == 6)
//
//  println("* Test regex")
//  assert("""[a-d][i,o]g""".r.run("dog") == Right("dog"))
//  assert("""[a-d][i,o]g""".r.run("doc").left.get.stack.head._1.offset == 0)
//
//  println("* Test succeed")
//  assert(succeed(10).run("") == Right(10))
//  assert(succeed(List(1, 2)).run("ciao") == Right(List(1, 2)))
//
//  println("* Test delay")
//  lazy val thunk = {println("\tParser evaluated"); succeed(10)}
//  println("\tInstantiating delayed parser prints nothing")
//  println("\tInstantiating...")
//  val delayedSucceed = delay(thunk)
//  println("\tNothing printed")
//  println("\tNow running it")
//  assert(delayedSucceed.run("ciao") == Right(10))
//
//  println("* Test flatMap")
//  def ifZero(c: Char): Parser[String]=
//    if (c == '0') string("isZero") else string("isNotZero")
//  assert(char('0').flatMap(ifZero).run("0isZero") == Right("isZero"))
//  assert(char('1').flatMap(ifZero).run("1isNotZero") == Right("isNotZero"))
//  assert(char('0').flatMap(ifZero).run("0isNotZero")
//    .left.get.stack.head._1.offset == 3)
//  assert(char('1').flatMap(ifZero).run("1isZero")
//    .left.get.stack.head._1.offset == 3)
//
//  println("* Test slice")
//  assert(string("abc").map(_.length).run("abcdef") == Right(3))
//  assert(string("abc").map(_.length).slice.run("abcdef") == Right("abc"))
//  assert(regex("""[a-d][i,o]g""".r).slice.run("dog and cat") == Right("dog"))
//  assert(string("abc").map(_.length).slice.run("abCdef")
//    .left.get.stack.head._1.offset == 2)
//
//  println("* Test label")
//  assert {
//    val relabeledError = label("new label")("abc").run("aBc").left.get
//    relabeledError.stack.length == 1 &&
//      relabeledError.stack.head._2 == "new label"
//  }
//
//  println("* Test scope")
//  assert {
//    val scopedError =
//      scope("scope message")(label("new label")("abc")).run("aBc").left.get
//    scopedError.stack.length == 2 &&
//      scopedError.stack.head._2 == "scope message" &&
//      scopedError.stack.tail.head._2 == "new label"
//  }
//
//  val scopedParser = scope("scope message")(
//    label("first label")(string("ciao")) **
//      label("second label")(string(" ciao"))
//  )
//  assert {
//    val error1 = scopedParser.run("cIao ciao").left.get
//    val error2 = scopedParser.run("ciao cIao").left.get
//    error1.stack.head._2 == "scope message" &&
//      error1.stack(1)._2 == "first label" &&
//      error2.stack.head._2 == "scope message" &&
//      error2.stack(1)._2 == "second label"
//  }
//
//  println("* Test attempt")
//  val committed = string("ciao")
//  val uncommitted = attempt(string("ciao"))
//  val uncommitted2 = attempt(string("ciao")) ** string(" ciao")
//  val initialState = ParserState(Location("ciao", 0), isCommitted = true)
//  assert(committed.action.runAndGetState(initialState).isCommitted)
//  assert(!uncommitted.action.runAndGetState(initialState).isCommitted)
//  assert(!uncommitted2.action.runAndGetState(initialState).isCommitted)
//
//  println("* Test or")
//  val committedOr = (string("ciao ") >* "ciao") | "hello"
//  val uncommittedOr = (attempt("ciao ") >* "ciao") | "hello"
//  assert(committedOr.run("ciao ciao") == Right("ciao"))
//  assert(committedOr.run("hello").isLeft)
//  assert(committedOr.run("cia0 ciao").isLeft)
//  assert(uncommittedOr.run("ciao ciao") == Right("ciao"))
//  assert(uncommittedOr.run("hello") == Right("hello"))
//  assert(uncommittedOr.run("cia0 ciao").isLeft)
//
//  //  println("* Test sequence")
////  val sequenceParser = sequence(List[Parser[String]]("ciao", " ", "5", " ", "ciao"))
////  assert(sequenceParser.run("ciao 5 ciao") == Right(List("ciao", " ", "5", " ", "ciao")))
////  assert(sequenceParser.run("ciao 6 ciao").left.get.stack.head._1.offset == 0)
//
//}
