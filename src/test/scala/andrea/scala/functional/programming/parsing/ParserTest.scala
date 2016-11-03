package andrea.scala.functional.programming.parsing

import andrea.scala.functional.programming.testing.{Prop, Sampler}
import andrea.scala.functional.programming.either.{Left, Right}

/**
  * Created by andrea on 10/25/16.
  */
object ParserTest extends App {

  object ParserLaws {

    implicit val alphabet = "abcd".toVector

    /**
      * A utility to test that two parsers p1, p2 are equivalent, in the same that
      * they produce the same output on all strings.
      */
    def areEqual[T](p1: MyParser[T], p2: MyParser[T]): Prop =
      Sampler.string(100).forall(string => p1.run(string) == p2.run(string))

    /**
      * The proposition that a parser p is left unaltered when mapped with the identify
      */
    def mapLaw[T](p: MyParser[T]): Prop = areEqual(p, p.map(identity))

    /**
      * A law that must be satisfied by string
      */
    def stringLaw: Prop =
      Sampler.string(100).forall(s => MyParser.string(s).run(s).right.get == s) &&
        Sampler.listOfN(Sampler.string(100), 2).forall {
          case List(s1, s2) if s1 != s2 => MyParser.string(s1).run(s2).isLeft
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
        case (s, t) => MyParser.succeed(t).run(s) == t
      }
    }

    /**
      * Two law satisfied by the product. Associativity and commutativity with a vector function
      * and its projections.
      * Exercise 9.2
      */
    def productLaw[T1, T2, T3, S1, S2]
    (p1: MyParser[T1], p2: MyParser[T2], p3: MyParser[T3])
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

  import MyParser._
  println("* Test string")
  assert("ciao ciao".run("ciao ciao bella") == Right("ciao ciao"))
  assert("ciao ciao".run("cia").left.get.stack.head._1.offset == 3)
  assert("ciao ciao".run("ciao cIao").left.get.stack.head._1.offset == 6)

  println("* Test regex")
  assert("""[a-d][i,o]g""".r.run("dog") == Right("dog"))
  assert("""[a-d][i,o]g""".r.run("doc").left.get.stack.head._1.offset == 0)

  println("* Test succeed")
  assert(succeed(10).run("") == Right(10))
  assert(succeed(List(1, 2)).run("ciao") == Right(List(1, 2)))

  println("* Test delay")
  lazy val thunk = {println("\tparser evaluated"); succeed(10)}
  println("\tinstantiating delayed parser prints nothing")
  val delayedSucceed = delay(thunk)
  println("\tnothing printed")
  println("\tNow running it")
  assert(delayedSucceed.run("ciao") == Right(10))

  println("* Test sequence")
  val sequenceParser = sequence(List[MyParser[String]]("ciao", " ", "5", " ", "ciao"))
  assert(sequenceParser.run("ciao 5 ciao") == Right(List("ciao", " ", "5", " ", "ciao")))
  assert(sequenceParser.run("ciao 6 ciao").left.get.stack.head._1.offset == 0)

}
