package andrea.scala.functional.programming

import andrea.scala.functional.programming.state.{RandState, SimpleRandomState, StateAction}
import andrea.scala.functional.programming.Chapter8.Prop.{Falsified, Passed, Result}
import andrea.scala.functional.programming.Chapter8.Gen
import andrea.scala.functional.programming.stream.Stream
import andrea.scala.functional.programming.stream.Stream.unfold
/**
  * Created by andrea on 7/10/16.
  */
object Chapter8 {

  case class Gen[A](sample: StateAction[RandState, A]) {
    def forAll(predicate: A => Boolean): Prop =
      Prop {
        (_, testCases, rng) => {
          val stream = unfold(rng)(rng => Some(sample.run(rng)))
          val streamWithCounts = stream.zip(Stream.from(0)).take(testCases)
          val firstFailure = streamWithCounts.filter {
            case (a, _) => !predicate(a)
          }.headOption
          firstFailure.map {
            case (failureCase, successes) =>
              Falsified(Gen.buildMessage(failureCase), successes)
          }.getOrElse(Prop.Passed)
        }
      }

    // Exercise 8.6
    def flatMap[B](op: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(op(_).sample))

    def listOfN(n: Gen[Int]): Gen[List[A]] =
      n.flatMap(Gen.listOfN(_, this))

    def map2[B, C](other: Gen[B])(op: (A, B) => C): Gen[C] =
      flatMap {
        a => other.flatMap {
          b => Gen.unit(op(a, b))
        }
      }

    // Exercise 8.7
    def union(other: Gen[A]) =
      Gen.boolean.flatMap {
        if (_) this else other
      }

    def weighted(w1: Double, w2: Double, other: Gen[A]): Gen[A] = {
      val p = w1 / (w1 + w2)
      Gen.double.flatMap(d => if (d <= p) this else other)
    }

    // Esercise 8.10
    def unsized: SGen[A] = SGen[A](_ => this)

  }

  case class SGen[A](forSize: Int => Gen[A]) {
    // Exercise 8.11
    def forAll(predicate: A => Boolean): Prop =
      Prop {
        (maxSize, testCases, rng) => {
          val casesPerSize = (testCases + maxSize - 1) / maxSize
          val props = Stream.from(1).take(maxSize.min(testCases)).map(forSize(_).forAll(predicate))
          val prop = props.map {
            prop => Prop((maxSize, _, rng) => prop.run(maxSize, casesPerSize, rng))
          }.toList.reduce(_ && _)
          prop.run(maxSize, testCases, rng)
        }
      }

    def flatMap[B](op: A => SGen[B]): SGen[B] =
      SGen(i => forSize(i).flatMap(b => op(b).forSize(i)))

    def listOfN(n: SGen[Int]): SGen[List[A]] =
      SGen(i => forSize(i).listOfN(n.forSize(i)))

    def map2[B, C](other: SGen[B])(op: (A, B) => C): SGen[C] =
      SGen(i => forSize(i).map2(other.forSize(i))(op))

    def union(other: SGen[A]) =
      SGen(i => forSize(i).union(other.forSize(i)))

    def weighted(w1: Double, w2: Double, other: SGen[A]): SGen[A] =
      SGen(i => forSize(i).weighted(w1, w2, other.forSize(i)))

  }

  object Gen {
    // Exercise 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(StateAction(_.nextInt))

    // Exercise 8.5
    def unit[A](a: A): Gen[A] = Gen(StateAction.unit(a))

    def boolean: Gen[Boolean] = {
      val state = new StateAction[RandState, Int](_.nextInt).map { i => if (i < 0) false else true }
      Gen(state)
    }

    def double: Gen[Double] =
      Gen(StateAction[RandState, Double](_.nextDouble))

    // Exercise 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(i => g.listOfN(Gen.unit(i)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val states = StateAction.sequence(List.fill(n)(g.sample))
      Gen(states)
    }

    def buildMessage[A](a: A): String = s"Test Case $a failed to pass the test"

  }

  case class Prop(run: (Prop.MaxSize, Prop.TestCases, RandState) => Prop.Result) {
    // Exercise 8.9
    def &&(other: Prop): Prop = Prop {
      (maxSize, testCases, rng) => {
        val resultL = run(maxSize, testCases, rng)
        resultL match {
          case Prop.Falsified(_, _) => resultL
          case Prop.Passed => other.run(maxSize, testCases, rng)
        }
      }
    }

    def ||(other: Prop): Prop = Prop {
      (maxSize, testCases, rng) => {
        val resultL = run(maxSize, testCases, rng)
        resultL match {
          case Prop.Falsified(_, _) => other.run(maxSize, testCases, rng)
          case Prop.Passed => Prop.Passed
        }
      }
    }

    def check: Prop.Result = ???
  }

  object Prop {

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RandState = SimpleRandomState(System.currentTimeMillis())): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, counts) =>
          println(s"! Falsified after $counts passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests")
      }

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified: Boolean = true
    }

    type MaxSize = Int
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int
  }

}

object TestChapter8 extends App {
  import Chapter8._
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Gen.listOf(smallInt).forAll {
    ns => !ns.exists( _ > ns.max)
  }
  Prop.run(maxProp)
}
