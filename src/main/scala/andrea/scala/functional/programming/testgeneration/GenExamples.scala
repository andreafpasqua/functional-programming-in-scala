package andrea.scala.functional.programming.testgeneration

import andrea.scala.functional.programming.state.StateAction.{nextNonNegativeIntLessThan, sequence}

/**
  * Created by andrea on 10/13/16.
  */

/**
  * Generates examples of the type T
  */
trait GenExamples[+T] {

  def forall(p: T => Boolean): Prop

}

case class GenNExamples[+T](n: Int, seed: Long) extends GenExamples[T] {

  def forall(p : T => Boolean): Prop = ???

}

object GenExamples {

  /**
    * Generates examples of T (with an ordering ord) in the interval [lowerbound, upperbound]
    */
  def chooseInInterval[T](lb: T, ub: T)
                         (implicit seed: Long, ord: Ordering[T])
  : GenExamples[T] = ???

  def chooseInInterval(lb: Int, ub: Int): GenExamples[Int] =
    GenNExamples(nextNonNegativeIntLessThan(ub - lb).map(_ + lb)

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    */
  def listOf[T](examples: GenExamples[T]): GenExamples[List[T]] = ???
}

/**
  * Contains all the information about a property that
  * may or may not hold for an object and how to test
  * it against a set of examples.
  */
trait Prop {

  import Prop._
  /**
    * If the property holds for all examples returns how many
    * examples were tested, if it doesn't it returns a failed case
    * as a string.
    */
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  /**
    * Constructs a proposition that succeed when both this and other
    * succeed and fails otherwise
    */
  def &&(other: => Prop): Prop = new Prop {

    def check = (this.check, other.check) match {
      case (Right(c1), Right(c2)) => Right(c1 + c2)
      case (Left(_), _ ) => this.check
      case (_, Left(_)) => other.check
    }

  }

  /**
    * Constructs a proposition that succeed when either this and other
    * succeed and fails if both fail
    */
  def ||(other: => Prop): Prop = new Prop {

    def check = (this.check, other.check) match {
      case (Left((f1, c1)), Left((f2, c2))) => Left((s"$f1 ande $f2", c1 + c2))
      case (Right(_), _ ) => this.check
      case (_, Right(_)) => other.check
    }

  }
}

object Prop {

  type SuccessCount = Int

  type FailedCase = String


  /***************** OLD METHODS
    *
    */
  trait PropOld {
    /**
      * True if the property holds for all examples, false if it
      * can be falsified by at least one of them
      */
    def check: Boolean

    /**
      * Combines two Props in a Prop that checks true if both props check true
      * Exercise 8.3
      */
    def && (other: => PropOld): PropOld = new PropOld {
      def check = this.check && other.check
    }
  }
}