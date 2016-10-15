package andrea.scala.functional.programming.testgeneration

import andrea.scala.functional.programming.state.{RandState, StateAction}
import andrea.scala.functional.programming.state.StateAction.{RandAction, nextNonNegativeIntLessThan, sequence}

/**
  * Created by andrea on 10/13/16.
  */

/**
  * Generates examples of the type T
  */
case class GenSamples[+T](sample: RandAction[T])(implicit numSamples: Int) {

  def forall(p: T => Boolean): Prop = {
    val allSamples = List.fill(numSamples)(sample)
    val checkResult = sequence(allSamples).map {
      _.foldLeft(
        Right(0): Prop.CheckResult
        ) {
        case (Right(c), i) if p(i) => Right(c + 1)
        case (Right(c), i) => Left((i.toString, c))
        case (Left((s, c)), i) if p(i) => Left((s, c + 1))
        case (Left((s, c)), _) => Left((s, c))
      }
    }
    Prop(checkResult)
  }

  /**
    * Maps samples generated to new values using f for the mapping
    */
  def map[S](f: T => S): GenSamples[S] = GenSamples(sample.map(f))

}

object GenSamples {

  /**
    * Generates examples of T (with an ordering ord) in the interval [lb, ub)
    */
  def chooseInInterval[T](lb: T, ub: T)
                         (implicit numSamples: Int, ord: Ordering[T])
  : GenSamples[T] = ???

  /**
    * Specialized chooseInInterval to integers. Requires a numSamples
    * Exercise 8.4
    */
  def chooseInInterval(lb: Int, ub: Int)
                      (implicit numSamples: Int): GenSamples[Int] =
    GenSamples(nextNonNegativeIntLessThan(ub - 1 - lb)).map(_ + lb)

  /**
    * Given a thunk it generates test samples all with the same value t
    * Exercise 8.5
    */
  def unit[T](t: => T)(implicit numSamples: Int): GenSamples[T] =
    GenSamples(StateAction.unit[RandState, T](t))

  /**
    * Generates booleans.
    * Exercise 8.5
    */
  def boolean(implicit numSamples: Int): GenSamples[Boolean] =
    GenSamples(nextNonNegativeIntLessThan(2)).map(i => if (i == 0) false else true)

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    * of length n.
    * Exercise 8.5
    */
  def listOfN[T](sample: GenSamples[T], n: Int)
                (implicit numSamples: Int): GenSamples[List[T]]
  = GenSamples(sequence(List.fill(n)(sample.sample)))

}

/**
  * Contains all the information about a property that
  * may or may not hold for an object and how to test
  * it against a set of examples.
  */
case class Prop(checkResult: RandAction[Prop.CheckResult]) {

  /**
    * If the property holds for all examples returns how many
    * examples were tested, if it doesn't it returns a failed case
    * as a string.
    */
  def check(implicit state: RandState): Prop.CheckResult =
    checkResult.runAndGetValue(state)

  /**
    * Constructs a proposition that succeed when both this and other
    * succeed and fails otherwise
    */
  def &&(other: => Prop): Prop = {

    val newRandAction: Prop.CheckResult = (state: RandState) => {
      val thisResult = checkResult.run(state)
      thisResult match {
        case (Left(x), _) => thisResult
        case (thisRight(c), newState) => {
          val otherResult = other.checkResult.run(newState)}
          otherResult match {
            case (Left(y) =>
          }
      }
    }
      checkResult.run(state) match {
        case (Left(x), newState) => Left(x)
        case Right(c) => other.checkResult.run(state)
      }


    //        checkResult.map2(other.checkResult) {
    //        case (Right(c1), Right(c2)) => Right(c1 + c2)
    //        case (Left(x), _ ) => Left(x)
    //        case (_, Left(x)) => Left(x)
    //      }
    //    }
    Prop(newRandAction)
  }

  /**
    * Constructs a proposition that succeed when either this and other
    * succeed and fails if both fail
    */
  def ||(other: => Prop): Prop = Prop(
    checkResult.map2(other.checkResult) {
      case (Left((f1, c1)), Left((f2, c2))) => Left((s"$f1 and $f2", c1 + c2))
      case (Right(x), _ ) => Right(x)
      case (_, Right(x)) => Right(x)
    }
  )

}

object Prop {

  type SuccessCount = Int

  type FailedCase = String

  type CheckResult = Either[(FailedCase, SuccessCount), SuccessCount]

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