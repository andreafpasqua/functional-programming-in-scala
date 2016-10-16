package andrea.scala.functional.programming.testgeneration

import andrea.scala.functional.programming.state.{RandState, SimpleRandomState}

/**
  * Contains all the information about a property that
  * may or may not hold for an object and how to test
  * it against a set of examples each of size TestSize
  * and numbering SampleSize.
  */
case class Prop(run: (Prop.MaxTestSize, Prop.SampleSize, RandState) => Prop.CheckResult) {

  /**
    * A convenient method to call run with defaults for the max test size, the sample size and
    * the random state
    */
  def check(maxTestSize: Prop.MaxTestSize = 100,
            numSamples: Prop.SampleSize = 100,
            state: RandState = SimpleRandomState(System.currentTimeMillis())
           ): Prop.CheckResult = {
    val result = run(maxTestSize, numSamples, state)
    result match {
      case Prop.Falsified(msg, n) => println(s"Falsified after $n samples with message $msg")
      case Prop.Passed => println(s"Passed after $numSamples tests")
    }
    result
  }

  /**
    * Constructs a proposition that is falsified when either this and other
    * are falsified
    * Exercise 8.9
    */
  def &&(other: => Prop): Prop = Prop(
    (testSize, sampleSize, state) => {
      val thisResult = run(testSize, sampleSize, state)
      if (thisResult.isFalsified) thisResult
      else other.run(testSize, sampleSize, state)
    }
  )

  /**
    * Constructs a proposition that is falsified when both this and other
    * are falsified
    * Exercise 8.9
    */
  def ||(other: => Prop): Prop = Prop(
    (testSize, sampleSize, state) => {
      val thisResult = run(testSize, sampleSize, state)
      if (thisResult.passed) thisResult
      else other.run(testSize, sampleSize, state)
    }
  )

}

object Prop {

  type SuccessCount = Int

  /**
    * This is the maximum size of each test point, not the size of the sampler.
    * What the meaning of the size is depends on the definition of unSizedSampler.
    * For instance it could be the length of a list, or the order of magnitude of
    * an integer and so on.
    */
  type MaxTestSize = Int

  type SampleSize = Int

  type FailedCase = String

  trait CheckResult {
    def isFalsified: Boolean
    val passed: Boolean = !isFalsified
  }

  case object Passed extends CheckResult {
    val isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends CheckResult {
    val isFalsified: Boolean = true
  }

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