package andrea.scala.functional.programming.testing

import andrea.scala.functional.programming.state.{RandState, SimpleRandomState}

/**
  * Copyright 2016, Radius Intelligence, Inc.
  * All Rights Reserved
  * Created by andreapasqua on 10/22/2016.
  */
case class Prop(run: (Prop.MaxTestSize, Prop.SampleSize, RandState) => Prop.CheckResult) {
  /**
    * A convenient method to call run with defaults for the max test size, the sample size and
    * the random state
    */
  def check(
             maxSize: Prop.MaxTestSize = 100,
             numSamples: Prop.SampleSize = 100,
             state: RandState = SimpleRandomState(System.currentTimeMillis())
           ): Prop.CheckResult = {
    val result = run(maxSize, numSamples, state)
    result match {
      case Prop.Falsified(msg, n) => println(s"Falsified after $n tests with message $msg")
      case Prop.Passed => println(s"Passed after $numSamples tests")
      case Prop.Proved => println("Proved")
    }
    result
  }

  /**
    * Constructs a proposition that is falsified when either this and other
    * are falsified
    * Exercise 8.9
    */
  def &&(other: => Prop): Prop = Prop(
    (maxTestSize, sampleSize, state) => {
      val thisResult = run(maxTestSize, sampleSize, state)
      if (thisResult.isFalsified) thisResult
      else other.run(maxTestSize, sampleSize, state)
    }
  )

  /**
    * Constructs a proposition that is falsified when both this and other
    * are falsified
    * Exercise 8.9
    */
  def ||(other: => Prop): Prop = Prop(
    (maxSize, sampleSize, state) => {
      val thisResult = run(maxSize, sampleSize, state)
      if (thisResult.notFalsified) thisResult
      else other.run(maxSize, sampleSize, state)
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

  sealed trait CheckResult {
    def isFalsified: Boolean
    def notFalsified: Boolean = !isFalsified
  }

  case object Passed extends CheckResult {
    val isFalsified: Boolean = false
  }

  case object Proved extends CheckResult {
    val isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends CheckResult {
    val isFalsified: Boolean = true
  }

  /**
    * It creates a proposition which is proved if the predicate p
    * holds and falsified otherwise
    */
  def prove(p: => Boolean): Prop = Prop(
    (_, _, _) => if (p) Proved else Falsified("()", 0)
  )

}