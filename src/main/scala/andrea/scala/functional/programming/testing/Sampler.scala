package andrea.scala.functional.programming.testing

import andrea.scala.functional.programming.state.{RandState, StateAction}
import andrea.scala.functional.programming.state.StateAction.{RandAction, nextNonNegativeIntLessThan, sequence}
import andrea.scala.functional.programming.stream.Stream.{from, unfold}
import andrea.scala.functional.programming.stream.Stream
/**
  * Created by andrea on 10/13/16.
  */

/**
  * Generates examples of the type T
  */
case class Sampler[+T](sample: RandAction[T]) {

  /**
    * Given a sampler generates the corresponding proposition that holds
    * only if all the samples generated for a given state and sample size
    * satisfy the property p
    */
  def forall(p: T => Boolean): Prop = Prop(
    (_, sampleSize, state) => {
      val samplesStreamed = unfold(state)(s => Some(sample.run(s)))
      val finiteSample = samplesStreamed.take(sampleSize)
      Sampler.checkSample(finiteSample, p)
    }
  )

  /**
    * Maps samples generated to new values using f for the mapping
    */
  def map[S](f: T => S): Sampler[S] = Sampler(sample.map(f))

  /**
    * given a function f that generates samples of s for a given t,
    * it turns sampled ts into sampled ss. Notice that the number of
    * sampled points doesn't change.
    * Exercise 8.6
    */
  def flatMap[S](f: T => Sampler[S]): Sampler[S] = Sampler(sample.flatMap(t => f(t).sample))

  /**
    * given a sampler of integers it constructs lists of length equal to the integers sampled
    * Exercise 8.6
    */
  def listOf(size: Sampler[Int]): Sampler[List[T]] = size.flatMap(sz => Sampler(sequence(List.fill(sz)(sample))))

  /**
    * It samples randomly from this or from other
    * Exercise 8.7
    */
  def union[TT >: T](other: Sampler[TT]): Sampler[TT] = Sampler.boolean.flatMap(bool => if (bool) this else other)

  /**
    * It samples from this or from other in proportion to thisWeight and otherWeight respectively.
    * thisWeight and otherWeight are nonnegative
    * Exercise 8.7
    */
  def weighted[TT >: T](other: Sampler[TT], thisWeight: Double, otherWeight: Double)
  : Sampler[TT] =
    if (thisWeight < 0 || otherWeight < 0)
      throw new IllegalArgumentException(s"Negative weights (w1, w2) = ${(thisWeight, otherWeight)}")
    else {
      val w1 = thisWeight / (thisWeight + otherWeight)
      Sampler.double.flatMap(d => if (d < w1) this else other)
    }

  /**
    * A routine to convert a sampler to an unsized sampler. the unsized sampler
    * simply ignores the size input.
    * Exercise 8.10
    */
  def unSized: BySizeSampler[T] = BySizeSampler(_ => this)

  /************** OLD ROUTINES
    */

  /**
    * Given a sampler generates the corresponding proposition that holds
    * only if all the samples generated for a given state and sample size
    * satisfy the property p
    */
  def forallOld(p: T => Boolean): Prop = Prop(
    (_, sampleSize, state) => {
      val allSamples = sequence(List.fill(sampleSize)(sample))
      allSamples.map {
        _.foldLeft((Prop.Passed: Prop.CheckResult, 0)) {
          case ((Prop.Passed, successes), t) if p(t) => (Prop.Passed, successes + 1)
          case ((Prop.Passed, successes), t) => (Prop.Falsified(t.toString, successes), successes)
          case ((Prop.Falsified(fail, counts), successes), t) if p(t) => (Prop.Falsified(fail, counts + 1), successes + 1)
          case ((Prop.Falsified(fail, counts), successes), t) => (Prop.Falsified(fail, counts), successes)
        }
      }.runAndGetValue(state)._1
    }
  )
}

object Sampler {

  /**
    * Specialized chooseInInterval to integers. Requires a numSamples
    * Exercise 8.4
    */
  def intInInterval(lb: Int, ub: Int): Sampler[Int] =
    Sampler(nextNonNegativeIntLessThan(ub - lb)).map(_ + lb)

  /**
    * Given a thunk it generates test samples all with the same value t
    * Exercise 8.5
    */
  def unit[T](t: => T): Sampler[T] =
    Sampler(StateAction.unit[RandState, T](t))

  /**
    * Generates booleans.
    * Exercise 8.5
    */
  def boolean: Sampler[Boolean] =
    Sampler(nextNonNegativeIntLessThan(2)).map(i => if (i == 0) false else true)

  /**
    * Generates pairs of integers each in the interval [lb, ub)
    */
  def intPairInInterval(lb: Int, ub: Int): Sampler[(Int, Int)] = {
    intInInterval(lb, ub).listOf(unit(2)).
      map(x => (x.head, x(1)))
  }

  /**
    * Generates doubles in [0, 1)
    * Exercise 8.8
    */
  def double: Sampler[Double] = Sampler(StateAction.nextDouble)

  /**
    * Generates a random string of length length made with the characters
    * in alphabet
    */
  def string(maxLength: Int)
            (implicit alphabet: IndexedSeq[Char]): Sampler[String] =
    intInInterval(0, alphabet.length - 1)
      .listOf(intInInterval(0, maxLength + 1))
      .map(_.map(alphabet).mkString)

  /**
    * Given a sample (exhaustive or not) it checks if the predicate p applies to
    * all examples and returns a CheckResult, as either Falsified or Passed.
    */
  def checkSample[T](samples: Stream[T], p: T => Boolean): Prop.CheckResult = {
    val outcomes = samples.zip(from(0)).map {
      case (t, count) => try {
        if (p(t)) Prop.Passed else Prop.Falsified(t.toString, count)
      }
      catch {
        case e: Exception => Prop.Falsified(buildMessageFromException(e, t), count)
      }
    }
    outcomes.find(_.isFalsified).getOrElse(Prop.Passed)
  }

  private def buildMessageFromException[T](e: Exception, t: T): String =
    s"test case $t\ngenerated an exception ${e.getMessage}\nwith stack trace:\n" +
      s"${e.getStackTrace.mkString}\n"

  /*********************** OLD ROUTINES
    *
    */

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    * of length n.
    * Exercise 8.5
    */
  def listOfN[T](sample: Sampler[T], n: Int): Sampler[List[T]] =
    Sampler(sequence(List.fill(n)(sample.sample)))

}



