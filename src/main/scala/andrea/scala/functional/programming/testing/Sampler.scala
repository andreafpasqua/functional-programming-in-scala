package andrea.scala.functional.programming.testing

import andrea.scala.functional.programming.state.{RandState, StateAction}
import andrea.scala.functional.programming.state.StateAction._
import andrea.scala.functional.programming.stream.Stream
import andrea.scala.functional.programming.option.{Some, None, Option}


/**
  * Generates samples from a population of the type T randomly, optionally it
  * contains the entire population and its size.
  * This version attempts to create a sampler that keeps track of the entire
  * population in most cases.
  * Exercise 8.15
  */
case class Sampler[+T](sample: RandAction[T], population: Stream[Option[T]] = Stream(None)) {

  /**
    * Given a sampler generates the corresponding proposition that holds
    * only if all the samples generated for a given state and sample size
    * satisfy the property p
    */
  def forall(p: T => Boolean): Prop = {
    /**
      * given a stream of optional values it carries out numChecks checks that p holds
      * for the values. It knows numSuccesses checks succeeded so far. If the stream ends
      * then it returns the result specified by onEnd
      */
    @annotation.tailrec
    def go(stream: Stream[Option[T]], numSuccesses: Int, numChecks: Int,
           onEnd: Prop.CheckResult): Prop.CheckResult = {
      if (stream.isEmpty) onEnd
      else if (numChecks == 0) Prop.Passed
      else stream.head match {
        case None => Prop.Passed
        case Some(t) =>
          val boolEither = try {Right(p(t))} catch { case e: Exception => Left(e)}
          boolEither match {
            case Left(e) => Prop.Falsified(Sampler.buildMessageFromException(e, t), numSuccesses)
            case Right(true) => go(stream.tail, numSuccesses + 1, numChecks - 1, onEnd)
            case Right(false) => Prop.Falsified(t.toString, numSuccesses)
          }
      }
    }

    Prop(
      (_, sampleSize, state) =>
        if (population.isEmpty || population.head.isEmpty) {
          go(getRandomSample(state), 0, sampleSize, Prop.Passed)
        }
        else {
            go(population, 0, sampleSize / 3, Prop.Proved) match {
              case Prop.Passed => go(getRandomSample(state), sampleSize / 3, 2 * sampleSize / 3, Prop.Passed)
              case result => result
            }
        }
    )
  }

  /**
    * Given a random state state generates a random sample of infinite length, with each element
    * wrapped in a Some
    */
  def getRandomSample(state: RandState) = Sampler.unfoldWithSome(state)(s => Some(sample.run(s)))

  /**
    * Maps samples generated to new values using f for the mapping
    */
  def map[S](f: T => S): Sampler[S] = Sampler(sample.map(f), population.map(_.map(f)))

  /**
    * given a function f that generates samples of s for a given t,
    * it turns sampled ts into sampled ss. Notice that the number of
    * sampled points doesn't change.
    * Exercise 8.6
    */
  def flatMap[S](f: T => Sampler[S]): Sampler[S] =
    Sampler(
      sample.flatMap(f(_).sample),
      population.flatMap {
        case Some(t) => f(t).population
        case None => Stream(None)
      }
    )


  /**
    * given a sampler of integers it constructs lists of length equal to the integers sampled
    * Exercise 8.6
    */
  def listOf(size: Sampler[Int]): Sampler[List[T]] = size.flatMap {
    sz => Sampler.listOfN(this, sz)
  }

  /**
    * It samples randomly from this or from other
    * Exercise 8.7
    */
  def union[TT >: T](other: Sampler[TT]): Sampler[TT] =
    Sampler.boolean.flatMap(bool => if (bool) this else other)

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

}

object Sampler {

  /**
    * The same as unfold of s and f but it wraps the values returned into a Some
    */
  private def unfoldWithSome[T, S](s: S)(f: S => Option[(T, S)]): Stream[Option[T]] =
    Stream.unfold(s)(f).map(Some(_))

  /**
    * Specialized chooseInInterval to integers. Requires a numSamples
    * Exercise 8.4
    */
  def intInInterval(lb: Int, ub: Int): Sampler[Int] = Sampler(
      nextNonNegativeIntLessThan(ub - lb),
      unfoldWithSome(0)(n => if (n < ub - lb) Some((n, n + 1)) else None)
  ).map(_ + lb)

  /**
    * Given a thunk it generates test samples all with the same value t
    * Exercise 8.5
    */
  def unit[T](t: => T): Sampler[T] = Sampler(StateAction.unit[RandState, T](t), Stream(Some(t)))

  /**
    * Generates booleans.
    * Exercise 8.5
    */
  def boolean: Sampler[Boolean] = intInInterval(0, 2).map(i => if (i == 0) false else true)

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
    intInInterval(0, alphabet.length).listOf(
      intInInterval(0, maxLength + 1)
    ).map(_.map(alphabet).mkString)

  /**
    * Samples the space of functions from domain to coDomain. If both Samplers have
    * a population so will the space of functions. If domain does not have a
    * population than the functions will all have finite support of size supportSize
    * and a randomly chosen default value outside the support.
    * Exercise 8.19
    */
  def function[T, S](domain: Sampler[T], coDomain: Sampler[S],
                     supportSize: Int = 100): Sampler[T => S] = {

    val mapSampler = for {
      defaultValue <- coDomain
      coSupport <- Sampler.listOfN(coDomain, supportSize)
      support <- Sampler.listOfN(domain, supportSize)
    } yield support.distinct.zip(coSupport).toMap.withDefaultValue(defaultValue)

    mapSampler.map(map => (t: T) => map(t))
  }


  private def buildMessageFromException[T](e: Exception, t: T): String =
    s"test case $t\ngenerated an exception ${e.getMessage}\nwith stack trace:\n" +
      s"${e.getStackTrace.mkString}\n"

  /**
    * Given a population of type T, it constructs the full population for lists of length
    * n of type T elements
    */
  private def getPopulationForList[T](population: Stream[Option[T]], n: Int): Stream[Option[List[T]]] = {

    @annotation.tailrec
    def go(result: Stream[Option[List[T]]], n: Int): Stream[Option[List[T]]] = {
      if (n <= 0) result
      else {
        val newResult = for {
          list <- result
          t <- population
        } yield t.map2(list)(_ :: _)
        go(newResult, n - 1)
      }
    }
    go(Stream(Some(List.empty[T])), n)
  }

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    * of length n.
    * Exercise 8.5
    */
  def listOfN[T](sampler: Sampler[T], n: Int): Sampler[List[T]] = {
    val newSample = sequence(List.fill(n)(sampler.sample))
    val newPopulation = getPopulationForList(sampler.population, n)
    Sampler(newSample, newPopulation)
  }

}