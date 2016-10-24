package andrea.scala.functional.programming.testing

import andrea.scala.functional.programming.state.{RandState, StateAction, SimpleRandomState}
import andrea.scala.functional.programming.state.StateAction._
import andrea.scala.functional.programming.stream.Stream
import andrea.scala.functional.programming.stream.Stream.{apply => _, _}


case class Population[+T](population: Stream[T], size: Int) {

  /**
    * Combines two populations into a single one.
    * Notice that if the two populations overlap there will be repetitions
    */
  def ++[TT >: T](other: Population[TT]): Population[TT] =
    Population(population.append(other.population), size + other.size)

}
/**
  * Generates samples from a population of the type T randomly, optionally it
  * contains the entire population and its size.
  * This version attempts to create a sampler that keeps track of the entire
  * population in most cases.
  * Exercise 8.15
  */
case class Sampler[+T](sample: RandAction[T], population: Option[Population[T]] = None) {

  /**
    * Given a sampler generates the corresponding proposition that holds
    * only if all the samples generated for a given state and sample size
    * satisfy the property p
    */
  def forall(p: T => Boolean): Prop = Prop(
    (_, sampleSize, state) => population match {
      case Some(Population(pop, size)) if size <= sampleSize => Sampler.falsify(pop, p).getOrElse(Prop.Proved)
      case _ =>
        val randomSample = getRandomSample(sampleSize, state)
        Sampler.falsify(randomSample, p).getOrElse(Prop.Passed)
    }
  )

  /**
    * Given a random state state generates a random sample of sampleSize
    */
  def getRandomSample(sampleSize: Int, state: RandState) = unfold(state)(s => Some(sample.run(s))).take(sampleSize)

  /**
    * Maps samples generated to new values using f for the mapping
    */
  def map[S](f: T => S): Sampler[S] = Sampler(
    sample.map(f),
    population.map {pop =>  Population(pop.population.map(f), pop.size)}
  )

  /**
    * given a function f that generates samples of s for a given t,
    * it turns sampled ts into sampled ss. Notice that the number of
    * sampled points doesn't change.
    * Exercise 8.6
    */
  def flatMap[S](f: T => Sampler[S]): Sampler[S] = {

    val newSample = sample.flatMap(t => f(t).sample)

    val newPopulation = population.flatMap(
      _.population.foldRight(
        Some(Population(Stream.empty[S], 0)): Option[Population[S]]) {
          case (_, None) => None
          case (t, Some(cumulative)) => f(t).population.map(_ ++ cumulative)
        }
    )
    Sampler(newSample, newPopulation)
  }

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
    * Specialized chooseInInterval to integers. Requires a numSamples
    * Exercise 8.4
    */
  def intInInterval(lb: Int, ub: Int): Sampler[Int] =
    Sampler(
      nextNonNegativeIntLessThan(ub - lb),
      Some(Population(Stream.unfold(0)(n => if (n < ub - lb) Some((n, n + 1)) else None), ub - lb))
    ).map(_ + lb)

  /**
    * Given a thunk it generates test samples all with the same value t
    * Exercise 8.5
    */
  def unit[T](t: => T): Sampler[T] =
    Sampler(
      StateAction.unit[RandState, T](t),
      Some(Population(Stream(t), 1))
    )

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
    intInInterval(0, alphabet.length)
      .listOf(intInInterval(0, maxLength + 1))
      .map(_.map(alphabet).mkString)

  /**
    * Samples the space of functions from domain to coDomain. If both Samplers have
    * a population so will the space of functions. If domain does not have a
    * population than the functions will all have finite support of size supportSize
    * and a randomly chosen default value outside the support.
    * Exercise 8.19
    */
  def function[T, S](domain: Sampler[T], coDomain: Sampler[S],
                     supportSize: Int = 100): Sampler[T => S] = {

    val functionMap = domain.population.map {
      case Population(population, size) =>
        for {
          coSupport <- Sampler.listOfN(coDomain, size)
        } yield population.toList.zip(coSupport).toMap
    }.getOrElse { // if the population is not specified, sample
      for {
        defaultValue <- coDomain
        support <- Sampler.listOfN(domain, supportSize)
        coSupport <- Sampler.listOfN(coDomain, supportSize)
      } yield support.zip(coSupport).toMap.withDefaultValue(defaultValue)
    }
    functionMap.map(map => (t: T) => map(t))
  }

  /**
    * Given a sample (exhaustive or not) it checks if the predicate p applies to
    * all examples and returns a CheckResult, as either Falsified or Passed.
    */
  def falsify[T](samples: Stream[T], p: T => Boolean): Option[Prop.CheckResult] = {
    val outcomes = samples.zip(from(0)).map {
      case (t, count) => try {
        if (p(t)) Prop.Passed else Prop.Falsified(t.toString, count + 1)
      }
      catch {
        case e: Exception => Prop.Falsified(buildMessageFromException(e, t), count + 1)
      }
    }
    outcomes.find(_.isFalsified)
  }

  private def buildMessageFromException[T](e: Exception, t: T): String =
    s"test case $t\ngenerated an exception ${e.getMessage}\nwith stack trace:\n" +
      s"${e.getStackTrace.mkString}\n"

  /**
    * Given a population of type T, it constructs the full population for lists of length
    * n of type T elements
    */
  private def getPopulationForList[T](population: Population[T], n: Int): Population[List[T]] = {

    @annotation.tailrec
    def go(result: Stream[List[T]], n: Int): Stream[List[T]] = {
      if (n <= 0) result
      else {
        val newResult = for {
          list <- result
          t <- population.population
        } yield t :: list
        go(newResult, n - 1)
      }
    }
    Population(go(Stream(List.empty[T]), n), math.pow(population.size, n).toInt)
  }

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    * of length n.
    * Exercise 8.5
    */
  def listOfN[T](sampler: Sampler[T], n: Int): Sampler[List[T]] = {
    val newSample = sequence(List.fill(n)(sampler.sample))
    val newPopulation = sampler.population.map(getPopulationForList(_, n))
    Sampler(newSample, newPopulation)
  }

}