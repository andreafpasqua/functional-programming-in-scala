//package andrea.scala.functional.programming.newtesting
//
//import andrea.scala.functional.programming.state.{RandState, StateAction, SimpleRandomState}
//import andrea.scala.functional.programming.state.StateAction._
//import andrea.scala.functional.programming.stream.Stream
//import andrea.scala.functional.programming.stream.Stream.{apply => _, _}
///**
//  * Created by andreapasqua on 10/17/2016.
//  */
//
//case class BySizeSampler[+T](forSize: Int => Sampler[T]){
//
//  /**
//    * Given an unsized sampler, it constructs a proposition
//    * that holds true if p holds for all samples generated.
//    * The samples are generated in equal numbers for each
//    * of the test point sizes ranging from 0 to a max of testSize
//    * (the first argument of check. What
//    */
//  def forall(p: T => Boolean): Prop = Prop(
//    (maxSize, numSamples, state) => {
//      val samplesPerSize = (numSamples + maxSize - 1) / maxSize // the total number of samples >= numSamples
//      val allProps = for (
//          size <- (0 to 1 + maxSize.min(numSamples)).toList
//        ) yield {
//          forSize(size).forall(p)
//        }
//      allProps.reduce(_ && _).run(maxSize, samplesPerSize, state)
//      // note that check ignores maxSize here since the propositions
//      // where constructed by regular Samplers.
//    }
//  )
//
//  /**
//    * Maps samples generated to new values using f for the mapping.
//    * Exercise 8.11
//    */
//  def map[S](f: T => S): BySizeSampler[S] = BySizeSampler(n => forSize(n).map(f))
//
//  /**
//    * given a function f that generates samples of s for a given t,
//    * it turns sampled ts into sampled ss. Notice that the number of
//    * sampled points doesn't change.
//    * Exercise 8.11
//    */
//  def flatMap[S](f: T => (BySizeSampler[S], Option[Long])): BySizeSampler[S] =
//    BySizeSampler(n => forSize(n).flatMap {
//        t => {
//          val (newSampler, opInt) = f(t)
//          (newSampler.forSize(n), opInt)
//        }
//      })
//
//  /**
//    * given a sampler of integers it constructs lists of length equal to the integers sampled
//    * Exercise 8.12
//    */
//  def listOf(size: BySizeSampler[Int]): BySizeSampler[List[T]] = BySizeSampler(
//    n => forSize(n).listOf(size.forSize(n))
//  )
//
//  /**
//    * It samples randomly from this or from other
//    * Exercise 8.11
//    */
//  def union[TT >: T](other: BySizeSampler[TT]): BySizeSampler[TT] = BySizeSampler(
//    n => forSize(n).union(other.forSize(n))
//  )
//
//  /**
//    * It samples from this or from other in proportion to thisWeight and otherWeight respectively.
//    * thisWeight and otherWeight are nonnegative
//    * Exercise 8.11
//    */
//  def weighted[TT >: T](other: BySizeSampler[TT], thisWeight: Double, otherWeight: Double)
//  : BySizeSampler[TT] = BySizeSampler(
//    n => forSize(n).weighted(other.forSize(n), thisWeight, otherWeight)
//  )
//
//}
//
///**
//  * Generates samples from a population of the type T randomly, optionally it
//  * contains the entire population and its size
//  */
//case class Sampler[+T](sample: RandAction[T], population: Option[Stream[T]] = None, size: Option[Long] = None) {
//
//  /**
//    * Given a sampler generates the corresponding proposition that holds
//    * only if all the samples generated for a given state and sample size
//    * satisfy the property p
//    */
//  def forall(p: T => Boolean): Prop = Prop(
//      (_, sampleSize, state) => (population, size) match {
//        case (Some(pop), Some(n)) if n <= sampleSize => Sampler.falsify(pop, p).getOrElse(Prop.Proved)
//        case (Some(_), _) | (_, Some(_)) =>
//          throw new IllegalArgumentException(s"empty size with non empty population or viceversa")
//        case _ => {
//          val randomSample = unfold(state)(s => Some(sample.run(s))).take(sampleSize)
//          Sampler.falsify(randomSample, p).getOrElse(Prop.Passed)
//        }
//      }
//      )
//
//  /**
//    * Maps samples generated to new values using f for the mapping
//    */
//  def map[S](f: T => S): Sampler[S] = Sampler(
//    sample.map(f), population.map(_.map(f)), size)
//
//  /**
//    * given a function f that generates samples of s for a given t,
//    * it turns sampled ts into sampled ss. Notice that the number of
//    * sampled points doesn't change.
//    * Exercise 8.6
//    */
//  def flatMap[S](f: T => (Sampler[S], Option[Long])): Sampler[S] = {
//
//    val newSample = sample.flatMap(t => f(t)._1.sample)
//    val newPopulation = population.map {_.flatMap((t: T) => f(t)._1.population.getOrElse(Stream.empty[S]))}
//    val newSize = population.map {_.flatMap((t: T) => f(t)._2).foldRight(0L)(_ + _)}
//    Sampler(newSample, newPopulation, newSize)
//
//  }
//
//
//  /**
//    * given a sampler of integers it constructs lists of length equal to the integers sampled
//    * Exercise 8.6
//    */
//  def listOf(size: Sampler[Int]): Sampler[List[T]] = size.flatMap(
//    sz => (Sampler(sequence(List.fill(sz)(sample))), Option.empty[Long])
//  )
//
//  /**
//    * It samples randomly from this or from other
//    * Exercise 8.7
//    */
//  def union[TT >: T](other: Sampler[TT]): Sampler[TT] =
//    Sampler.boolean.flatMap(
//      bool =>
//        if (bool) (this, Some(1L))
//        else (other, Some(1L))
//    )
//
//  /**
//    * It samples from this or from other in proportion to thisWeight and otherWeight respectively.
//    * thisWeight and otherWeight are nonnegative
//    * Exercise 8.7
//    */
//  def weighted[TT >: T](other: Sampler[TT], thisWeight: Double, otherWeight: Double)
//  : Sampler[TT] =
//    if (thisWeight < 0 || otherWeight < 0)
//      throw new IllegalArgumentException(s"Negative weights (w1, w2) = ${(thisWeight, otherWeight)}")
//    else {
//      val w1 = thisWeight / (thisWeight + otherWeight)
//      Sampler.double.flatMap(d => if (d < w1) (this, None) else (other, None))
//    }
//}
//
//case class Prop(run: (Prop.MaxTestSize, Prop.SampleSize, RandState) => Prop.CheckResult) {
//  /**
//    * A convenient method to call run with defaults for the max test size, the sample size and
//    * the random state
//    */
//  def check(
//             maxSize: Prop.MaxTestSize = 100,
//             numSamples: Prop.SampleSize = 100,
//             state: RandState = SimpleRandomState(System.currentTimeMillis())
//           ): Prop.CheckResult = {
//    val result = run(maxSize, numSamples, state)
//    result match {
//      case Prop.Falsified(msg, n) => println(s"Falsified after $n samples with message $msg")
//      case Prop.Passed => println(s"Passed after $numSamples tests")
//      case Prop.Proved => println("Proved")
//    }
//    result
//  }
//
//  /**
//    * Constructs a proposition that is falsified when either this and other
//    * are falsified
//    * Exercise 8.9
//    */
//  def &&(other: => Prop): Prop = Prop(
//    (maxTestSize, sampleSize, state) => {
//      val thisResult = run(maxTestSize, sampleSize, state)
//      if (thisResult.isFalsified) thisResult
//      else other.run(maxTestSize, sampleSize, state)
//    }
//  )
//
//  /**
//    * Constructs a proposition that is falsified when both this and other
//    * are falsified
//    * Exercise 8.9
//    */
//  def ||(other: => Prop): Prop = Prop(
//    (maxSize, sampleSize, state) => {
//      val thisResult = run(maxSize, sampleSize, state)
//      if (thisResult.notFalsified) thisResult
//      else other.run(maxSize, sampleSize, state)
//    }
//  )
//
//}
//
////object Prop {
////
////  type SuccessCount = Int
////
////  /**
////    * This is the maximum size of each test point, not the size of the sampler.
////    * What the meaning of the size is depends on the definition of unSizedSampler.
////    * For instance it could be the length of a list, or the order of magnitude of
////    * an integer and so on.
////    */
////  type MaxTestSize = Int
////
////  type SampleSize = Int
////
////  type FailedCase = String
////
////  sealed trait CheckResult {
////    def isFalsified: Boolean
////    val notFalsified: Boolean = !isFalsified
////  }
////
////  case object Passed extends CheckResult {
////    val isFalsified: Boolean = false
////  }
////
////  case object Proved extends CheckResult {
////    val isFalsified: Boolean = false
////  }
////
////  case class Falsified(failure: FailedCase, successes: SuccessCount) extends CheckResult {
////    val isFalsified: Boolean = true
////  }
////
////  /**
////    * It creates a proposition which is proved if the predicate p
////    * holds and falsified otherwise
////    */
////  def prove(p: => Boolean): Prop = Prop(
////    (_, _, _) => if (p) Proved else Falsified("()", 0)
////  )
////}
//
//object Prop {
//  type SuccessCount = Int
//
//  /**
//    * This is the maximum size of each test point, not the size of the sampler.
//    * What the meaning of the size is depends on the definition of unSizedSampler.
//    * For instance it could be the length of a list, or the order of magnitude of
//    * an integer and so on.
//    */
//  type MaxTestSize = Int
//
//  type SampleSize = Int
//
//  type FailedCase = String
//
//  sealed trait CheckResult {
//    def isFalsified: Boolean
//    val passed: Boolean = !isFalsified
//  }
//
//  case object Passed extends CheckResult {
//    val isFalsified: Boolean = false
//  }
//
//  case object Proved extends CheckResult {
//    val isFalsified: Boolean = false
//  }
//
//  case class Falsified(failure: FailedCase, successes: SuccessCount) extends CheckResult {
//    val isFalsified: Boolean = true
//  }
//
//}
//
//object Sampler {
//
//  /**
//    * Specialized chooseInInterval to integers. Requires a numSamples
//    * Exercise 8.4
//    */
//  def intInInterval(lb: Int, ub: Int): Sampler[Int] =
//    Sampler(
//      nextNonNegativeIntLessThan(ub - lb),
//      Some(Stream.unfold(lb)(n => if (n < ub) Some((n, n + 1)) else None)),
//      Some((ub - lb).toLong)
//    ).map(_ + lb)
//
//  /**
//    * Given a thunk it generates test samples all with the same value t
//    * Exercise 8.5
//    */
//  def unit[T](t: => T): Sampler[T] =
//    Sampler(
//      StateAction.unit[RandState, T](t),
//      Some(Stream(t)),
//      Some(1L)
//    )
//
//  /**
//    * Generates booleans.
//    * Exercise 8.5
//    */
//  def boolean: Sampler[Boolean] =
//    Sampler(
//      nextNonNegativeIntLessThan(2).map(i => if (i == 0) false else true),
//      Some(Stream(true, false)),
//      Some(2L))
//
//  /**
//    * Generates pairs of integers each in the interval [lb, ub)
//    */
//  def intPairInInterval(lb: Int, ub: Int): Sampler[(Int, Int)] = {
//    intInInterval(lb, ub).listOf(unit(2)).
//      map(x => (x.head, x(1)))
//  }
//
//  /**
//    * Generates doubles in [0, 1)
//    * Exercise 8.8
//    */
//  def double: Sampler[Double] = Sampler(StateAction.nextDouble)
//
//  /**
//    * Generates a random string of length length made with the characters
//    * in alphabet
//    */
//  def string(maxLength: Int)
//            (implicit alphabet: IndexedSeq[Char]): Sampler[String] =
//    intInInterval(0, alphabet.length - 1)
//      .listOf(intInInterval(0, maxLength + 1))
//      .map(_.map(alphabet).mkString)
//
//  /**
//    * Given a sample (exhaustive or not) it checks if the predicate p applies to
//    * all examples and returns a CheckResult, as either Falsified or Passed.
//    */
//  def falsify[T](samples: Stream[T], p: T => Boolean): Option[Prop.CheckResult] = {
//    val outcomes = samples.zip(from(0)).map {
//      case (t, count) => try {
//        if (p(t)) Prop.Passed else Prop.Falsified(t.toString, count)
//      }
//      catch {
//        case e: Exception => Prop.Falsified(buildMessageFromException(e, t), count)
//      }
//    }
//    outcomes.find(_.isFalsified)
//  }
//
//  private def buildMessageFromException[T](e: Exception, t: T): String =
//    s"test case $t\ngenerated an exception ${e.getMessage}\nwith stack trace:\n" +
//      s"${e.getStackTrace.mkString}\n"
//
//  /*********************** OLD ROUTINES
//    *
//    */
//
//  /**
//    * Given a way to generate examples of type T, it generates examples of List[T]
//    * of length n.
//    * Exercise 8.5
//    */
//  def listOfN[T](sample: Sampler[T], n: Int): Sampler[List[T]] =
//    Sampler(sequence(List.fill(n)(sample.sample)))
//
//}
//
//
