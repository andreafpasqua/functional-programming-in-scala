package andrea.scala.functional.programming.testing

/**
  * Created by andreapasqua on 10/17/2016.
  */

case class BySizeSampler[+T](forSize: Int => Sampler[T]){

  /**
    * Given an unsized sampler, it constructs a proposition
    * that holds true if p holds for all samples generated.
    * The samples are generated in equal numbers for each
    * of the test point sizes ranging from 0 to a max of testSize
    * (the first argument of check. What
    */
  def forall(p: T => Boolean): Prop = Prop(
    (maxSize, numSamples, state) => {
      val samplesPerSize = (numSamples + maxSize - 1) / maxSize // the total number of samples >= numSamples
      val allProps = for (
          size <- (0 to 1 + maxSize.min(numSamples)).toList
        ) yield {
          forSize(size).forall(p)
        }
      allProps.reduce(_ && _).run(maxSize, samplesPerSize, state)
      // note that run ignores maxSize here since the propositions
      // where constructed by regular Samplers.
    }
  )

  /**
    * Maps samples generated to new values using f for the mapping.
    * Exercise 8.11
    */
  def map[S](f: T => S): BySizeSampler[S] = BySizeSampler(n => forSize(n).map(f))

  /**
    * given a function f that generates samples of s for a given t,
    * it turns sampled ts into sampled ss. Notice that the number of
    * sampled points doesn't change.
    * Exercise 8.11
    */
  def flatMap[S](f: T => BySizeSampler[S]): BySizeSampler[S] =
    BySizeSampler(n => forSize(n).flatMap(f(_).forSize(n)))

  /**
    * given a sampler of integers it constructs lists of length equal to the integers sampled
    * Exercise 8.12
    */
  def listOf(size: BySizeSampler[Int]): BySizeSampler[List[T]] = BySizeSampler(
    n => forSize(n).listOf(size.forSize(n))
  )

  /**
    * It samples randomly from this or from other
    * Exercise 8.11
    */
  def union[TT >: T](other: BySizeSampler[TT]): BySizeSampler[TT] = BySizeSampler(
    n => forSize(n).union(other.forSize(n))
  )

  /**
    * It samples from this or from other in proportion to thisWeight and otherWeight respectively.
    * thisWeight and otherWeight are nonnegative
    * Exercise 8.11
    */
  def weighted[TT >: T](other: BySizeSampler[TT], thisWeight: Double, otherWeight: Double)
  : BySizeSampler[TT] = BySizeSampler(
    n => forSize(n).weighted(other.forSize(n), thisWeight, otherWeight)
  )

}
