package andrea.scala.functional.programming.testgeneration

import andrea.scala.functional.programming.state.SimpleRandomState

import scala.util.Random

/**
  * Created by andrea on 10/13/16.
  */

object SamplerTest extends App {

  /**
    * Some properties of a sum of integers are the following:
    * sum -> List[Int].sum
    * a. independent of order: List[Int].sum == List[Int].sorted.sum
    * b1. linear in the arguments. List[Int].map(_ * k).sum == List[Int].sum * k
    * b2. linear in the arguments. list1.zip(list2).map(_ + _).sum = list1.sum + list2.sum
    * c. List(1).sum == 1
    * d. sum(list1.sum, list2.sum) == (list1 ++ list2).sum
    *
    * Exercise 8.1
    */

  /**
    * Some properties of a max of integers are the following:
    * max -> List[Int].max
    * a. independent of order: List[Int].max == List[Int].sorted.max
    * b. how it behaves under scalar multiplication. List[Int].map(_ * k).max == List[Int].max * k
    * c. List(1).max == 1
    * d. max(list1.max, list2.max) == (list1 ++ list2).max
    *
    * Exercise 8.2
    */

  val numSamples = 1000
  val maxSize = 1000
  val state = SimpleRandomState(0L)
  implicit val alphabet = "abcd".toVector
  val one = Sampler.unit(1)

  println("Test unit")
  assert(one.forall(_ == 1).check(maxSize, numSamples, state).passed)
  assert(one.forall(_ != 1).check(maxSize, numSamples, state).isFalsified)

  println("Test map")
  val two = one.map(_ + 1)
  val eleven = one.map(i => i.toString + i.toString)
  assert(two.forall(_ == 2).check(maxSize, numSamples, state).passed)
  assert(eleven.forall(_ == "11").check(maxSize, numSamples, state).passed)

  println("Test intInInterval")
  val zeroToThree = Sampler.intInInterval(0, 4)
  assert(zeroToThree.forall(_ < 4).check(maxSize, numSamples, state).passed)
  assert(zeroToThree.forall(_ < 3).check(maxSize, numSamples, state).isFalsified)

  println("Test boolean")
  val booleans = Sampler.boolean
  assert(booleans.forall(identity).check(maxSize, numSamples, state).isFalsified)

  println("Test listOfN")
  val lists = Sampler.listOfN(Sampler.intInInterval(0, 4), 10)
  assert(lists.forall(_.length == 10).check(maxSize, numSamples, state).passed)
  assert(lists.forall(_.forall(_ < 4)).check(maxSize, numSamples, state).passed)
  assert(lists.forall(_.forall(_ < 2)).check(maxSize, numSamples, state).isFalsified)

  println("Test intPairInInterval")
  val pairs = Sampler.intPairInInterval(0, 2)
  assert(pairs.forall(x => x._1 >= 0 && x._1 < 2).check(maxSize, numSamples, state).passed)
  assert(pairs.forall(x => x._2 >= 0 && x._2 < 2).check(maxSize, numSamples, state).passed)

  println("Test string")
  val strings = Sampler.string(10)
  assert(strings.forall(_.length < 10).check(maxSize, numSamples, state).isFalsified)
  assert(strings.forall(_.length < 11).check(maxSize, numSamples, state).passed)
  assert(strings.forall(_.forall(alphabet.contains)).check(maxSize, numSamples, state).passed)

  println("Test flatMap")
  assert(Sampler.intInInterval(0, 4).flatMap(Sampler.string).forall(_.length < 4).check(maxSize, numSamples, state).passed)
  val randomInts = Sampler.boolean.flatMap(bool => if (bool) Sampler.intInInterval(0, 10) else Sampler.intInInterval(10, 20))
  assert(randomInts.forall(_ < 20).check(maxSize, numSamples, state).passed)
  assert(randomInts.forall(_ < 10).check(maxSize, numSamples, state).isFalsified)

  println("Test listOf")
  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 5).check(maxSize, numSamples, state).passed)
  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 4).check(maxSize, numSamples, state).isFalsified)

  println("Test union")
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(0, 1)).forall(_ == 0).check(maxSize, numSamples, state).passed)
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ == 0).check(maxSize, numSamples, state).isFalsified)
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ < 2).check(maxSize, numSamples, state).passed)

  println("Test double")
  assert(Sampler.double.forall(_ < 1).check(maxSize, numSamples, state).passed)
  assert(Sampler.double.forall(_ < 0.5).check(maxSize, numSamples, state).isFalsified)

  println("Test weighted")
  assert(Sampler.unit(0).weighted(Sampler.unit(1), 0D, 1D).forall(_ == 1).check(maxSize, numSamples, state).passed)
  assert(Sampler.unit(0).weighted(Sampler.unit(1), 1D, 0D).forall(_ == 0).check(maxSize, numSamples, state).passed)

  println("Test forall")
  assert(Sampler.unit(0).forall(n => (2 / n) > 0).check(maxSize, numSamples, state).isFalsified)
  assert(Sampler.unit(1).forall(n => (2 / n) > 0).check(maxSize, numSamples, state).passed)

  println("Test &&")
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check(maxSize, numSamples, state).passed)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ >= 0)).check(maxSize, numSamples, state).isFalsified)
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check(maxSize, numSamples, state).isFalsified)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ < 0)).check(maxSize, numSamples, state).isFalsified)

  println("Test ||")
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ >= 0)).check(maxSize, numSamples, state).passed)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ >= 0)).check(maxSize, numSamples, state).passed)
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ < 0)).check(maxSize, numSamples, state).passed)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ < 0)).check(maxSize, numSamples, state).isFalsified)

  println("Test forall for UnSizedSampler")
  val variousLengths = UnSizedSampler(n => Sampler.double.listOf(Sampler.unit(n)))
  def maxPredicate1(list: List[Double]) = if (list.isEmpty) true else {
    val max = list.max
    list.forall(_ <= max)
  }
  assert(variousLengths.forall(_.length < 2).check(maxSize, maxSize * numSamples, state).isFalsified)
  assert(variousLengths.forall(list => list.headOption.forall(_ < 0.2)).check(maxSize, maxSize * numSamples, state).isFalsified)
  assert(variousLengths.forall(maxPredicate1).check(10, 10 * numSamples, state).passed)

  def maxPredicate2(list: List[Double]) = {
    val max = list.max
    list.forall(_ <= max)
  }

  /**
    * Exercise 8.13
    */
  val nonEmptyLists = UnSizedSampler(n => Sampler.double.listOf(Sampler.unit(n.max(1))))
  assert(nonEmptyLists.forall(maxPredicate2).check(10, 10 * numSamples, state).passed)

  /**
    * Exercise 8.14
    */
  def maxSortPredicate(list: List[Double]) = list.sorted.last == list.max
  def sortPredicate1(list: List[Double]) = Random.shuffle(list).sorted == list.sorted
  def sortPredicate2(list: List[Double]) = list.sorted.length < 2 || list.sorted.sliding(2).forall{ case List(i, j) => i <= j }
  def sortPredicate3(list: List[Double]) = list.sorted.forall(list.contains)
  def sortPredicate4(list: List[Double]) = list.forall(list.sorted.contains)
  assert(nonEmptyLists.forall(maxSortPredicate).check(10, 10 * numSamples, state).passed)
  assert(nonEmptyLists.forall(sortPredicate1).check(10, 10 * numSamples, state).passed)
  assert((nonEmptyLists.forall(sortPredicate2) &&
    nonEmptyLists.forall(sortPredicate3)
    && nonEmptyLists.forall(sortPredicate4)).check(10, 10 * numSamples, state).passed)

}
