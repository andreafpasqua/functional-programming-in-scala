package andrea.scala.functional.programming.newtesting

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

  val largeSample = 1000
  val smallSample = 5
  val noSample = 0
  val maxSize = 1000
  val state = SimpleRandomState(0L)
  implicit val alphabet = "abcd".toVector
  val one = Sampler.unit(1)
  val lessThanTen = Sampler.intInInterval(0, 10)


  println("* Test check")
  assert(lessThanTen.forall(_ < 10).check(maxSize, largeSample, state) == Prop.Proved)
  assert(lessThanTen.forall(_ < 10).check(maxSize, smallSample, state) == Prop.Passed)
  assert(lessThanTen.forall(_ < 5).check(maxSize, largeSample, state).isFalsified)
  assert(lessThanTen.forall(_ < 5).check(maxSize, noSample, state) == Prop.Passed)

  println("* Test unit")
  assert(one.forall(_ == 1).check(maxSize, largeSample, state) == Prop.Proved)
  assert(one.forall(_ != 1).check(maxSize, largeSample, state).isFalsified)


  println("* Test map")
  val two = one.map(_ + 1)
  val eleven = one.map(i => i.toString + i.toString)
  assert(two.forall(_ == 2).check(maxSize, largeSample, state) == Prop.Proved)
  assert(eleven.forall(_ == "11").check(maxSize, largeSample, state) == Prop.Proved)

  println("* Test intInInterval")
  val zeroToFive = Sampler.intInInterval(0, 6)
  assert(zeroToFive.forall(_ < 6).check(maxSize, largeSample, state) == Prop.Proved)
  assert(zeroToFive.forall(_ < 6).check(maxSize, smallSample, state) == Prop.Passed)
  assert(zeroToFive.forall(_ < 3).check(maxSize, largeSample, state).isFalsified)

  println("* Test boolean")
  val booleans = Sampler.boolean
  assert(booleans.forall(identity).check(maxSize, largeSample, state).isFalsified)

//  println("* Test listOfN")
//  val lists = Sampler.listOfN(Sampler.intInInterval(0, 4), 2)
//  assert(lists.forall(_.length == 10).check(maxSize, largeSample, state).notFalsified)
//  assert(lists.forall(_.forall(_ < 4)).check(maxSize, largeSample, state).notFalsified)
//  assert(lists.forall(_.forall(_ < 2)).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test intPairInInterval")
//  val pairs = Sampler.intPairInInterval(0, 2)
// assert(pairs.forall(x => x._1 >= 0 && x._1 < 2).check(maxSize, largeSample, state).notFalsified)
//  assert(pairs.forall(x => x._2 >= 0 && x._2 < 2).check(maxSize, largeSample, state).notFalsified)
//
//  println("Test string")
//  val strings = Sampler.string(10)
//  assert(strings.forall(_.length < 10).check(maxSize, largeSample, state).isFalsified)
//  assert(strings.forall(_.length < 11).check(maxSize, largeSample, state).notFalsified)
//  assert(strings.forall(_.forall(alphabet.contains)).check(maxSize, largeSample, state).notFalsified)
//
//  println("Test flatMap")
//  assert(Sampler.intInInterval(0, 4).flatMap(Sampler.string).forall(_.length < 4).check(maxSize, largeSample, state).notFalsified)
//  val randomInts = Sampler.boolean.flatMap(bool => if (bool) Sampler.intInInterval(0, 10) else Sampler.intInInterval(10, 20))
//  assert(randomInts.forall(_ < 20).check(maxSize, largeSample, state).notFalsified)
//  assert(randomInts.forall(_ < 10).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test listOf")
//  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 5).check(maxSize, largeSample, state).notFalsified)
//  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 4).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test union")
//  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(0, 1)).forall(_ == 0).check(maxSize, largeSample, state).notFalsified)
//  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ == 0).check(maxSize, largeSample, state).isFalsified)
//  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ < 2).check(maxSize, largeSample, state).notFalsified)
//
//  println("Test double")
//  assert(Sampler.double.forall(_ < 1).check(maxSize, largeSample, state).notFalsified)
//  assert(Sampler.double.forall(_ < 0.5).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test weighted")
//  assert(Sampler.unit(0).weighted(Sampler.unit(1), 0D, 1D).forall(_ == 1).check(maxSize, largeSample, state).notFalsified)
//  assert(Sampler.unit(0).weighted(Sampler.unit(1), 1D, 0D).forall(_ == 0).check(maxSize, largeSample, state).notFalsified)
//
//  println("Test forall")
//  assert(Sampler.unit(0).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).isFalsified)
//  assert(Sampler.unit(1).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).notFalsified)
//
//  println("Test &&")
//  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).notFalsified)
//  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).isFalsified)
//  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)
//  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test ||")
//  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).notFalsified)
//  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).notFalsified)
//  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).notFalsified)
//  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)
//
//  println("Test forall for UnSizedSampler")
//  val variousLengths = BySizeSampler(n => Sampler.double.listOf(Sampler.unit(n)))
//  def maxPredicate1(list: List[Double]) = if (list.isEmpty) true else {
//    val max = list.max
//    list.forall(_ <= max)
//  }
//  assert(variousLengths.forall(_.length < 2).check(maxSize, maxSize * largeSample, state).isFalsified)
//  assert(variousLengths.forall(list => list.headOption.forall(_ < 0.2)).check(maxSize, maxSize * largeSample, state).isFalsified)
//  assert(variousLengths.forall(maxPredicate1).check(10, 10 * largeSample, state).notFalsified)
//
//  def maxPredicate2(list: List[Double]) = {
//    val max = list.max
//    list.forall(_ <= max)
//  }
//
//  /**
//    * Exercise 8.13
//    */
//  val nonEmptyLists = BySizeSampler(n => Sampler.double.listOf(Sampler.unit(n.max(1))))
//  assert(nonEmptyLists.forall(maxPredicate2).check(10, 10 * largeSample, state).notFalsified)
//
//  /**
//    * Exercise 8.14
//    */
//  def maxSortPredicate(list: List[Double]) = list.sorted.last == list.max
//  def sortPredicate1(list: List[Double]) = Random.shuffle(list).sorted == list.sorted
//  def sortPredicate2(list: List[Double]) = list.sorted.length < 2 || list.sorted.sliding(2).forall{ case List(i, j) => i <= j }
//  def sortPredicate3(list: List[Double]) = list.sorted.forall(list.contains)
//  def sortPredicate4(list: List[Double]) = list.forall(list.sorted.contains)
//  assert(nonEmptyLists.forall(maxSortPredicate).check(10, 10 * largeSample, state).notFalsified)
//  assert(nonEmptyLists.forall(sortPredicate1).check(10, 10 * largeSample, state).notFalsified)
//  assert((nonEmptyLists.forall(sortPredicate2) &&
//    nonEmptyLists.forall(sortPredicate3)
//    && nonEmptyLists.forall(sortPredicate4)).check(10, 10 * largeSample, state).notFalsified)
//
//  println("Test prove")
//  def tautology = 3 + 2 == 5
//  def contradiction = 3 + 2 == 4
//  assert(Prop.prove(tautology).check().notFalsified)
//  assert(Prop.prove(contradiction).check().isFalsified)
//  assert((Prop.prove(tautology) || Prop.prove(contradiction)).check().notFalsified)
//  assert((Prop.prove(tautology) &&  one.forall(_ == 1)).check().notFalsified)

}
