package andrea.scala.functional.programming.testgeneration

import andrea.scala.functional.programming.state.SimpleRandomState

/**
  * Created by andrea on 10/13/16.
  */

object GenSamplesTest extends App {

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

  implicit val numSamples = 1000
  implicit val state = SimpleRandomState(0L)
  implicit val alphabet = "abcd".toVector
  val one = GenSamples.unit(1)

  println("Test unit")
  assert(one.forall(_ == 1).check == Right(numSamples))
  assert(one.forall(_ != 1).check == Left(1.toString, 0))

  println("Test map")
  val two = one.map(_ + 1)
  val eleven = one.map(i => i.toString + i.toString)
  assert(two.forall(_ == 2).check == Right(numSamples))
  assert(eleven.forall(_ == "11").check == Right(numSamples))

  println("Test intInInterval")
  val zeroToThree = GenSamples.intInInterval(0, 4)
  assert(zeroToThree.forall(_ < 3).check == Right(numSamples))
  assert(zeroToThree.forall(_ < 0).check == Left("0", 0))

  println("Test boolean")
  val booleans = GenSamples.boolean
//  assert(booleans.forall(identity).check == Left(false.toString, 54))

  println("Test listOfN")
  val lists = GenSamples.listOfN(GenSamples.intInInterval(0, 4), 10)
  assert(lists.forall(_.length == 10).check == Right(numSamples))
  assert(lists.forall(_.forall(_ < 4)).check == Right(numSamples))
//  assert(lists.forall(_.forall(_ < 2)).check ==
//    Left(List(0, 2, 1, 1, 2, 1, 1, 1, 1, 0).toString, 3))

  println("Test intPairInInterval")
  val pairs = GenSamples.intPairInInterval(0, 2)
  assert(pairs.forall(x => x._1 >= 0 && x._1 < 2).check == Right(numSamples))
  assert(pairs.forall(x => x._2 >= 0 && x._2 < 2).check == Right(numSamples))

  println("Test string")
  val strings = GenSamples.string(10)
  assert(strings.forall(_.length == 10).check == Right(numSamples))
  assert(strings.forall(_.forall(alphabet.contains)).check == Right(numSamples))

  println("Test flatMap")
  assert(one.flatMap(GenSamples.string).forall(_ => true).check == Right(numSamples))
  assert(GenSamples.intInInterval(0, 4).flatMap(GenSamples.string)
      .forall(_.length < 4).check == Right(numSamples))
  println(GenSamples.intInInterval(0, 4)
    .forall(_ < 3).check)


  //  println("Test &&")
//  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check == Right(2 * numSamples))
//  assert((zeroToThree.forall(_ < 0) && zeroToThree.forall(_ >= 0)).check == Left("0", 0))
//  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check == Left("2", 0))
//
//  println("Test ||")

}
