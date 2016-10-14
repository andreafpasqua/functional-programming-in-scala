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

  implicit val numSamples = 100
  implicit val state = SimpleRandomState(0L)
  val one = GenSamples.unit(1)

  println("Test unit")
  assert(one.forall(_ == 1).check == Right(numSamples))
  assert(one.forall(_ != 1).check == Left(1.toString, 0))

  println("Test map")
  val two = one.map(_ + 1)
  val eleven = one.map(i => i.toString + i.toString)
  assert(two.forall(_ == 2).check == Right(numSamples))
  assert(eleven.forall(_ == "11").check == Right(numSamples))

  println("Test chooseInInterval")
  val zeroToThree = GenSamples.chooseInInterval(0, 4)
  assert(zeroToThree.forall(_ < 4).check == Right(numSamples))
  assert(zeroToThree.forall(_ < 0).check == Left("0", 0))
  assert(zeroToThree.forall(_ < 2).check == Left("2", 64))

  println("Test boolean")
  val booleans = GenSamples.boolean
  assert(booleans.forall(identity).check == Left(false.toString, 54))

  println("Test listOfN")
  val lists = GenSamples.listOfN(GenSamples.chooseInInterval(0, 4), 10)
  assert(lists.forall(_.length == 10).check == Right(numSamples))
  assert(lists.forall(_.forall(_ < 4)).check == Right(numSamples))
  assert(lists.forall(_.forall(_ < 2)).check ==
    Left(List(0, 2, 1, 1, 2, 1, 1, 1, 1, 0).toString, 3))

  println("Test &&")
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check == Right(2 * numSamples))
  assert((zeroToThree.forall(_ < 0) && zeroToThree.forall(_ >= 0)).check == Left("0", 0))
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check == Left("2", 0))

  println("Test ||")

}
