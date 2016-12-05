package andrea.scala.functional.programming.testing

import scala.util.Random

import andrea.scala.functional.programming.option.{Some, None, Option}
import andrea.scala.functional.programming.state.SimpleRandomState

/**
  * Created by andrea on 10/13/16.
  */

object SamplerTest extends App {
  import Prop._
  import Sampler._

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

  val largeSample = 10000
  val smallSample = 5
  val noSample = 0
  val maxSize = 1000
  val state = SimpleRandomState(0L)
  implicit val alphabet = "abcd".toVector
  val one = unit(1)
  val lessThanTen = intInInterval(0, 10)

  println("* Test check")
  assert(lessThanTen.forall(_ < 10).check(maxSize, largeSample, state) == Proved)
  assert(lessThanTen.forall(_ < 10).check(maxSize, smallSample, state) == Passed)
  assert(lessThanTen.forall(_ < 5).check(maxSize, largeSample, state).isFalsified)
  assert(lessThanTen.forall(_ < 5).check(maxSize, noSample, state) == Passed)

  println("* Test unit")
  assert(one.forall(_ == 1).check(maxSize, largeSample, state) == Proved)
  assert(one.forall(_ != 1).check(maxSize, largeSample, state).isFalsified)


  println("* Test map")
  val two = one.map(_ + 1)
  val eleven = one.map(i => i.toString + i.toString)
  val largeInterval = intInInterval(0, 10000001).map(_ * 2)
  assert(two.forall(_ == 2).check(maxSize, largeSample, state) == Proved)
  assert(eleven.forall(_ == "11").check(maxSize, largeSample, state) == Proved)
  assert(largeInterval.forall(_ <= 20000001).check(maxSize, largeSample, state) == Passed)
  assert(largeInterval.forall(1 / _ > 0).check(maxSize, largeSample, state).isFalsified)

  println("* Test intInInterval")
  val zeroToFive = intInInterval(0, 6)
  val sixToTen = intInInterval(6, 11)
  assert(zeroToFive.population.toList.toSet == Set(0, 1, 2, 3, 4, 5).map(Some(_)))
  assert(sixToTen.population.toList.toSet == Set(6, 7, 8, 9, 10).map(Some(_)))
  assert(zeroToFive.forall(_ < 6).check(maxSize, largeSample, state) == Proved)
  assert(zeroToFive.forall(_ < 6).check(maxSize, smallSample, state) == Passed)
  assert(zeroToFive.forall(_ < 3).check(maxSize, largeSample, state).isFalsified)

  println("* Test boolean")
  val booleans = boolean
  assert(booleans.population.toList.toSet == Set(true, false).map(Some(_)))
  assert(booleans.forall(identity).check(maxSize, largeSample, state).isFalsified)

  println("* Test listOfN")
  val smallPop = listOfN(intInInterval(0, 3), 2)
  val setOfLists = Set(List(0, 0), List(0, 1), List(0, 2),
    List(1, 0), List(1, 1), List(1, 2), List(2, 0), List(2, 1), List(2, 2))
  assert(smallPop.population.toList.toSet == setOfLists.map(Some(_)))
  val lists = listOfN(intInInterval(0, 4), 10)
  assert(lists.forall(_.length == 10).check(maxSize, largeSample, state).notFalsified)
  assert(lists.forall(_.forall(_ < 4)).check(maxSize, largeSample, state).notFalsified)
  assert(lists.forall(_.forall(_ < 2)).check(maxSize, largeSample, state).isFalsified)

  println("* Test intPairInInterval")
  val pairs = intPairInInterval(0, 2)
  val setOfPairs = Set((0, 0), (0, 1), (1, 0), (1, 1))
  assert(pairs.population.toList.toSet == setOfPairs.map(Some(_)))
  assert(pairs.forall{ case (x, y) => x >= 0 && x < 2}.check(maxSize, largeSample, state).notFalsified)
  assert(pairs.forall{ case (x, y) => y >= 0 && y < 2}.check(maxSize, largeSample, state).notFalsified)

  println("* Test string")
  val strings = string(5)
  assert(strings.forall(_.length < 5).check(maxSize, largeSample, state).isFalsified)
  assert(strings.forall(_.length < 6).check(maxSize, largeSample, state) == Proved)
  assert(strings.forall(_.forall(alphabet.contains)).check(maxSize, largeSample, state) == Proved)

  println("* Test flatMap")
  assert(
    intInInterval(0, 4).flatMap(string).forall(_.length < 4)
      .check(maxSize, largeSample, state).notFalsified)
  val randomInts = boolean.flatMap(bool => if (bool) intInInterval(0, 10) else intInInterval(10, 20))
  assert(randomInts.forall(_ < 20).check(maxSize, largeSample, state).notFalsified)
  assert(randomInts.forall(_ < 10).check(maxSize, largeSample, state).isFalsified)
  val sameString = string(100).flatMap (s1 => unit("ciao"))
  assert(sameString.forall(_ == "ciao").check(maxSize, largeSample, state).notFalsified)

  println("* Test listOf")
  val booleanList = boolean.listOf(intInInterval(0, 5))
  assert(booleanList.forall(_.length < 5).check(maxSize, largeSample, state) == Proved)
  assert(booleanList.forall(_.length < 4).check(maxSize, largeSample, state).isFalsified)

  println("* Test union")
  assert(intInInterval(0, 10).union(intInInterval(5, 15)).population.toList.toSet ==
    Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14).map(Some(_)))
  assert(intInInterval(0, 1).union(intInInterval(0, 1)).forall(_ == 0)
    .check(maxSize, largeSample, state) == Proved)
  assert(intInInterval(0, 1).union(intInInterval(1, 2)
    ).forall(_ == 0).check(maxSize, largeSample, state).isFalsified)
  assert(intInInterval(0, 1).union(intInInterval(1, 2)
    ).forall(_ < 2).check(maxSize, largeSample, state).notFalsified)

  println("* Test double")
  assert(double.forall(_ < 1).check(maxSize, largeSample, state) == Passed)
  assert(double.forall(_ < 0.5).check(maxSize, largeSample, state).isFalsified)
  assert(double.population.toList == List(None))

  println("* Test weighted")
  assert(unit(0).weighted(unit(1), 0D, 1D).forall(_ == 1).check(maxSize, largeSample, state)
    == Passed)
  assert(unit(0).weighted(unit(1), 1D, 0D).forall(_ == 0).check(maxSize, largeSample, state) ==
    Passed)

  println("* Test forall")
  assert(unit(0).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).isFalsified)
  assert(unit(1).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).notFalsified)

  println("* Test &&")
  val zeroToThree = intInInterval(0, 4)
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Proved)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).isFalsified)
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)

  println("* Test ||")
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Proved)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Proved)
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state) == Proved)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)

  println("* Test isFalsified, notFalsified")
  val falsified = Falsified("0", 1)
  val passed = Passed
  val proved = Proved
  assert(falsified.isFalsified)
  assert(!falsified.notFalsified)
  assert(!passed.isFalsified)
  assert(passed.notFalsified)
  assert(!proved.isFalsified)
  assert(proved.notFalsified)

  println("* Test forall for UnSizedSampler")
  val variousLengths = BySizeSampler(n => double.listOf(unit(n)))
  val variousSizeInt = BySizeSampler(n => intInInterval(0, n + 1))
  assert(variousLengths.forall(_.length < 2).check(maxSize, largeSample, state)
    .asInstanceOf[Falsified].failure.split(",").length == 2)
  assert(variousLengths.forall(list => list.headOption.forall(_ < 0.2)).check(maxSize, largeSample, state)
    .isFalsified)
  assert(variousSizeInt.forall(_ < 100).check(150, largeSample, state).isFalsified)

  println("* Test max properties")

  def maxPredicate1(list: List[Double]) = if (list.isEmpty) true
  else {
    val max = list.max
    list.forall(_ <= max)
  }

  def maxPredicate2(list: List[Double]) = {
    val max = list.max
    list.forall(_ <= max)
  }

  assert(variousLengths.forall(maxPredicate1).check(10, largeSample, state).notFalsified)
  /**
    * Exercise 8.13
    */
  val nonEmptyLists = BySizeSampler(n => double.listOf(unit(n.max(1))))
  assert(nonEmptyLists.forall(maxPredicate2).check(10, largeSample, state).notFalsified)

  println("* Test sort properties")

  /**
    * Exercise 8.14
    */
  def maxSortPredicate(list: List[Double]) = list.sorted.last == list.max

  def sortPredicate1(list: List[Double]) = Random.shuffle(list).sorted == list.sorted

  def sortPredicate2(list: List[Double]) = list.sorted.length < 2 || list.sorted.sliding(2).forall { case List(i, j) => i <= j }

  def sortPredicate3(list: List[Double]) = list.sorted.forall(list.contains)

  def sortPredicate4(list: List[Double]) = list.forall(list.sorted.contains)

  assert(nonEmptyLists.forall(maxSortPredicate).check(10, 10 * largeSample, state).notFalsified)
  assert(nonEmptyLists.forall(sortPredicate1).check(10, 10 * largeSample, state).notFalsified)
  assert((nonEmptyLists.forall(sortPredicate2) &&
    nonEmptyLists.forall(sortPredicate3)
    && nonEmptyLists.forall(sortPredicate4)).check(10, 10 * largeSample, state).notFalsified)

  println("* Test prove")

  def tautology = 3 + 2 == 5

  def contradiction = 3 + 2 == 4

  assert(prove(tautology).check() == Proved)
  assert(prove(contradiction).check().isFalsified)
  assert((prove(tautology) || prove(contradiction)).check() == Proved)
  assert((prove(tautology) && one.forall(_ == 1)).check() == Proved)

  println("* Test function")
  val intTo5 = intInInterval(0, 5)
  val intTo4 = intInInterval(0, 4)
  val doubles = double
  val intToInt = function(intTo5, intTo4)
  val intToDouble = function(intTo5, doubles)
  val doubleToInt = function(doubles, intTo5)
  assert(doubleToInt.population.head.isEmpty)
  assert(intToDouble.population.head.isEmpty)
  assert((for {int <- intTo5; f <- intToInt} yield f(int))
    .forall(n => n >= 0 & n < 4).check(maxSize, 10 * largeSample, state).notFalsified)
  assert((for {int <- intTo5; f <- intToDouble} yield f(int))
    .forall(d => d >= 0D & d < 1D).check(maxSize, largeSample, state) == Passed)
  assert((for {d <- doubles; f <- doubleToInt} yield f(d))
    .forall(n => n >= 0 & n < 5).check(maxSize, largeSample, state) == Passed)

  println("* Test takeWhile and dropWhile properties")
  /**
    * Properties of takeWhile (and dropWhile)
    * Exercise 8.18
    */
  val length = intInInterval(0, 101)
  val doubleLists =double.listOf(length)
  val doubleToBool = function(double, boolean)
  val beforeAndAfter = for {
    doubleList <- doubleLists
    predicate <- doubleToBool
  } yield (doubleList, predicate, doubleList.takeWhile(predicate), doubleList.dropWhile(predicate))
  val shorter = beforeAndAfter.forall {
    case (original, _, takeWhile, _) => takeWhile.length <= original.length}
  val beginsWith = beforeAndAfter.forall {
    case (original, _, takeWhile, _) => original.splitAt(takeWhile.length)._1 == takeWhile}
  val withDropWhile = beforeAndAfter.forall {
    case (original, _, takeWhile, dropWhile) => original == takeWhile ++ dropWhile
  }
  val nextDoesNotSatisfy = beforeAndAfter.forall {
    case (original, p, takeWhile, _) => original.splitAt(takeWhile.length)._2.headOption.forall(d => !p(d))
  }
  assert(shorter.check(maxSize, largeSample, state) == Passed)
  assert(beginsWith.check(maxSize, largeSample, state) == Passed)
  assert(withDropWhile.check(maxSize, largeSample, state) == Passed)
  assert(nextDoesNotSatisfy.check(maxSize, largeSample, state) == Passed)

  println("* Test take properties")
  /**
    * Properties of take and drop
    * Exercise 8.20
    */
  val lengthArg = intInInterval(-100, 201)
  val beforeAndAfterTakeOrDrop = for {
    doubleList <- doubleLists
    n <- lengthArg
  } yield (doubleList, n, doubleList.take(n), doubleList.drop(n))
  val takeProp = beforeAndAfterTakeOrDrop.forall {
    case (l, n, taken, dropped) =>
      val prop1 =
        if (n <= 0) taken.isEmpty && dropped == l
        else if (n > l.length) l == taken && dropped.isEmpty
        else l.splitAt(n)._1 == taken && l.splitAt(n)._2 == dropped
      val prop2 = taken ++ dropped == l
      prop1 && prop2
  }
  assert(takeProp.check(maxSize, largeSample, state) == Passed)

  println("* Test filter properties")
  /**
    * Properties of filter
    * Exercise 8.20
    */
  val beforeAndAfterFilter = for {
    doubleList <- doubleLists
    predicate <- doubleToBool
  } yield (doubleList, predicate, doubleList.filter(predicate))
  val isShorter = beforeAndAfterFilter.forall {
    case (original, _, filtered) => filtered.length <= original.length}
  val satisfiesP = beforeAndAfterFilter.forall {
    case (_, p, filtered) => filtered.forall(p)}
  val isContained = beforeAndAfterFilter.forall {
    case (original, _, filtered) => filtered.toSet.subsetOf(original.toSet)}
  val restDoesNotSatisfy = beforeAndAfterFilter.forall {
    case (original, p, filtered) => !original.toSet.diff(filtered.toSet).exists(p)}
  assert(isShorter.check(maxSize, largeSample, state) == Passed)
  assert(satisfiesP.check(maxSize, largeSample, state) == Passed)
  assert(isContained.check(maxSize, largeSample, state) == Passed)
  assert(restDoesNotSatisfy.check(maxSize, largeSample, state) == Passed)

  println("* Test sequence properties")
  /**
    * Properties of sequence
    * Exercise 8.20
    */
  val optionInt = intInInterval(0, 100001).map(n => Some(n)).
    weighted(unit[Option[Int]](None), .75, .25)
  val optionList = optionInt.listOf(intInInterval(0, 101))
  val beforeAndAfterSequence = for {
    list <- optionList
  } yield (list, Option.sequence(list))

  val sequenceProp = beforeAndAfterSequence.forall {
    case (list, sequence) =>
      if (list.exists(x => x.isEmpty)) sequence.isEmpty
      else list.map(_.asInstanceOf[Some[Int]].get) == sequence.asInstanceOf[Some[List[Int]]].get
  }
  assert(sequenceProp.check(maxSize, largeSample, state) == Passed)

}
