package andrea.scala.functional.programming.testing

import andrea.scala.functional.programming.state.SimpleRandomState
import andrea.scala.functional.programming.option._
import andrea.scala.functional.programming.state.StateAction.RandAction

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
  val sixToTen = Sampler.intInInterval(6, 11)
  assert(zeroToFive.population.get.population.toList.toSet == Set(0, 1, 2, 3, 4, 5))
  assert(zeroToFive.population.get.size == 6)
  assert(sixToTen.population.get.population.toList.toSet == Set(6, 7, 8, 9, 10))
  assert(sixToTen.population.get.size == 5)
  assert(zeroToFive.forall(_ < 6).check(maxSize, largeSample, state) == Prop.Proved)
  assert(zeroToFive.forall(_ < 6).check(maxSize, smallSample, state) == Prop.Passed)
  assert(zeroToFive.forall(_ < 3).check(maxSize, largeSample, state).isFalsified)

  println("* Test boolean")
  val booleans = Sampler.boolean
  assert(booleans.forall(identity).check(maxSize, largeSample, state).isFalsified)

  println("* Test listOfN")
  val smallPop = Sampler.listOfN(Sampler.intInInterval(0, 3), 2)
  val setOfLists = Set(List(0, 0), List(0, 1), List(0, 2),
    List(1, 0), List(1, 1), List(1, 2), List(2, 0), List(2, 1), List(2, 2))
  assert(smallPop.population.get.population.toList.toSet == setOfLists)
  val lists = Sampler.listOfN(Sampler.intInInterval(0, 4), 10)
  assert(lists.forall(_.length == 10).check(maxSize, largeSample, state).notFalsified)
  assert(lists.forall(_.forall(_ < 4)).check(maxSize, largeSample, state).notFalsified)
  assert(lists.forall(_.forall(_ < 2)).check(maxSize, largeSample, state).isFalsified)

  println("* Test intPairInInterval")
  val pairs = Sampler.intPairInInterval(0, 2)
  assert(pairs.forall(x => x._1 >= 0 && x._1 < 2).check(maxSize, largeSample, state) == Prop.Proved)
  assert(pairs.forall(x => x._2 >= 0 && x._2 < 2).check(maxSize, largeSample, state) == Prop.Proved)

  println("* Test string")
  val strings = Sampler.string(5)
  println(strings.population.get.size)
  assert(strings.forall(_.length < 5).check(maxSize, largeSample, state).isFalsified)
  assert(strings.forall(_.length < 6).check(maxSize, largeSample, state).notFalsified)
  assert(strings.forall(_.forall(alphabet.contains)).check(maxSize, largeSample, state).notFalsified)

  println("* Test flatMap")
  assert(Sampler.intInInterval(0, 4).flatMap(Sampler.string).forall(_.length < 4).check(maxSize, largeSample, state).notFalsified)
  val randomInts = Sampler.boolean.flatMap(bool => if (bool) Sampler.intInInterval(0, 10) else Sampler.intInInterval(10, 20))
  assert(randomInts.forall(_ < 20).check(maxSize, largeSample, state).notFalsified)
  assert(randomInts.forall(_ < 10).check(maxSize, largeSample, state).isFalsified)

  println("* Test listOf")
  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 5).check(maxSize, largeSample, state) == Prop.Proved)
  assert(Sampler.boolean.listOf(Sampler.intInInterval(0, 5)).forall(_.length < 4).check(maxSize, largeSample, state).isFalsified)

  println("* Test union")
  assert(Sampler.intInInterval(0, 10).union(Sampler.intInInterval(5, 15)).population.get.population.toList.toSet ==
    Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(0, 1)).forall(_ == 0).check(maxSize, largeSample, state) == Prop.Proved)
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ == 0).check(maxSize, largeSample, state).isFalsified)
  assert(Sampler.intInInterval(0, 1).union(Sampler.intInInterval(1, 2)).forall(_ < 2).check(maxSize, largeSample, state) == Prop.Proved)

  println("* Test double")
  assert(Sampler.double.forall(_ < 1).check(maxSize, largeSample, state) == Prop.Passed)
  assert(Sampler.double.forall(_ < 0.5).check(maxSize, largeSample, state).isFalsified)

  println("* Test weighted")
  assert(Sampler.unit(0).weighted(Sampler.unit(1), 0D, 1D).forall(_ == 1).check(maxSize, largeSample, state)
    == Prop.Passed)
  assert(Sampler.unit(0).weighted(Sampler.unit(1), 1D, 0D).forall(_ == 0).check(maxSize, largeSample, state) ==
    Prop.Passed)

  println("* Test forall")
  assert(Sampler.unit(0).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).isFalsified)
  assert(Sampler.unit(1).forall(n => (2 / n) > 0).check(maxSize, largeSample, state).notFalsified)

  println("* Test &&")
  val zeroToThree = Sampler.intInInterval(0, 4)
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Prop.Proved)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state).isFalsified)
  assert((zeroToThree.forall(_ < 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)
  assert((zeroToThree.forall(_ >= 4) && zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)

  println("* Test isFalsified, notFalsified")
  val falsified = Prop.Falsified("0", 1)
  val passed = Prop.Passed
  val proved = Prop.Proved
  assert(falsified.isFalsified)
  assert(!falsified.notFalsified)
  assert(!passed.isFalsified)
  assert(passed.notFalsified)
  assert(!proved.isFalsified)
  assert(proved.notFalsified)

  println("* Test ||")
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Prop.Proved)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ >= 0)).check(maxSize, largeSample, state) == Prop.Proved)
  assert((zeroToThree.forall(_ < 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state) == Prop.Proved)
  assert((zeroToThree.forall(_ >= 4) || zeroToThree.forall(_ < 0)).check(maxSize, largeSample, state).isFalsified)

  println("* Test forall for UnSizedSampler")
  val variousLengths = BySizeSampler(n => Sampler.double.listOf(Sampler.unit(n)))
  val variousSizeInt = BySizeSampler(n => Sampler.intInInterval(0, n + 1))
  println(variousLengths.forall(_.length < 2).check(maxSize, maxSize * largeSample, state)
    .asInstanceOf[Prop.Falsified].failure.split(",").length == 2)
  assert(variousLengths.forall(list => list.headOption.forall(_ < 0.2)).check(maxSize, maxSize * largeSample, state)
    .isFalsified)
  assert(variousSizeInt.forall(_ < 100).check(150, 150 * largeSample, state)
    .asInstanceOf[Prop.Falsified].failure == "100")


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

  assert(variousLengths.forall(maxPredicate1).check(10, 10 * largeSample, state).notFalsified)
  /**
    * Exercise 8.13
    */
  val nonEmptyLists = BySizeSampler(n => Sampler.double.listOf(Sampler.unit(n.max(1))))
  assert(nonEmptyLists.forall(maxPredicate2).check(10, 10 * largeSample, state).notFalsified)

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

  assert(Prop.prove(tautology).check().notFalsified)
  assert(Prop.prove(contradiction).check().isFalsified)
  assert((Prop.prove(tautology) || Prop.prove(contradiction)).check().notFalsified)
  assert((Prop.prove(tautology) && one.forall(_ == 1)).check().notFalsified)

  println("* Test function")
  val intTo5 = Sampler.intInInterval(0, 5)
  val intTo4 = Sampler.intInInterval(0, 4)
  val doubles = Sampler.double
  val intToInt = Sampler.function(intTo5, intTo4)
  val intToDouble = Sampler.function(intTo5, doubles)
  val doubleToInt = Sampler.function(doubles, intTo5)
  assert(intToInt.population.get.size == math.pow(intTo4.population.get.size, intTo5.population.get.size))
  assert(doubleToInt.population.isEmpty)
  assert(intToDouble.population.isEmpty)
  assert((for {int <- intTo5; f <- intToInt} yield f(int))
    .forall(n => n >= 0 & n < 4).check(maxSize, 10 * largeSample, state) == Prop.Proved)
  assert((for {int <- intTo5; f <- intToDouble} yield f(int))
    .forall(d => d >= 0D & d < 1D).check(maxSize, largeSample, state) == Prop.Passed)
  assert((for {d <- doubles; f <- doubleToInt} yield f(d))
    .forall(n => n >= 0 & n < 5).check(maxSize, largeSample, state) == Prop.Passed)

  println("* Test takeWhile properties")
  /**
    * Properties of takeWhile (and dropWhile)
    * Exercise 8.18
    */
  val length = Sampler.intInInterval(0, 101)
  val doubleLists =Sampler.double.listOf(length)
  val doubleToBool = Sampler.function(Sampler.double, Sampler.boolean)
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
  assert(shorter.check(maxSize, largeSample, state) == Prop.Passed)
  assert(beginsWith.check(maxSize, largeSample, state) == Prop.Passed)
  assert(withDropWhile.check(maxSize, largeSample, state) == Prop.Passed)
  assert(nextDoesNotSatisfy.check(maxSize, largeSample, state) == Prop.Passed)

  println("* Test take properties")
  /**
    * Properties of take and drop
    * Exercise 8.20
    */
  val lengthArg = Sampler.intInInterval(-100, 201)
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

  assert(takeProp.check(maxSize, largeSample, state) == Prop.Passed)

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
  assert(isShorter.check(maxSize, largeSample, state) == Prop.Passed)
  assert(satisfiesP.check(maxSize, largeSample, state) == Prop.Passed)
  assert(isContained.check(maxSize, largeSample, state) == Prop.Passed)
  assert(restDoesNotSatisfy.check(maxSize, largeSample, state) == Prop.Passed)


  println("* Test sequence properties")
  /**
    * Properties of sequence
    * Exercise 8.20
    */
  val optionInt = Sampler.intInInterval(0, 100001).map(n => Some(n)).
    weighted(Sampler.unit[Option[Int]](None), .75, .25)
  val optionList = optionInt.listOf(Sampler.intInInterval(0, 101))
  val beforeAndAfterSequence = for {
    list <- optionList
  } yield (list, Option.sequence(list))

  val sequenceProp = beforeAndAfterSequence.forall {
    case (list, sequence) =>
      if (list.exists(x => x.isEmpty)) sequence.isEmpty
      else list.map(_.asInstanceOf[Some[Int]].get) == sequence.asInstanceOf[Some[List[Int]]].get
  }
  assert(sequenceProp.check(maxSize, largeSample, state) == Prop.Passed)

}
