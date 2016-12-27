package andrea.scala.functional.programming.monoid

import org.scalatest.FunSuite
import andrea.scala.functional.programming.state.SimpleRandomState
import andrea.scala.functional.programming.testing.{Prop, Sampler}

/**
  * Created by andrea on 11/16/16.
  */

class MonoidTest extends FunSuite {
  
  import Monoid._

  val maxSize = 100
  val numSamples = 1000
  val state = SimpleRandomState(System.currentTimeMillis())
  implicit val alphabet = "abcdef".toVector

  /**
    * Exercise 10.4
    */
  test("Test that integer with additions are a monoid") {
    val intAdditionIsMonoid: Prop = monoidLaws(intAddition, Sampler.intInInterval(-100, 100))
    assert(intAdditionIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that integer with multiplication are a monoid") {
    val intMultiplicationIsMonoid: Prop = monoidLaws(intMultiplication, Sampler.intInInterval(-100, 100))
    assert(intMultiplicationIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that booleans with or are a monoid") {
    val booleanOrIsMonoid: Prop = monoidLaws(booleanOr, Sampler.boolean)
    assert(booleanOrIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that booleans with and are a monoid") {
    val booleanAndIsMonoid: Prop = monoidLaws(booleanAnd, Sampler.boolean)
    assert(booleanAndIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that strings with concatenation are a monoid") {
    val stringMonoidIsMonoid: Prop = monoidLaws(stringMonoid, Sampler.string(10))
    assert(stringMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that lists with appending are a monoid") {
    val listSampler = Sampler.intInInterval(-100, 100).listOf(Sampler.intInInterval(0, 100))
    val listMonoidIsMonoid: Prop = monoidLaws(listMonoid[Int], listSampler)
    assert(listMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  /**
    * Exercise 10.4
    */
  test("Test that options with orElse are a monoid") {
    val optionSampler = Sampler.boolean.flatMap(
      bool =>
        if (bool)
          Sampler.intInInterval(-100, 100).map(Some(_))
        else Sampler.unit(None)
    )
    val optionMonoidIsMonoid: Prop = monoidLaws(optionMonoid[Int], optionSampler)
    assert(optionMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)
  }

  test("Test foldMap, foldLeftViaFoldMap") {
    val strings = List("a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg")
    val ints = List(1, 2, 3, 4, 5, 6, 7)
    assert(foldMap(strings, intAddition)(_.length) == strings.foldLeft(0)(_ + _.length))
    assert(foldMap(ints, stringMonoid)(_.toString) == ints.foldLeft("")(_ ++ _.toString))
    assert(foldLeftViaFoldMap(strings)(0)(_ + _.length) == strings.foldLeft(0)(_ + _.length))
    assert(foldLeftViaFoldMap(ints)("")(_ ++ _.toString) == ints.foldLeft("")(_ ++ _.toString))
  }

  test("Test foldMap for IndexedSequences") {
    val stringsVector = Vector("a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg")
    val intsVector = Vector(1, 2, 3, 4, 5, 6, 7)
    assert(foldMap(stringsVector, intAddition)(_.length) == stringsVector.foldLeft(0)(_ + _.length))
    assert(foldMap(intsVector, stringMonoid)(_.toString) == intsVector.foldLeft("")(_ ++ _.toString))
  }

  test("Sorting via foldMap and monoids") {
    val emptyVector = Vector.empty[Int]
    val vectorOfOne = Vector(1)
    val sortedVector = Vector(1, 2, 3, 4, 7, 9, 10)
    val unsortedVectorMiddle = Vector(1, 2, 4, 3, 7, 9, 10)
    val unsortedVectorBeginning = Vector(4, 1, 2, 3, 7, 9, 10)
    val r = new scala.util.Random
    val unsortedLargeVector = (1 to 100000).toVector.map(_ => r.nextInt(1000))
    val sortedLargeVector = unsortedLargeVector.sorted
    assert(isSorted(emptyVector))
    assert(isSorted(vectorOfOne))
    assert(isSorted(sortedVector))
    assert(!isSorted(unsortedVectorMiddle))
    assert(!isSorted(unsortedVectorBeginning))
    assert(!isSorted(unsortedLargeVector))
    assert(isSorted(sortedLargeVector))

  }

  /**
    * Exercise 10.10
    */
  test("wordCountMonoid is a monoid") {
    val stubSampler = for {
      string <- Sampler.string(10)
    } yield Stub(string)

    val partSampler = for {
      left <- stubSampler
      right <- stubSampler
      int <- Sampler.intInInterval(0, 1001)
    } yield Part(left, right, int)

    val wordCountSampler = Sampler.boolean.flatMap(bool => if (bool) stubSampler else partSampler)

    val wordCountMonoidIsMonoid: Prop = monoidLaws(wordCountMonoid, wordCountSampler)
    assert(wordCountMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)

  }

  test("wordCount counts words correctly") {
    val emptyWord = ""
    val singleWord = "ciao"
    val spaceAndWord = " ciao"
    val wordAndSpace = "ciao "
    val wordsNoSpacesAtEdges = "ciao ciao ciao ciao"
    val wordsSpaceLeft = " ciao ciao ciao ciao"
    val wordsSpaceRight = "ciao ciao  ciao ciao "
    assert(countWords(emptyWord) === 0)
    assert(countWords(singleWord) === 1)
    assert(countWords(spaceAndWord) === 1)
    assert(countWords(wordAndSpace) === 1)
    assert(countWords(wordsNoSpacesAtEdges) === 4)
    assert(countWords(wordsSpaceLeft) === 4)
    assert(countWords(wordsSpaceRight) === 4)

  }

  test("productMonoid is a monoid") {
    val productIntSampler = for {
      i1 <- Sampler.intInInterval(0, 1001)
      i2 <- Sampler.intInInterval(0, 1001)
    } yield (i1, i2)

    val productMonoidProp = monoidLaws(productMonoid(intAddition, intAddition), productIntSampler)
    assert(productMonoidProp.check(maxSize, numSamples, state).notFalsified)
  }

  test("Bag works correctly") {
    val emptyVector = Vector.empty[Int]
    val justOne = Vector(1)
    val vector1 = Vector(1, 2, 3, 4, 5, 6)
    val vector2 = Vector(1, 1, 2, 4, 3, 5, 6, 5, 5, 5, 6)
    val vector3 = Vector(1, 4, 3, 2, 5, 6)

    assert(bag(emptyVector) == Map.empty[Int, Int])
    assert(bag(justOne) == Map(1 -> 1))
    assert(bag(vector1) == Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1))
    assert(bag(vector2) == Map(1 -> 2, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 4, 6 -> 2))
    assert(bag(vector3) == Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1))
  }

}
