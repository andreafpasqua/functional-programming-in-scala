package andrea.scala.functional.programming.monoid

import andrea.scala.functional.programming.state.SimpleRandomState
import andrea.scala.functional.programming.testing.{Prop, Sampler}

/**
  * Created by andrea on 11/16/16.
  */

object MonoidTest extends App {

  val maxSize = 100
  val numSamples = 1000
  val state = SimpleRandomState(System.currentTimeMillis())
  implicit val alphabet = "abcdef".toVector

  /**
    * Exercise 10.4
    */
  println("* Test that integer with additions are a monoid")
  val intAdditionIsMonoid: Prop = Monoid.monoidLaws(Monoid.intAddition, Sampler.intInInterval(-100, 100))
  assert(intAdditionIsMonoid.check(maxSize, numSamples, state).notFalsified)

  /**
    * Exercise 10.4
    */
  println("* Test that integer with multiplication are a monoid")
  val intMultiplicationIsMonoid: Prop = Monoid.monoidLaws(Monoid.intMultiplication, Sampler.intInInterval(-100, 100))
  assert(intMultiplicationIsMonoid.check(maxSize, numSamples, state).notFalsified)

  /**
    * Exercise 10.4
    */
  println("* Test that booleans with or are a monoid")
  val booleanOrIsMonoid: Prop = Monoid.monoidLaws(Monoid.booleanOr, Sampler.boolean)
  assert(booleanOrIsMonoid.check(maxSize, numSamples, state).notFalsified)

  /**
    * Exercise 10.4
    */
  println("* Test that booleans with and are a monoid")
  val booleanAndIsMonoid: Prop = Monoid.monoidLaws(Monoid.booleanAnd, Sampler.boolean)
  assert(booleanAndIsMonoid.check(maxSize, numSamples, state).notFalsified)

  /**
    * Exercise 10.4
    */
  println("* Test that strings with concatenation are a monoid")
  val stringMonoidIsMonoid: Prop = Monoid.monoidLaws(Monoid.stringMonoid, Sampler.string(10))
  assert(stringMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)

  /**
    * Exercise 10.4
    */
  println("* Test that lists with appending are a monoid")
  val listSampler = Sampler.intInInterval(-100, 100).listOf(Sampler.intInInterval(0, 5))
  val listMonoidIsMonoid: Prop = Monoid.monoidLaws(Monoid.listMonoid[Int], listSampler)
  assert(listMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)
  /**
    * Note that you get stack-overflow from the Sampler if you make the list too big. This is
    * on account of filter using foldRight. But without it the performance is very slow
    */

  /**
    * Exercise 10.4
    */
  println("* Test that options with orElse are a monoid")
  val optionSampler = Sampler.boolean.flatMap(
    bool =>
      if (bool)
        Sampler.intInInterval(-100, 100).map(Some(_))
      else Sampler.unit(None)
  )
  val optionMonoidIsMonoid: Prop = Monoid.monoidLaws(Monoid.optionMonoid[Int], optionSampler)
  assert(optionMonoidIsMonoid.check(maxSize, numSamples, state).notFalsified)

  println("* Test foldMap, foldLeftViaFoldMap")
  val strings = List("a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg")
  val ints = List(1, 2, 3, 4, 5, 6, 7)
  assert(Monoid.foldMap(strings, Monoid.intAddition)(_.length) == strings.foldLeft(0)(_ + _.length))
  assert(Monoid.foldMap(ints, Monoid.stringMonoid)(_.toString) == ints.foldLeft("")(_ ++ _.toString))
  assert(Monoid.foldLeftViaFoldMap(strings)(0)(_ + _.length) == strings.foldLeft(0)(_ + _.length))
  assert(Monoid.foldLeftViaFoldMap(ints)("")(_ ++ _.toString) == ints.foldLeft("")(_ ++ _.toString))


}
