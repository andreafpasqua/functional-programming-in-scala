package andrea.scala.functional.programming.monoid

import andrea.scala.functional.programming.monoid.Monoid.MonoidLaws
import andrea.scala.functional.programming.testing.Sampler

/**
  * Created by andreapasqua on 11/07/2016.
  */
object MonoidTest extends App {

  implicit val alphabet = "abcdefghijklmnopqrstuvwxyz".toVector

  val optionSampler: Sampler[Option[Int]] = for {
    boolean <- Sampler.boolean
    int <- Sampler.intInInterval(-100, 101)
  } yield if (boolean) Some(int) else None

  val endoSampler: Sampler[(Int) => Int] =
    Sampler.function(Sampler.intInInterval(-100, 101), Sampler.intInInterval(-100, 101))

  val stringSampler: Sampler[String] = Sampler.string(30)

  val intSampler: Sampler[Int] = Sampler.intInInterval(-100, 100)

  println("* Test op associavity on options")
  assert(MonoidLaws.opLaw(optionSampler)(Monoid.MonoidExamples.optionMonoid).check(numSamples = 1000).notFalsified)
  println("* Test zero property on options")
  assert(MonoidLaws.zeroLaw(optionSampler)(Monoid.MonoidExamples.optionMonoid).check(numSamples = 1000).notFalsified)

//  println("* Test op associavity on endofunctions")
////  assert(MonoidLaws.opLaw(endoSampler)(Monoid.MonoidExamples.endoMonoid).check(numSamples = 10).notFalsified)
//  println("* Test zero property on endofunctions")
//  assert(MonoidLaws.zeroLaw(endoSampler)(Monoid.MonoidExamples.endoMonoid).check(numSamples = 100).notFalsified)
//
  println("* Test op associavity on strings")
    assert(MonoidLaws.opLaw(stringSampler)(Monoid.MonoidExamples.stringMonoid).check(numSamples = 1000).notFalsified)
  println("* Test zero property on strings")
  assert(MonoidLaws.zeroLaw(stringSampler)(Monoid.MonoidExamples.stringMonoid).check(numSamples = 1000).notFalsified)

}