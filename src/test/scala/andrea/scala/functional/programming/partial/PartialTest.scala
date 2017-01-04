package andrea.scala.functional.programming.partial

import org.scalatest.FunSuite


/**
  * Created by andreapasqua on 10/04/2016.
  */

class PartialTest extends FunSuite {
  import Partial._

  val partialError: Partial[String, Int] = Errors(List("err"))
  val partialErrors: Partial[String, Int] = Errors(List("err1", "err2"))
  val partialSuccess: Partial[String, Int] = Success(1)
  val partialSuccessTwo: Partial[String, Int] = Success(2)
  val doubleIfNotOne = (n: Int) => if (n == 1) Errors(List("not 1")) else Success(n * 2)

  test("Test map") {
    assert(partialError.map(_ * 2) == partialError)
    assert(partialErrors.map(_ * 2) == partialErrors)
    assert(partialSuccess.map(_ * 2) == Success(2))
  }

  test("Test flatMap") {
    assert(partialError.flatMap(doubleIfNotOne) == partialError)
    assert(partialErrors.flatMap(doubleIfNotOne) == partialErrors)
    assert(partialSuccess.flatMap(doubleIfNotOne) == Errors(List("not 1")))
    assert(partialSuccessTwo.flatMap(doubleIfNotOne) == Success(4))
  }

  test("Test orElse") {
    assert(partialError.orElse(partialErrors) == partialErrors)
    assert(partialError.orElse(partialSuccess) == partialSuccess)
    assert(partialSuccess.orElse(partialError) == partialSuccess)
  }

  test("Test gerOrElse") {
    assert(partialError.getOrElse(0) == 0)
    assert(partialSuccess.getOrElse(0) == 1)
  }

  test("Test map2") {
    assert(partialError.map2(partialSuccess)(_ + _) == partialError)
    assert(partialError.map2(partialErrors)(_ + _) == Errors(List("err", "err1", "err2")))
    assert(partialSuccess.map2(partialError)(_ + _) == partialError)
    assert(partialSuccess.map2(partialSuccessTwo)(_ + _) == Success(3))
  }

  test("Test sequence") {
    assert(sequence(List(partialError, partialSuccess, partialErrors)) == Errors(List("err", "err1", "err2")))
    assert(sequence(List(partialSuccess, partialSuccessTwo, partialSuccess)) == Success(List(1, 2, 1)))
  }

  test("Test traverse") {
    assert(traverse(List(1, 2, 1, 3))(doubleIfNotOne) == Errors(List("not 1", "not 1")))
    assert(traverse(List(1, 2, 1, 3, 4))(n => Success(n * 2)) == Success(List(2, 4, 2, 6, 8)))
  }

}
