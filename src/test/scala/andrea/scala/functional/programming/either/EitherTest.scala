package andrea.scala.functional.programming.either

import org.scalatest.FunSuite

/**
  * Created by andreapasqua on 10/04/2016.
  */

class EitherTest extends FunSuite {
  import andrea.scala.functional.programming.either.Either._

  val eitherLeft: Either[String, Int] = Left("abcd")
  val eitherRight: Either[String, Int] = Right(1)

  test("Test map") {
    assert(eitherLeft.map(_.length, _ * 2) == 4)
    assert(eitherRight.map(_.length, _ * 2) == 2)
  }

  test("Test left.map") {
    assert(eitherLeft.left.map(_.length) == Left(4))
    assert(eitherRight.left.map(_.length) == Right(1))
  }

  test("Test right.map") {
    assert(eitherLeft.right.map(_ * 2) == Left("abcd"))
    assert(eitherRight.right.map(_ * 2) == Right(2))
  }

  test("Test left.flatMap") {
    assert(eitherLeft.left.flatMap(_ => Right(2)) == Right(2))
    assert(eitherLeft.left.flatMap(s => Left(s + s)) == Left("abcdabcd"))
    assert(eitherRight.left.flatMap(_ => Right(2)) == Right(1))
  }

  test("Test right.flatMap") {
    assert(eitherLeft.right.flatMap(_ => Right(2)) == Left("abcd"))
    assert(eitherRight.right.flatMap(n => Right(n * 2)) == Right(2))
    assert(eitherRight.right.flatMap(n => Left(n * 2)) == Left(2))
  }

  test("Test left.getOrElse") {
    assert(eitherLeft.left.getOrElse("defaultString") == "abcd")
    assert(eitherRight.left.getOrElse("defaultString") == "defaultString")
  }

  test("Test right.getOrElse") {
    assert(eitherLeft.right.getOrElse(2) == 2)
    assert(eitherRight.right.getOrElse(2) == 1)
  }

  test("Test left.orElse") {
    assert(eitherLeft.left.orElse(Left("newString")) == Left("abcd"))
    assert(eitherLeft.left.orElse(Right(2)) == Left("abcd"))
    assert(eitherRight.left.orElse(Left("newString")) == Left("newString"))
    assert(eitherRight.left.orElse(Right(2)) == Right(2))
  }

  test("Test right.orElse") {
    assert(eitherLeft.right.orElse(Left("newString")) == Left("newString"))
    assert(eitherLeft.right.orElse(Right(2)) == Right(2))
    assert(eitherRight.right.orElse(Left("newString")) == Right(1))
    assert(eitherRight.right.orElse(Right(2)) == Right(1))
  }

  test("Test left.filter") {
    assert(eitherLeft.left.filter(_.length > 3).contains(Left("abcd")))
    assert(eitherLeft.left.filter(_.length > 4).isEmpty)
    assert(eitherRight.left.filter(_.length > 3).isEmpty)
    assert(eitherRight.left.filter(_.length > 4).isEmpty)
  }

  test("Test right.filter") {
    assert(eitherLeft.right.filter(_ > 0).isEmpty)
    assert(eitherLeft.right.filter(_ > 1).isEmpty)
    assert(eitherRight.right.filter(_ > 0).contains(Right(1)))
    assert(eitherRight.right.filter(_ > 1).isEmpty)
  }

  test("Test left.map2") {
    assert(eitherLeft.left.map2(eitherLeft)(_ + _) == Left("abcdabcd"))
    assert(eitherLeft.left.map2(Right(2))(_ + _) == Right(2))
    assert(eitherRight.left.map2(eitherLeft)(_ + _) == Right(1))
    assert(eitherRight.left.map2(Right(2))(_ + _) == Right(1))
  }

  test("Test right.map2") {
    assert(eitherLeft.right.map2(Left("abc"))(_ + _) == Left("abcd"))
    assert(eitherLeft.right.map2(Right(2))(_ + _) == Left("abcd"))
    assert(eitherRight.right.map2(Left("abc"))(_ + _) == Left("abc"))
    assert(eitherRight.right.map2(eitherRight)(_ + _) == Right(2))
  }

  test("Test sequenceLeft") {
    assert(sequenceLeft(List(eitherLeft, eitherLeft, Right(2), eitherLeft)) == Right(2))
    assert(sequenceLeft(List(eitherLeft, Right(1), Right(2), eitherLeft)) == Right(1))
    assert(sequenceLeft(List.fill(4)(eitherLeft)) == Left(List.fill(4)("abcd")))
  }

  test("Test sequenceRight") {
    assert(sequenceRight(List(eitherRight, eitherRight, Left("abcd"), eitherRight)) == Left("abcd"))
    assert(sequenceRight(List(eitherRight, Left("abc"), Left("abcd"), eitherRight)) == Left("abc"))
    assert(sequenceRight(List.fill(4)(eitherRight)) == Right(List.fill(4)(1)))
  }

  test("Test traverseLeft") {
    assert(traverseLeft(List("a", "aa", "aaa"))((s: String) => Left(s.length)) == Left(List(1, 2, 3)))
    assert(traverseLeft(List("a", "aa", "aaa"))((s: String) => Right(s.length)) == Right(1))
    assert(traverseLeft(List("a", "aa", "aaa"))((s: String) => if (!s.contains("aa")) Left(s.length) else Right(s.length))
      == Right(2))
  }

  test("Test traverseRight") {
    assert(traverseRight(List(1, 2, 3))((n: Int) => Right(n.toString)) == Right(List("1", "2", "3")))
    assert(traverseRight(List(1, 2, 3))((n: Int) => Left(n.toString)) == Left("1"))
    assert(traverseRight(List(1, 2, 3))((n: Int) => if (n < 2) Right(n.toString) else Left(n.toString))
      == Left("2"))
  }

}
