package andrea.scala.functional.programming.either

import andrea.scala.functional.programming.option.Some
/**
  * Created by andrea on 7/16/16.
  */
object EitherTest extends App {
  val either1 = Left[String, Int]("a")
  val either2 = Right[String, Int](1)
  val either3 = Right[String, String]("b")

  println(s"""Left[String, Int]("a").map(Some(_)) = ${Left[String, Int]("a").map(Some(_))}""")
  println(s"""Right[String, Int](1).map(Some(_)) = ${Right[String, Int](1).map(Some(_))}""")
  println(s"""Right[String, Int](1).flatMap(i => Left(i.toString)) = ${Right[String, Int](1).flatMap(i => Left(i.toString))}""")
  println(s"""Left[String, Int]("a").flatMap(i => Left(i.toString)) = ${Left[String, Int]("a").flatMap(i => Left(i.toString))}""")
  println(s"""Right[String, Int](1).flatMap(i => Right(i * 2)) = ${Right[String, Int](1).flatMap(i => Right(i * 2))}""")
  println(s"""Left[String, Int]("a").orElse(Right[Double, String]("ciao")) = ${Left[String, Int]("a").orElse(Right[Double, String]("ciao"))}""")
  println(s"""Right[String, Int](1).orElse(Right[Double, String]("ciao")) = ${Right[String, Int](1).orElse(Right[Double, String]("ciao"))}""")
  println(s"""Left[String, Int]("a").orElse(Left[Double, String](2D)) = ${Left[String, Int]("a").orElse(Left[Double, String](2D))}""")
  println(s"""Left[String, Int]("a").map2(Right[String, Int](1))(_ + _) = ${Left[String, Int]("a").map2(Right[String, Int](1))(_ + _)}""")
  println(s"""Right[String, Int](1).map2(Right[String, Int](1))(_ + _) = ${Right[String, Int](1).map2(Right[String, Int](1))(_ + _)}""")
  println(s"""Right[String, Int](1).map2(Right[String, String]("b"))(_.toString + _) = ${Right[String, Int](1).map2(Right[String, String]("b"))(_.toString + _)}""")


}
