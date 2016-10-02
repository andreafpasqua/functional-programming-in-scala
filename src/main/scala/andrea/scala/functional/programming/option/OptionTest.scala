package andrea.scala.functional.programming.option

import scala.util.Try
import andrea.scala.functional.programming.listold.List

/**
  * Created by andrea on 7/16/16.
  */
object OptionTest extends App {
  import Option._

  class Parent(a: Int) {
    override def toString: String = s"Parent($a)"
  }
  object Parent {
    def apply(a: Int) = new Parent(a)
  }
  case class Child(a: Int) extends Parent(a) {
    override def toString: String = s"Child($a)"
  }

  println(s"""Some(Parent(1)).orElse(Some("a")) = ${Some(Parent(1)).orElse(Some("a"))}""")
  println(s"""Some(Child(2).orElse(Some("a")) = ${Some(Child(2)).orElse(Some("a"))}""")
  println(s"""Some(None.orElse(Some("a")) = ${None.orElse(Some("a"))}""")

  println(s"Vector(1D, 2D, 3D).mean = ${Vector(1D, 2D, 3D).mean}")
  println(s"Vector(1D).mean = ${Vector(1D).mean}")
  println(s"Vector().mean = ${Vector().mean}")
  println(s"Vector(1D, 2D, 3D).variance = ${Vector(1D, 2D, 3D).variance}")
  println(s"Vector(1D).variance = ${Vector(1D).variance}")
  println(s"Vector().variance = ${Vector().variance}")

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    def toOptInt(s: String): Option[Int] = {
      val tryVal = Try(s.toInt)
      if (tryVal.isSuccess) Some(tryVal.get) else None
    }
    val optAge = toOptInt(age)
    val optTickets = toOptInt(numberOfSpeedingTickets)
    optAge.map2(optTickets)(insuranceRateQuote)
  }

  println(s"""parseInsuranceRateQuote("12", "36") = ${parseInsuranceRateQuote("12", "36")}""")
  println(s"""parseInsuranceRateQuote("a", "36") = ${parseInsuranceRateQuote("a", "36")}""")
  println(s"""parseInsuranceRateQuote("1", "36.2") = ${parseInsuranceRateQuote("1", "36.2")}""")


  println(s"sequence(List(Some(Parent(1)), Some(Child(2)), None)) = ${sequence(List(Some(Parent(1)), Some(Child(2)), None))}")
  println(s"sequence(List(Some(Parent(1)), None, Some(Child(2)))) = ${sequence(List(Some(Parent(1)), None, Some(Child(2))))}")
  println(s"sequence(List(Some(Parent(1)), Some(Parent(3)), Some(Child(4)))) = ${sequence[Parent](List(Some(Parent(1)), Some(Parent(3)), Some(Child(4))))}")

}
