package andrea.scala.functional.programming.option

/**
  * Created by andrea on 10/2/16.
  */
object OptionTest extends App {
  import Option._

  val optionEmpty: Option[Int] = None
  val optionOne = Some(1)
  val noFailFunc = (n: Int) => Some(n * 2)
  val failFunc = (n: Int) => if (n == 1) None else Some(n * 2)
  println("Test map")
  assert(optionEmpty.map(_ * 2) == optionEmpty)
  assert(optionOne.map(_ * 2) == Some(2))

  println("Test getOrElse")
  assert(optionEmpty.getOrElse(2) == 2)
  assert(optionOne.getOrElse(2) == 1)

  println("Test flatMap")
  assert(optionEmpty.flatMap(noFailFunc) == optionEmpty)
  assert(optionOne.flatMap(noFailFunc) == Some(2))
  assert(optionOne.flatMap(failFunc) == None)

  println("Test orElse")
  assert(optionEmpty.orElse(Some("a")) == Some("a"))
  assert(optionOne.orElse(Some("a")) == optionOne)

  println("Test filter")
  assert(optionEmpty.filter(_ == 1) == optionEmpty)
  assert(optionOne.filter(_ == 1) == optionOne)
  assert(optionOne.filter(_ > 1) == optionEmpty)

  println("Test map2")
  assert(optionEmpty.map2(optionOne)(_ + _) == optionEmpty)
  assert(optionOne.map2(optionEmpty)(_ + _) == optionEmpty)
  assert(optionOne.map2(optionOne)(_ + _) == Some(2))

  println("Test sequence")
  assert(sequence(List()) == Some(List()))
  assert(sequence(List(optionOne, optionEmpty, optionOne)) == None)
  assert(sequence(List(optionOne, optionOne, optionOne)) == Some(List(1, 1, 1)))

  println("Test traverse")
  assert(traverse(List[Int]())(failFunc) == Some(List()))
  assert(traverse(List(1, 2, 3))(noFailFunc) == Some(List(2, 4, 6)))
  assert(traverse(List(1, 2, 3))(failFunc) == None)

}
