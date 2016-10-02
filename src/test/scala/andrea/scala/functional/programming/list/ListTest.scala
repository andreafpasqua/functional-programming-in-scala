package andrea.scala.functional.programming.list

/**
  * Created by andreapasqua on 10/01/2016.
  */
object ListTest extends App {
  import List._

  val listEmpty = List[Int]()
  val listOne = List(1)
  val listMultiple = List(1, 2, 3, 4, 5)
  val listMultipleWithZero = List(1, 2, 0, 4, 5)
  println("Test sum")
  assert(listEmpty.sum == 0)
  assert(listOne.sum == 1)
  assert(listMultiple.sum == 15)
  assert(listMultipleWithZero.sum == 12)

  println("Test product")
  assert(listEmpty.product == 1)
  assert(listOne.product == 1)
  assert(listMultiple.product == 120)
  assert(listMultipleWithZero.product == 0)

  println("Test fill")
  assert(fill(5, "ciao") == List("ciao", "ciao", "ciao", "ciao", "ciao"))

  println("Test tail")
  assert(listOne.tail == Nil)
  assert(listMultiple.tail == List(2, 3, 4, 5))

  println("Test setHead")
  assert(listEmpty.setHead(1) == List(1))
  assert(listOne.setHead(2) == List(2))
  assert(listMultiple.setHead(2) == List(2, 2, 3, 4, 5))

  println("Test drop")
  assert(listEmpty.drop(10) == Nil)
  assert(listOne.drop(2) == Nil)
  assert(listOne.drop(1) == Nil)
  assert(listMultiple.drop(3) == List(4, 5))

  println("Test dropWhile")
  val pLess = (n: Int) => n <= 3
  val pMore = (n: Int) => n > 3
  assert(listEmpty.dropWhile(pLess) == Nil)
  assert(listOne.dropWhile(pLess) == Nil)
  assert(listMultiple.dropWhile(pLess) == List(4, 5))
  assert(listMultiple.dropWhile(pMore) == listMultiple)

  println("Test append")
  assert(listEmpty.append(listMultiple) == listMultiple)
  assert(listOne.append(listMultiple) == List(1, 1, 2, 3, 4, 5))
  assert(listMultiple.append(listOne) == List(1, 2, 3, 4, 5, 1))
  assert(listMultiple.append(listEmpty) == listMultiple)

  println("Test init")
  assert(listEmpty.init == listEmpty)
  assert(listOne.init == listEmpty)
  assert(listMultiple.init == List(1, 2, 3, 4))

  println("Test foldRight")
  assert(listEmpty.foldRight(1)(_ + _) == 1)
  assert(listOne.foldRight(1)(_ + _) == 2)
  assert(listMultiple.foldRight("")((n: Int, s: String) => n.toString + s) == "12345")
  // Exercise 3.8
  assert(listMultiple.foldRight(Nil.asInstanceOf[List[Int]])(Cons(_,_)) == listMultiple)

  println("Test length")
  assert(listEmpty.length == 0)
  assert(listOne.length == 1)
  assert(listMultipleWithZero.length == 5)


  println("Test foldLeft")
  assert(listEmpty.foldLeft(1)(_ + _) == 1)
  assert(listOne.foldLeft(1)(_ + _) == 2)
  assert(listMultiple.foldLeft("")((s: String, n: Int) => s + n.toString) == "12345")
  // Continue HERE
  assert(listMultiple.foldLeft(Nil.asInstanceOf[List[Int]])(Cons(_,_)) == listMultiple)

}
