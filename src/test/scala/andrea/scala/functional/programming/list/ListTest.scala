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
  val listManyOne = fill(100000)(1)

  println("Test sum")
  println(listEmpty.sum)
  assert(listEmpty.sum == 0)
  assert(listOne.sum == 1)
  assert(listMultiple.sum == 15)
  assert(listMultipleWithZero.sum == 12)
  assert(listManyOne.sum == 100000)

  println("Test product")
  assert(listEmpty.product == 1)
  assert(listOne.product == 1)
  assert(listMultiple.product == 120)
  assert(listMultipleWithZero.product == 0)
  assert(listManyOne.product == 1)

  println("Test fill")
  assert(fill(5)("ciao") == List("ciao", "ciao", "ciao", "ciao", "ciao"))

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
  assert(listManyOne.dropWhile(pLess) == Nil)
  assert(listManyOne.dropWhile(pMore) == listManyOne)

  println("Test append")
  assert(listEmpty.append(listMultiple) == listMultiple)
  assert(listOne.append(listMultiple) == List(1, 1, 2, 3, 4, 5))
  assert(listMultiple.append(listOne) == List(1, 2, 3, 4, 5, 1))
  assert(listMultiple.append(listEmpty) == listMultiple)
  /**
    * The expression below would give a stack overflow error on
    * large lists as append relies on foldRight.
    *
    * assert(listManyOne.append(listManyOne).sum == 200000)
    */

  println("Test init")
  assert(listEmpty.init == listEmpty)
  assert(listOne.init == listEmpty)
  assert(listMultiple.init == List(1, 2, 3, 4))
  /**
    * One gets a stack overflow error on large lists with init, for instance
    * on the following command
    *
    *   assert(listManyOne.init.sum == 99999)
    */

  println("Test foldRight")
  assert(listEmpty.foldRight(1)(_ + _) == 1)
  assert(listOne.foldRight(1)(_ + _) == 2)
  assert(listMultiple.foldRight("")((n: Int, s: String) => n.toString + s) == "12345")
  // Exercise 3.8
  assert(listMultiple.foldRight(Nil.asInstanceOf[List[Int]])(Cons(_, _)) == listMultiple)
  /** Note that using foldRight with a long List gives stack overflow. Even though it's
    * tail recursive. So the following operation will fail
    *
    *    assert(listManyOne.foldRight(0)(_ + _) == 100000)
    *
    * A different implementation where you first reverse and then do a foldLeft will
    * remove the problem, but even then the following will fail
    *
    * listManyOne.foldRight(Nil: List[Int])(Cons(_, _))
    *
    * I am not sure why that is, but it has probably to do with the implementation of Cons.
    */

  println("Test length")
  assert(listEmpty.length == 0)
  assert(listOne.length == 1)
  assert(listMultipleWithZero.length == 5)
  assert(listManyOne.length == 100000)

  println("Test foldLeft")
  assert(listEmpty.foldLeft(1)(_ + _) == 1)
  assert(listOne.foldLeft(1)(_ + _) == 2)
  assert(listMultiple.foldLeft("")((s, n) => s + n.toString) == "12345")
  assert(listManyOne.foldLeft("")((s, n) => if (s.length >= 5) s else s + n.toString) == "11111")


  println("Test reverse")
  assert(listEmpty.reverse == listEmpty)
  assert(listOne.reverse == listOne)
  assert(listMultiple.reverse == List(5, 4, 3, 2, 1))
  /**
    * On large lists this gives a stack overflow. For instance with the following
    *
    * assert(listManyOne.reverse == listManyOne)
    */

  println("Test concatenate")
  assert(concatenate(List(listEmpty, listEmpty, listEmpty)) == listEmpty)
  assert(concatenate(List(listEmpty, listOne, listEmpty)) == listOne)
  assert(concatenate(List(listOne, listMultiple, listOne)) == List(1, 1, 2, 3, 4, 5, 1))
  /**
    * With large constituents list this gives a stack-overflow since it relies on leftRight.
    * As for instance on
    *
    *   assert(concatenate(List(listManyOne, listManyOne)) == fill(200000)(1))
    */

  println("Test map")
  assert(listEmpty.map(_ + 1) == listEmpty)
  assert(listMultiple.map(_ + 1) == List(2, 3, 4, 5, 6))
  assert(listMultiple.map(_.toString) == List("1", "2", "3", "4", "5"))
  /**
    * With large lists this gives a stack-overflow since it relies on leftRight.
    * As for instance on
    *
    *     assert(listManyOne.map(_ + 1) == fill(100000)(2))
    */

  println("Test filter")
  val isEven = (n: Int) => n % 2 == 0
  assert(listEmpty.filter(isEven) == listEmpty)
  assert(listOne.filter(isEven) == listEmpty)
  assert(listMultiple.filter(isEven) == List(2, 4))
  /**
    * With large lists this gives a stack-overflow since it relies on leftRight.
    * As for instance on
    *
    *     assert(listManyOne.filter(_ > 1) == listEmpty)
    */

  println("Test flatMap")
  val doubler = (n: Int) => List(n, n)
  assert(listEmpty.flatMap(doubler) == listEmpty)
  assert(listOne.flatMap(doubler) == List(1, 1))
  assert(listMultiple.flatMap(doubler) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  /**
    * With large lists this gives a stack-overflow since it relies on leftRight.
    * As for instance on
    *
    *     assert(listManyOne.flatMap(List(_)) == listManyOne)
    */

  println("Test zipWith")
  assert(listEmpty.zipWith(listOne)(_ + _) == listEmpty)
  assert(listMultiple.zipWith(listEmpty)(_ + _) == listEmpty)
  assert(listMultiple.zipWith(listOne)(_ + _) == List(2))
  assert(listMultiple.zipWith(listMultiple)(_ + _) == List(2, 4, 6, 8, 10))
  assert(listMultiple.zipWith(listMultiple)( (_, _)) == List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))

  println("Test ::")
  assert(1 :: List(2, 3, 4, 5) == listMultiple)
  assert(1 :: Nil == listOne)

  println("Test take")
  assert(listEmpty.take(10) == listEmpty)
  assert(listOne.take(10) == listOne)
  assert(listMultiple.take(3) == List(1, 2, 3))
  assert(listManyOne.take(100) == fill(100)(1))
  /**
    * Not a tail recursive implementation, so it will fail when both list and n are large.
    * For instance on
    *
    *   assert(listManyOne.take(100000) == listManyOne)
    */

  println("Test takeWhile")
  assert(listEmpty.takeWhile(_ > 0) == listEmpty)
  assert(listOne.takeWhile(_ > 0) == listOne)
  assert(listOne.takeWhile(_ > 1) == listEmpty)
  assert(listMultiple.takeWhile(_ < 3) == List(1, 2))
  assert(listManyOne.takeWhile(_ > 1) == listEmpty)
  /**
    * Not a tail recursive implementation, so it will fail when the list
    * is large and when the predicate doesn't fail
    * For instance on
    *
    *    assert(listManyOne.takeWhile(_ > 0) == listManyOne)
    */

  println("Test forall")
  assert(listEmpty.forall(_ > 0))
  assert(listOne.forall(_ > 0))
  assert(!listOne.forall(_ > 1))
  assert(!listMultiple.forall(_ < 3))
  assert(listManyOne.forall(_ > 0))
  assert(!listManyOne.forall(_ > 1))

  println("Test exists")
  assert(!listEmpty.exists(_ > 0))
  assert(listOne.exists(_ > 0))
  assert(!listOne.exists(_ > 1))
  assert(listMultiple.exists(_ < 3))
  assert(!listMultiple.exists(_ > 6))
  assert(listManyOne.exists(_ > 0))
  assert(!listManyOne.exists(_ > 1))

  println("Test scanLeft")
  assert(listEmpty.scanLeft(0)(_ + _) == List(0))
  assert(listOne.scanLeft(0)(_ + _) == List(0, 1))
  assert(listMultiple.scanLeft(0)(_ + _) == List(0, 1, 3, 6, 10, 15))
  /**
    * This implementation of scanLeft will give stack overflow on a large list. For instance
    *   assert(listManyOne.scanLeft(Nil: List[Int])((l, n) => l) == fill(100001)(Nil: List[Int]))
    */

  println("Test scanRight")
  assert(listEmpty.scanRight(0)(_ + _) == List(0))
  assert(listOne.scanRight(0)(_ + _) == List(1, 0))
  assert(listMultiple.scanRight(0)(_ + _) == List(15, 14, 12, 9, 5, 0))
  /**
    * This implementation of scanLeft will give stack overflow on a large list. For instance
    *   assert(listManyOne.scanLeft(Nil: List[Int])((l, n) => l) == fill(100001)(Nil: List[Int]))
    */

  println("Test hasSubsequence")
  assert(listEmpty.hasSubsequence(listEmpty))
  assert(!listEmpty.hasSubsequence(listOne))
  assert(listMultiple.hasSubsequence(listOne))
  assert(listMultiple.hasSubsequence(listMultiple))
  assert(listMultiple.hasSubsequence(listEmpty))
  assert(listManyOne.hasSubsequence(listOne))
  assert(listManyOne.hasSubsequence(fill(50000)(1)))
}
