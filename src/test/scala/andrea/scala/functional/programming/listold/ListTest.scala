package andrea.scala.functional.programming.listold

/**
  * Created by andrea on 7/16/16.
  */
object ListTest extends App {
  import List._

  val list1 = List(1, 2, 3, 4, 5)
  println(s"list1 = $list1")
  val list2= List(1, 2, 0, 4)
  println(s"list2 = $list2")

  val res1 = list1 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + t.sum
    case _ => 101
  }
  assert(res1 == 1 + 2, s"$res1 not equal to 1 + 2")

  println(s"list1.init = ${list1.init}")

  println(s"List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)) = ${List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _))}")
  println(s"Nil.length = ${Nil.length}")
  println(s"List(1, 2, 3).length = ${List(1, 2, 3).length}")
  println(s"List(1, 2, 3).reverse = ${List(1, 2, 3).reverse}")
  println(s"list1.append(list2) = ${list1.append(list2)}")
  println(s"concatenate(List(list1, list2, list1)) = ${concatenate(List(list1, list2, list1))}")
  println(s"list1.map(_ * 2) = ${list1.map(_ * 2)}")
  println(s"list1.filter(_<=3) = ${list1.filter(_<=3)}")
  println(s"list1.flatMap(i => List(i, i)) = ${list1.flatMap(i => List(i, i))}")
  println(s"list1.zipWith(list1)(_ + _) = ${list1.zipWith(list1)(_ + _)}")
  println(s"List(1, 2, 3, 4, 5).startsWith(List(1, 2, 3, 4)) = ${List(1, 2, 3, 4, 5).startsWith(List(1, 2, 3, 4))}")
  println(s"list1.take(3) = ${list1.take(3)}")
  println(s"list1.scanRight(0)(_ + _) = ${list1.scanRight(0)(_ + _)}")
  println(s"list1.scanLeft(0)(_ + _) = ${list1.scanLeft(0)(_ + _)}")
  println(s"hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)) = ${List(1, 2, 3, 4, 5).hasSubsequence(List(2, 3, 4))}")
  println(s"hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 5)) = ${List(1, 2, 3, 4, 5).hasSubsequence(List(2, 3, 5))}")
  println(s"hasSubsequence(List(), List(2, 3, 4)) = ${List().hasSubsequence(List(2, 3, 4))}")
  println(s"hasSubsequence(List(), List()) = ${List().hasSubsequence(List())}")
  println(s"list1.foldRightWithStopValue(1, 0, 0)(_ * _) = ${list1.foldRightWithStopValue(1, 0, 0)(_ * _)}")
  println(s"list2.foldRightWithStopValue(1, 0, 0)(_ * _) = ${list2.foldRightWithStopValue(1, 0, 0)(_ * _)}")
}
