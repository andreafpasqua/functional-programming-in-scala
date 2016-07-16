package andrea.scala.functional.programming.tree

/**
  * Created by andrea on 7/15/16.
  */
sealed trait Tree[+A] {

  // Exercise 3.29
  def fold[B](op: A => B)(combine: (B, B) => B): B =
    this match {
      case Leaf(a) => op(a)
      case Branch(l, r) => combine(l.fold(op)(combine), r.fold(op)(combine))
    }
  def size: Int = fold[Int](_ => 1)(1 + _ + _)
  def maximum[B >: A](implicit ord: Ordering[B]): B =
    fold[B](b => b)((b1, b2) => if (ord.lteq(b1, b2)) b2 else b1)
  def depth: Int = fold[Int](_ => 0)((b1, b2) => 1 + b1.max(b2))
  def map[B](op: A => B): Tree[B] =
    fold[Tree[B]](a => Leaf(op(a)))(Branch(_, _))

  // Exercise 3.25
  def sizeNoFold: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.sizeNoFold + r.sizeNoFold
  }
  // Exercise 3.26
  def maximumNoFold[B >: A] (implicit ord: Ordering[B]): B = this match {
    case Leaf(a) => a
    case Branch(l, r) =>
      val lMax = l.maximumNoFold(ord)
      val rMax = r.maximumNoFold(ord)
      if (ord.lteq(lMax, rMax)) rMax else lMax
  }
  // Exercise 3.27
  def depthNoFold: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depthNoFold.max(r.depthNoFold)
  }
  // Exercise 3.28
  def mapNoFold[B](op: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(op(a))
    case Branch(l, r) => Branch(l.mapNoFold(op), r.mapNoFold(op))
  }

}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeTest extends App {

    val tree1 = Branch(
      Branch(Leaf(1), Leaf(6)),
      Branch(
        Branch(Leaf(3), Leaf(4)),
        Leaf(5)))

    println(s"tree1 = $tree1")
    println(s"tree1.size = ${tree1.size}")
    println(s"tree1.depth = ${tree1.depth}")
    println(s"tree1.map(_ * 2) = ${tree1.map(_ * 2)}")
    println(s"tree1.maximum = ${tree1.maximum}")
    println(s"tree1.map(_ * 2).maximum = ${tree1.map(_ * 2).maximum}")

}