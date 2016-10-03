package andrea.scala.functional.programming.tree

/**
  * Created by andrea on 10/2/16.
  */
sealed trait Tree[+A] {

  /**
    * It traverses a tree, acting on each leaf with op and combine the left and right
    * results at a branch with comb.
    * Note that it is not tail recursive
    * Exercise 3.29
    */
  def fold[B](op: A => B, comb: (B, B) => B): B = this match {
    case Leaf(v) => op(v)
    case Branch(l, r) => comb(l.fold(op, comb), r.fold(op, comb))
  }

  /**
    * Computes the number of nodes in the tree, each branch and each leaf counts as 1.
    * Note that this is not tail recursive
    * Exercise 3.29
    */
  def size: Int = fold[Int](_ => 1, _ + _ + 1)

  /** Computes the maximum value occurring at any leaf of a tree, for all classes
    * that can be wrapped inside num.
    * Note that this is not tail recursive
    * Exercise 3.29
    */
  def maximum[B >: A](implicit num: Numeric[B]): B = fold[B]((a: A) => a, num.max)

  /** Computes the maximum path length from the root to any leaf of a tree. For
    * a single leaf it's 0.
    * Note that this is not tail recursive
    * Exercise 3.29
    */
  def depth: Int = fold[Int](_ => 0, _.max(_) + 1)

  /**
    * Transforms all values of the tree using the function f.
    * Note that it is not tail recursive
    * Exercise 3.29
    */
  def map[B](f: A => B): Tree[B] = fold[Tree[B]](v => Leaf(f(v)), Branch(_, _))


  /************ OLDER VERSIONS OF ROUTINES
    */

  /**
    * Computes the number of nodes in the tree, each branch and each leaf counts as 1.
    * Note that this is not tail recursive
    * Exercise 3.25
    */
  def sizeNoFold: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => l.size + r.size + 1
  }

  /** Computes the maximum value occurring at any leaf of a tree, for all classes
    * that can be wrapped inside num.
    * Note that this is not tail recursive
    * Exercise 3.26
    */
  def maximumNoFold[B >: A](implicit num: Numeric[B]): B = this match {
    case Leaf(v) => v
    case Branch(l, r) => num.max(l.maximumNoFold(num), r.maximumNoFold(num))
  }

  /** Computes the maximum path length from the root to any leaf of a tree. For
    * a single leaf it's 0.
    * Note that this is not tail recursive
    * Exercise 3.27
    */
  def depthNoFold: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => l.depthNoFold.max(r.depthNoFold) + 1
  }

  /**
    * Transforms all values of the tree using the function f.
    * Note that it is not tail recursive
    * Exercise 3.28
    */
  def mapNoFold[B](f: A => B): Tree[B] = this match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.mapNoFold(f), r.mapNoFold(f))
  }

}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
