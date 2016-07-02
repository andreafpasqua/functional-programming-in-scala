import scala.annotation.tailrec

/**
  * Created by andrea on 6/30/16.
  */
object Chapter3 {

  sealed trait List[+A] {

    @tailrec
    final def foldLeft[B](z: B)(op: (B, A) => B): B = {
      println("One call to foldLeft")
      this match {
        case Nil => z
        case Cons(hd, tl) => tl.foldLeft(op(z, hd))(op)
      }
    }

    /** The idea is that (...(z, a_1), a_2), ... an)
      * can be rewritten as op_n(op_n-1(...(op_2(op_1)...) (z)
      * where op_i = (z, a_i) is a transformation from a z to a new z
      * So use the foldLeft to compose the functions and then apply the
      * resulting function to z.
      */
    def foldRight[B](z: B)(op: (A, B) => B): B =
      foldLeft(identity[B](_)){
        (func, a) => {
          val funcA = (x: B) => op(a, x)
          (b: B) => func(funcA(b))
        }
      }(z)

    def map[B](op: A => B): List[B] =
      foldRight(Nil: List[B]) {
        (a, soFar) => Cons(op(a), soFar)
      }

    def flatMap[B](op: A => List[B]): List[B] =
      foldRight(Nil: List[B]) {
        (a, list) => op(a).append(list)
      }

    def filter(p: A => Boolean): List[A] =
      foldRight(Nil: List[A]){
        (a, soFar) =>
          if (p(a)) Cons(a, soFar)
          else soFar
      }

    def filter2(p: A => Boolean): List[A] =
      flatMap{
        a =>
          if (p(a)) Nil
          else List(a)
      }

    def length: Int = foldLeft(0)((b, a) => b + 1)

    def reverse: List[A] = foldRight(Nil: List[A])(
      (currentElement, soFar) => soFar.append(List(currentElement))
    )

    def tail: List[A] = this match {
      case Nil => throw new UnsupportedOperationException("tail on an empty list")
      case Cons(hd, tl) => tl
    }

    def setHead[B >: A](newHead: B): List[B] = Cons(newHead, this)

    def headOption: Option[A] = this match {
      case Nil => None
      case Cons(hd, tl) => Some(hd)
    }

    def drop(n: Int): List[A] =
      if (n <= 0) this
      else this match {
        case Nil => Nil
        case Cons(hd, tl) => tl.drop(n - 1)
      }

    def dropWhile(p: A => Boolean): List[A] = this match {
      case Nil => Nil
      case Cons(hd, tl) =>
        if (p(hd)) tl.dropWhile(p)
        else this
    }

    def append[B >: A](other: List[B]): List[B] =
      this.foldRight(other){
        (a, soFar) => Cons(a, soFar)
      }

    def zipWith[B, C](other: List[B])(op: (A, B) => C): List[C] =
      (this, other) match {
        case (Nil, Nil) => Nil
        case (Nil, Cons(_, _)) | (Cons(_, _), Nil) =>
          throw new IllegalArgumentException(" this and other have different number of elements")
        case (Cons(a, t1), Cons(b, t2)) =>
          Cons(op(a, b), t1.zipWith(t2)(op))
      }

    override def toString = {
      def innerString(list: List[A]): String = list match {
        case Nil => ""
        case Cons(hd, tl) => s"$hd, ${innerString(tl)}"
      }
      s"List(${innerString(this).dropRight(2)})"
    }
  }

  case class Cons[+A](hd: A, tl: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  object List {
    def sum(ints: List[Int]): Int = ints.foldLeft(0)(_ + _)
    def product(doubles: List[Double]): Double = doubles.foldLeft(1D)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def init[A](list: List[A]): List[A] = list match {
      case Nil => throw new UnsupportedOperationException("init on empty list")
      case Cons(hd, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))
    }

    def concatenate[A](list: List[List[A]]): List[A] =
      list.foldRight(List[A]())(_.append(_))

    def startsWith[A](sup: List[A], sub: List[A]): Boolean = sub match {
      case Nil => true
      case Cons(h2, t2) =>
        sup match {
          case Nil => false
          case Cons(h1, t1) =>
            h1 == h2 && startsWith(t1, t2)
        }
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
      startsWith(sup, sub) ||
        (sup match {
          case Nil => false
          case Cons(h, tl) => hasSubsequence(tl, sub)
        }
        )

    def foldRightWithStopValue[A, B](list: List[A])(z: B, stopper: A, stopValue: B)
                                    (op: (A, B) => B): B = {
      list match {
        case Nil => z
        case Cons(hd, tl) =>
          if (hd == stopper) stopValue
          else {
            val allTheRest = foldRightWithStopValue(tl)(z, stopper, stopValue)(op)
            if (allTheRest == stopper) stopValue
            else op(hd, allTheRest)
          }
      }
    }
  }

  sealed trait Tree[+A] {

    def fold[B](z: B)(op: (A, B) => B, mergeOp: (B, B) => B): B =
      this match {
        case EmptyTree => z
        case Leaf(value) => op(value, z)
        case Branch(left, right) =>
          mergeOp(
            left.fold(z)(op, mergeOp),
            right.fold(z)(op, mergeOp)
          )
      }

    def size: Int =
      this.fold(0)(
        (a, sz) => 1,
        (sz1, sz2) => sz1 + sz2 + 1
      )

    def depth: Int =
      this.fold(0)(
        (a, depth) => 1,
        (depth1, depth2) => depth1.max(depth2) + 1
      )

    def map[B](op: A => B): Tree[B] =
      this.fold(EmptyTree: Tree[B])(
        (a, b) => Leaf(op(a)),
        (tree1, tree2) => Branch(tree1, tree2)
      )

  }

  object Tree {

    def max(tree: Tree[Int]) =
      tree.fold(0)(
        (a: Int, z) => a,
        (z1, z2) => z1.max(z2)
      )

  }

  case object EmptyTree extends Tree[Nothing]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
}

object TestChapter3 extends App {

  import Chapter3._
  import Chapter3.List._
  import Chapter3.Tree._
  val list1 = List(1, 2, 3, 4, 5)
  val list2= List(1, 2, 0, 4)
  val res1 = list1 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  assert(res1 == 1 + 2, s"$res1 not equal to 1 + 2")

  println(s"init(list1) = ${init(list1)}")
  println(foldRightWithStopValue(list1)(1, 0, 0)(_ * _))
  println(foldRightWithStopValue(list2)(1, 0, 0)(_ * _))
  println(s"List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)) = ${List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _))}")
  println(s"Nil.length = ${Nil.length}")
  println(s"List(1, 2, 3).length = ${List(1, 2, 3).length}")
  println(s"List(1, 2, 3).reverse = ${List(1, 2, 3).reverse}")
  println(s"list1.append(list2) = ${list1.append(list2)}")
  println(s"concatenate(List(list1, list2, list1)) = ${concatenate(List(list1, list2, list1))}")
  println(s"list1.map(_ * 2) = ${list1.map(_ * 2)}")
  println(s"list1.filter(_<=3) = ${list1.filter(_<=3)}")
  println(s"list1.filter2(_<=3) = ${list1.filter2(_<=3)}")
  println(s"list1.flatMap(i => List(i, i)) = ${list1.flatMap(i => List(i, i))}")
  println(s"list1.zipWith(list1)(_ + _) = ${list1.zipWith(list1)(_ + _)}")
  println(s"startsWith(List(1, 2, 3, 4, 5), List(1, 2, 3, 4)) = ${startsWith(List(1, 2, 3, 4, 5), List(1, 2, 3, 4))}")
  println(s"hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)) = ${hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4))}")

  val tree1 = Branch(
    Branch(Leaf(1), Leaf(6)),
    Branch(
      Branch(Leaf(3), Leaf(4)),
      Leaf(5)))

  println(s"tree1 = $tree1")
  println(s"tree1.size = ${tree1.size}")
  println(s"tree1.depth = ${tree1.depth}")
  println(s"tree1.map(_ * 2) = ${tree1.map(_ * 2)}")
  println(s"max(tree1) = ${max(tree1)}")
  println(s"max(tree1.map(_ * 2)) = ${max(tree1.map(_ * 2))}")
}
