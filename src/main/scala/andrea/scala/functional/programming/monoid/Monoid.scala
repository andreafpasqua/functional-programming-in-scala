package andrea.scala.functional.programming.monoid

import andrea.scala.functional.programming.parallelism.ParComp
import andrea.scala.functional.programming.testing.{Prop, Sampler}


/**
  * Created by andrea on 11/16/16.
  */

trait Monoid[T] {

  def op(t1: T, t2: T): T
  def zero: T

}

object Monoid {

  /**
    * Given a monoid m of type T and a sampler s for T, it constructs
    * a Proposition to test associativity and the existence of a unit
    * Exercise 10.4
    */
  def monoidLaws[T](m: Monoid[T], p: Sampler[T]): Prop = {

    val associativity: Prop = Sampler.listOfN(p, 3).forall {
      case List(t1, t2, t3) => m.op(m.op(t1, t2), t3) == m.op(t1, m.op(t2, t3))
    }
    val zeroExistence: Prop = p.forall (
      t => m.op(t, m.zero) == t && m.op(m.zero, t) == t
      )
    associativity && zeroExistence
  }

  def stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    def zero: String = ""
  }

  def listMonoid[T] = new Monoid[List[T]] {
    def op(l1: List[T], l2: List[T]): List[T] = l1 ++ l2
    def zero: List[T] = List.empty[T]
  }

  /**
    * A monoid for integers under addition
    * Exercise 10.1
    */
  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero: Int = 0
  }

  /**
    * A monoid for integers under product
    * Exercise 10.1
    */
  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero: Int = 1
  }

  /**
    * A monoid for Booleans under or
    * Exercise 10.1
    */
  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero: Boolean = false
  }

  /**
    * A monoid for Booleans under and
    * Exercise 10.1
    */
  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero: Boolean = true
  }

  /**
    * a trait with just two cases to store all information relevant to wordCount
    * that is needed when the text is broken up in pieces arbitrarily. A stub
    * is a string with no spaces (it can be the empty string)
    * and it occurs at the beginning or at the end of a piece. A Part consists of
    * two Stubs and a count of the words in the middle.
    */
  trait WordCount
  case class Stub(string: String) extends WordCount
  case class Part(left: Stub, right: Stub, wordCount: Int) extends WordCount

  /**
    * A monoid for options
    * Exercise 10.2
    */
  def optionMonoid[T] = new Monoid[Option[T]] {
    def op(o1: Option[T], o2: Option[T]): Option[T] = o1.orElse(o2)
    def zero: Option[T] = None
  }

  /**
    * A monoid for endofunctions
    * Exercise 10.3
    */
  def endoMonoid[T] = new Monoid[T => T] {
    def op(f1: T => T, f2: T => T): T => T = (t: T) => f1(f2(t))
    def zero: T => T = (t: T) => t
  }

  /**
    * Given two monoids A and B, it returns a monoid for pairs of elements
    * from A and B respectively, which is just the product monoid (as for groups)
    * Exercise 10.16
    */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
    def op(p1: (A, B), p2: (A, B)): (A, B) = (A.op(p1._1, p2._1), B.op(p1._2, p2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  /**
    * Given a monoid V, it returns a monoid for maps from element of type K to V,
    * constructed assuming all maps have defaults the zero of the monoid and then
    * combining values using the op of V
    */
  def mapMergeMonoid[K, V](V: Monoid[V]) = new Monoid[Map[K, V]] {
    def op(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
      (m1.keySet ++ m2.keySet).foldLeft(zero){
        case (map, k) =>
          val v = V.op(m1.getOrElse(k, V.zero), m2.getOrElse(k, V.zero))
          map.updated(k, v)
      }
    }
    def zero: Map[K, V] = Map.empty[K, V]
  }

  /**
    * Given a monoid S, it returns a monoid for functions with codomain S. The product function
    * will return the combination using S.op of the values returned by each function.
    * Exercise 10.17
    */
  def functionMonoid[T, S](S: Monoid[S]) = new Monoid[T => S] {
    def op(f1: T => S, f2: T => S): T => S = t => S.op(f1(t), f2(t))
    def zero: T => S = t => S.zero
  }

  /**
    * Maps the elements of the list ts to elements of a monoid m using
    * f and then folds the transformed elements.
    * Exercise 10.5
    */
  def foldMap[T, S](ts: List[T], m: Monoid[S])(f: T => S): S =
    ts.foldLeft(m.zero)((s, t) => m.op(s, f(t)))

  /**
    * A foldMap for a random access sequence ts, where m is a monoid and f
    * turns the elements of ts into elements of the monoid
    * Exercise 10.7
    */
  def foldMap[T, S](ts: IndexedSeq[T], m: Monoid[S])(f: T => S): S =
    ts.length match {
      case 0 => m.zero
      case 1 => f(ts.head)
      case l =>
        val (left, right) = ts.splitAt(l / 2)
        m.op(foldMap(left, m)(f), foldMap(right, m)(f))
    }

  /**
    * A parallelizable version of foldMap for a random access sequence ts, where m is a monoid and f
    * turns the elements of ts into elements of the monoid
    * Exercise 10.8
    * TODO: implement this
    */
  def parFoldMap[T, S](ts: IndexedSeq[T], m: Monoid[S])(f: T => S): ParComp[S] = ???

  /**
    * Implements foldLeft as the foldMap on a certain monoid.
    * The monoid are the endofunctions of S and the foldMap returns
    * one such function. The output of the foldLeft is then obtained
    * by applying the resulting endofunction to the fold's zero
    * element.
    * Exercise 10.6
    */
  def foldLeftViaFoldMap[T, S](ts: List[T])(z: S)(g: (S, T) => S): S = {
    val m = Monoid.endoMonoid[S]
    val f = (t: T) => (s: S) => g(s, t)
    foldMap(ts.reverse, m)(f)(z)
  }

  /**
    * A monoid to count words using a WordCount trait.
    * Exercise 10.10
    */
  val wordCountMonoid = new Monoid[WordCount] {

    def op(wc1: WordCount, wc2: WordCount): WordCount = (wc1, wc2) match {
      case (Part(l1, Stub(r1), c1), Part(Stub(l2), r2, c2)) =>
        Part(l1, r2, if ((r1 + l2).isEmpty) c1 + c2 else c1 + c2 + 1)
      case (Stub(s), Part(Stub(l), r, c)) =>
        Part(Stub(s + l), r, c)
      case (Part(l, Stub(r), c), Stub(s)) =>
        Part(l, Stub(r + s), c)
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
    }

    def zero: WordCount = Stub("")

  }

  /**
    * Given a string string, it counts how many words it has
    * using the wordCountMonoid and foldMap
    * Exercise 10.11
    */
  def countWords(string: String): Int = {
    def charToWordCount(char: Char): WordCount =
      if (char.isWhitespace) Part(Stub(""), Stub(""), 0)
      else Stub(char.toString)

    foldMap(string.toIndexedSeq, wordCountMonoid)(charToWordCount) match {
      case Stub("") => 0
      case Stub(_) => 1
      case Part(l, r, c) => (l, r) match {
        case (Stub(""), Stub("")) => c
        case (Stub(""), _) | (_, Stub("")) => c + 1
        case _ => c + 2
      }
    }
  }

  /**
    * Checks whether an indexed sequence integers is ordered, using a suitably
    * constructed monoid.
    * Exercise 10.9
    */
  def isSorted[T](ts: IndexedSeq[T])(implicit ordering: Ordering[T]): Boolean = {
    /**
      * A monoid to use for checking whether sequences are sorted or not.
      */
    def sequenceOrderingMonoid = new Monoid[Option[(Boolean, T, T)]] {
        def op(e1: Option[(Boolean, T, T)], e2: Option[(Boolean, T, T)]): Option[(Boolean, T, T)] = (e1, e2) match {
          case (Some((bool1, min1, max1)), Some((bool2, min2, max2))) =>
            Some((bool1 && bool2 && ordering.lteq(max1, min2), ordering.min(min1, min2), ordering.max(max1, max2)))
          case (some, None) => some
          case (None, some) => some
        }
        def zero: Option[(Boolean, T, T)] = None
      }
    // use foldMap
    foldMap(ts, sequenceOrderingMonoid)(t => Some((true, t, t))).forall(_._1)
  }

  /**
    * Constructs a bag (i.e. a map from element to counts) for an indexed sequence ts of
    * elements of type T. It uses the mapMergeMonoid and foldMap
    * Exercise 10.18
    */
  def bag[T](ts: IndexedSeq[T]): Map[T, Int] = foldMap(ts, mapMergeMonoid[T, Int](intAddition))(t => Map(t -> 1))

}