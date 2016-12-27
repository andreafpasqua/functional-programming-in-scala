package andrea.scala.functional.programming.foldable

import andrea.scala.functional.programming.list.List
import andrea.scala.functional.programming.stream.Stream
import andrea.scala.functional.programming.monoid.Monoid
import andrea.scala.functional.programming.tree.Tree


/**
  * Created by andrea on 12/27/16.
  */

trait Foldable[F[_]] {

  // note that the circularity of the definitions needs to be broken when the trait is inherited
  def foldRight[T, S](ts: F[T])(z: S)(f: (T, S) => S): S = {
    val m = Monoid.dual(Monoid.endoMonoid[S])
    val g = f.curried
    foldMap(ts, m)(g)(z)
  }

  def foldLeft[T, S](ts: F[T])(z: S)(f: (S, T) => S): S = {
    val m = Monoid.endoMonoid[S]
    val g = (t: T) => (s: S) => f(s, t)
    foldMap(ts, m)(g)(z)
  }
  def foldMap[T, S](ts: F[T], m: Monoid[S])(f: T => S): S = foldLeft(ts)(m.zero)((s, t) => m.op(s, f(t)))

  def concatenate[T](ts: F[T], m: Monoid[T]): T = foldLeft(ts)(m.zero)(m.op)

  /**
    * Turns ts into a list of elements of type T.
    * Exercise 10.15
    */
  def toList[T](ts: F[T]): List[T] = foldRight(ts)(List.empty[T])(_ :: _)

}

object Foldable {

  /**
    * Exercise 10.12
    */
  object ListFoldable extends Foldable[List] {
    override def foldRight[T, S](ts: List[T])(z: S)(f: (T, S) => S) = ts.foldRight(z)(f)
    override def foldLeft[T, S](ts: List[T])(z: S)(f: (S, T) => S) = ts.foldLeft(z)(f)
  }

  /**
    * Exercise 10.12
    */
  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[T, S](ts: IndexedSeq[T])(z: S)(f: (T, S) => S) = ts.foldRight(z)(f)
    override def foldLeft[T, S](ts: IndexedSeq[T])(z: S)(f: (S, T) => S) = ts.foldLeft(z)(f)
    override def foldMap[T, S](ts: IndexedSeq[T], m: Monoid[S])(f: T => S): S = foldMap(ts, m)(f)
  }

  /**
    * Exercise 10.12
    */
  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[T, S](ts: Stream[T])(z: S)(f: (T, S) => S) = {
      val g: (T, => S) => S = (t, s) => f(t, s)
      ts.foldRight(z)(g)
    }
    override def foldLeft[T, S](ts: Stream[T])(z: S)(f: (S, T) => S) = {
      val g: (S, => T) => S = (s, t) => f(s, t)
      ts.foldLeft(z)(g)
    }
  }

  /**
    * Exercise 10.13
    */
  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[T, S](ts: Tree[T], m: Monoid[S])(f: T => S): S = ts.fold(f, m.op)
  }

  /**
    * Exercise 10.14
    */
  object OptionFoldable extends Foldable[Option] {
    override def foldRight[T, S](t: Option[T])(z: S)(f: (T, S) => S) = t.map(f(_, z)).getOrElse(z)
    override def foldLeft[T, S](t: Option[T])(z: S)(f: (S, T) => S) = t.map(f(z, _)).getOrElse(z)
    override def foldMap[T, S](t: Option[T], m: Monoid[S])(f: T => S): S = t.map(f).getOrElse(m.zero)
  }

}