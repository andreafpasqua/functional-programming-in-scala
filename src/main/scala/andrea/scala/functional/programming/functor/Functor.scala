package andrea.scala.functional.programming.functor

/**
  * Created by andrea on 12/27/16.
  */

trait Functor[F[_]] {

  /**
    * lifts a map f (think a morphism) from two objects T, S (assumed in the same category)
    * to a map between objects F(T) and F(S) where F maps to a different category, in such
    * a way that applying f and then F commutes with applying F and then map(F).
    * Hence the name functor (covariant)
    */
  def map[T, S](F: F[T])(f: T => S): F[S]

  /**
    * Given a functor F on a tuple of types (T, S), it produces a tuple of functors
    * (F[T], F[S]) mapping onto the first and then the second element of the tuple
    */
  def distribute[T, S](F: F[(T, S)]): (F[T], F[S]) =
    (map(F){ case (t, s) => t}, map(F){ case (t, s) => s})

  /**
    * Given in e either a functor F[T] or a functor F[S], it returns a functor of type
    * either T or S.
    */
  def codistribute[T, S](e: Either[F[T], F[S]]): F[Either[T, S]] = e match {
    case Left(functor) => map(functor)(Left(_))
    case Right(functor) => map(functor)(Right(_))
  }

}

object Functor {

  val listFunctor = new Functor[List] {
    def map[T, S](list: List[T])(f: T => S): List[S] = list.map(f)
  }

}