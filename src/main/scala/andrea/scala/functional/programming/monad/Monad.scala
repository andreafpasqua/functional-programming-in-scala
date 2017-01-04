package andrea.scala.functional.programming.monad

import andrea.scala.functional.programming.functor.Functor
import andrea.scala.functional.programming.parallelism.ParComp
import andrea.scala.functional.programming.testing.Sampler
import andrea.scala.functional.programming.option.{Option, Some}
import andrea.scala.functional.programming.stream.Stream
import andrea.scala.functional.programming.list.List
import andrea.scala.functional.programming.parsing.Parser
import andrea.scala.functional.programming.state.StateAction

/**
  * Created by andrea on 12/27/16.
  */

/**
  * The primitive methods of a Monad are either
  *   1. unit and flatMap (UF)
  *   2. unit and compose (UC)
  *   3. unit, map and join (UMJ)(this is the closest to the usual categorical definition)
  * We created MonadBase to hold all other methods and then we extend it with
  * Monad, MonadUC and MonadUMJ with the implementation according to UF, UC and UMJ respectively
  */
trait MonadBase[M[_]] extends Functor[M] {

  /**
    * Wraps t in a monad
    */
  def unit[T](t: => T): M[T]

  /**
    * Given a monad F of type T and a function from T to monads of type S,
    * returns a monad of type S
    */
  def flatMap[T, S](m: M[T])(f: T => M[S]): M[S]

  /**
    * This overrides map (already available in the functor trait), to use the now available unit
    */
  def map[T, S](m: M[T])(f: T => S): M[S]

  /**
    * It is the Kleisli composition of Kleisli functions f and g from types to monads.
    */
  def compose[T, S, U](f: T => M[S], g: S => M[U]): T => M[U]

  /**
    * Flattens a monad of a monad m onto a simple monad
    */
  def join[T](mm: M[M[T]]): M[T]

  /**
    * Combines two monads F1 and F2 using a function from their respective type T, S, to yet
    * another type U and returns a monad of type U.
    */
  def map2[T, S, U](m1: M[T], m2: M[S])(f: (T, S) => U): M[U] = flatMap(m1)(t => map(m2)(s => f(t, s)))

  /**
    * Turns a list of Monads ms into a single monad for the list of values (of type T)
    * Exercise 11.3
    */
  def sequence[T](ms: List[M[T]]): M[List[T]] = ms.foldRight(unit(List.empty[T])) (
    (monad, resultSoFar) => map2(monad, resultSoFar)(_ :: _)
  )

  /**
    * Turns a list ts of elements of type T into a single monad using a function f that turns each
    * element of a list in a monad of type S
    * Exercise 11.3
    */
  def traverse[T, S](ts: List[T])(f: T => M[S]): M[List[S]] = ts.foldRight(unit(List.empty[S])) (
    (t, resultSoFar) => map2(f(t), resultSoFar)(_ :: _)
  )

  /**
    * Given a monad of type T, it constructs a monad with a list of size n of elements of type T
    * inside
    * Exercise 11.4
    */
  def replicateM[T](m: M[T])(n: Int): M[List[T]] = sequence(List.fill(n)(m))

  /**
    * For lists replicateM with m a list of size m and n repetitions
    * returns a nested list of size m.pow(n) where the lists inside all have size n and contains all possible
    * combinations of elements from the initial list
    * are the initial list provided. For Options, it returns an option that contains a list of size n
    * of the element that is inside the initial option, or None, if the initial option is None.
    * Exercise 11.5
    **/

  /**
    * builds the product monad. The elements inside are tuples of elements from m1 and m2.
    */
  def product[T, S](m1: M[T], m2: M[S]): M[(T, S)] = map2(m1, m2)((_, _))

  /**
    * Filters a list ts on the basis of a predicate p that returns a monad with booleans inside. The result
    * is a monad with a sublist inside. In the sublist(s) elements of ts appear only if there is a true boolean.
    * For list monads, we obtain a nested list of size equal to the product of the size of all the Boolean lists
    * p generates and for each combination the corresponding element is added or omitted depending on the Boolean.
    * For option monads, it acts like a regular filter, but if any of the options is None, then the final
    * option is none.
    * For Par... TODO: do this later
    * Exercise 11.6
    */
  def filterM[T](ts: List[T])(p: T => M[Boolean]): M[List[T]] = ts.foldRight(unit(List.empty[T])) (
    (t, resultSoFar) => map2(p(t), resultSoFar)( (bool, list) => if (bool) t :: list else list)
  )

}

/**
  * An implementation of Monad where unit and flatMap are the primitive methods
  */
trait Monad[M[_]] extends MonadBase[M] {

  /**
    * This overrides map (already available in the functor trait), to use the now available unit
    */
  def map[T, S](m: M[T])(f: T => S): M[S] = flatMap(m)(t => unit(f(t)))

  /**
    * It is the Kleisli composition of Kleisli functions f and g from types to monads.
    * Exercise 11.7
    */
  def compose[T, S, U](f: T => M[S], g: S => M[U]): T => M[U] = (t: T) => flatMap(f(t))(g)

  /**
    * Flattens a monad of a monad m onto a simple monad
    * Exercise 11.12
    */
  def join[T](m: M[M[T]]): M[T] = flatMap(m)(Predef.identity)

  /**
    * Unit is the identity element of the compose operation, expressed in terms of flatMaps
    */
  def identityLaw[T, S](m: M[T], u: T, f: T => M[S]) = flatMap(m)(unit(_)) == m &&
    flatMap(unit(u))(f) == f(u)

  /**
    * Associativity of compose, expressed in terms of flatMap
    */
  def associativeLaw[T, S, U](m: M[T])(f: T => M[S], g: S => M[U]): Boolean =
    flatMap(flatMap(m)(f))(g) == flatMap(m)(t => flatMap(f(t))(g))

  /**
    * For List as a monad,
    *   List(a1, a2, .. an).flatMap(f) = List(b11, b12, ... b1m1, b21, b22, b2m2, ... bn1, bn2, ...bnmn),
    *   where f(ai) = List(bi1, bi2,...,bimi)
    *   Then
    *   List(a1, a2, .. an).flatMap(f).flatMap(g) = List(c111, c112, ... c11p11, c121, c122, ..., c12p12)
    *   where g(bij) = List(cij1, cij2, ... cijpij).
    *   In short this is List({cijk}) where i = 1, ..., n, j =1, ... mi, k = 1, ..., pij
    *   Conversely,
    *   ai => f(ai).flatMap(g) = List({bij}).flatMap(g) = List({cijk}) for  j =1, ... mi, k = 1, ..., pij
    *   so List(a1, ..., an).flatMap(ai => f(ai).flatMap(g)) = List({cijk})
    *     for i = 1, ..., n, j =1, ... mi, k = 1, ..., pij.
    *     And this proves associativity.
    *   For the identity,
    *     unit(a) = List(a), so unit(a).flatMap(f) = List(a).flatMap(f) = List(b1, b2, ..., bm)
    *     where f(a) = List(b1, b2, ..., bm)
    *     Conversely f(a).flatMap(unit) = List(b1, ... bm).flatMap(unit) = List(b1, b2, ..., bm).
    *     Again the same and this proves it.
    * Exercise 11.11
    *
    * For ParComp, the associative law states that the final description of a calculation is the same if you write it
    * in two ways. Either: describe the calculation in m, then add a description of a second step which depends on
    * the result of the first computation, then add a description for a third step which depends on the result of the
    * second computation. Or: describe the calculation in m, then add a description of a last step which itself consists
    * of two substeps, a first computation dependent on an input of type T, followed by a second computation
    * dependent on the result of the first. The combined computation will depend on the output of m, so it can be
    * applied on top of it. Associativity means that the two ways should amount to the same description
    * For Parser, the associative law states that if you parse the beginning of a text using the parser in m, and then
    * you parse what follows using an m-result-dependent parser given by f, and then what follows yet using the
    * f(t)-dependent parser given by f, you get the same output as if you first parsed the text using the parser in m,
    * and then you parsed the remainder with a single composite parser.
    * Exercise 11.15
    *
    * For Sampler, the left identity law states if we generate a sample on the basis of the value returned by a
    * first sampler and that sampler always returns the same value, then the sample is generated on the basis of that
    * value. The right identity law states that if we generate a first value and then generate a second on the basis
    * of the first, the result is the sample of the first if the second sampler always returns the value passed in.
    * As for lists, see Exercise 11.11
    * Exercise 11.16
    */
}

/**
  * An implementation of Monad where unit and compose are the primitive methods
  */
trait MonadUC[M[_]] extends MonadBase[M]{

  /**
    * Shows that flatMap can be implemented in terms of compose.
    * Exercise 11.8
    */
  def flatMap[T, S](m: M[T])(f: T => M[S]): M[S] = compose((_: Unit) => m, f)(())

  /**
    * show that map can be implemented in terms of compose and unit
    */
  def map[T, S](m: M[T])(f: T => S): M[S] = compose((_: Unit) => m, (t: T) => unit(f(t)))(())

  /**
    * Shows that join can be written in terms of compose and unit
    */
  def join[T](mm: M[M[T]]): M[T] = compose((_: Unit) => mm, Predef.identity[M[T]])(())

  /**
    * A predicate satisfied by compose and by unit. Unit is the identity element of the compose
    * operation
    */
  def identityLaw[T, S](f: T => M[S]): Boolean =
    compose[T, T, S](unit(_), f) == f && compose[T, S, S](f, unit(_)) == f

  /**
    * Associativity of compose
    */
  def associativeLaw[T, S, U, V](f: T => M[S], g: S => M[U], h: U => M[V]): Boolean =
    compose(compose(f, g), h) == compose(f, compose(g, h))

  /**
    * Note that the basic expression and the UC expression of the associative law are equivalent.
    * compose can be rewritten in terms of flatMap as follows
    *   compose(f, g) == t => flatMap(f(t))(g).
    * So then
    *   compose(compose(f, g), h)  -> t => flatMap(flatMap(f(t))(g))(h)            (1)
    * and
    *   compose(f, compose(g, h))  -> t => flatMap(f(t))(s => flatMap(g(s))(h))    (2)
    *
    * Now, if flatMapLaw holds, then composeLaw holds, because (1) (2) have the form
    *   t => flatMap(flatMap(m)(g))(h) and t => flatMap(m)(s => flatMap(g(s))(h))
    *   and the two images are the same for all values of m, g, and h, including m = f(t)
    * for any f.
    *
    * To prove the converse, just specialize the composeLaw to f: (_: Unit) => m. Then the
    * equality of the functions is the equality of their only images and the only images
    * match the left and right side of the flatMapLaw respectively.
    * Exercise 11.9
    *
    * We can also prove that the two forms of the identity laws are equivalent. From the
    * definition of compose
    *   compose(unit, f) == t => flatMap(unit(t))(f)
    *   compose(f, unit) == t => flatMap(f(t))(unit)
    *
    *  So if assume the identity law to hold for flatMap, then
    *   compose(unit, f) == t => f(t) = f
    *   compose(f, unit) == t => f(t) = f
    *
    *  Conversely if we assume the identity law to hold for compose, then
    *   t => flatMap(unit(t))(f) = f = t => f(t) which implies flatMap(unit(t))(f) = f(t)
    *   t => flatMap(f(t))(unit) = f = t => f(t) which implies  flatMap(f(t))(unit) = f(t)
    * Exercise 11.10
    */
}

/**
  * An implementation of Monad where unit, map and join are the primitive methods
  */
trait MonadUMJ[M[_]] extends MonadBase[M] {

  /**
    * Shows that flatMap can be expressed in terms of map and join
    * Exercise 11.13
    */
  def flatMap[T, S](m: M[T])(f: T => M[S]): M[S] = join(map(m)(f))

  /**
    * Shows that compose can be expressed in terms of map and join
    * Exercise 11.13
    */
  def compose[T, S, U](f: T => M[S], g: S => M[U]): T => M[U] = (t: T) => join(map(f(t))(g))

  /**
    * A predicate satisfied by UMJ. Unit serves as identity
    * Exercise 11.14
    */
  def identityLaw[T, S](t: T, m: M[T]): Boolean =
    join(map(m)(unit(_))) == m && join(unit(m)) == m

  /**
    * Associativity of UMJ
    * Exercise 11.14
    */
  def associativeLaw[T, S, U, V](m: M[M[M[T]]]): Boolean =
    join(join(m)) == join(map(m)(join(_)))
}

object Monad {
  val samplerMonad = new Monad[Sampler] {
    def unit[T](t: => T): Sampler[T] = Sampler.unit(t)
    def flatMap[T, S](F: Sampler[T])(f: T => Sampler[S]): Sampler[S] = F.flatMap(f)
  }

  /**
    * Exercise 11.1
    * TODO. implement it. Exercise 7.13
    */
  val parCompMonad = new Monad[ParComp] {
    def unit[T](t: => T): ParComp[T] = ParComp.unit(t)
    def flatMap[T, S](F: ParComp[T])(f: T => ParComp[S]): ParComp[S] = ???
    //ParComp(es => f(F.run(es).get).run(es))
  }

  /**
    * Exercise 11.1
    */
  val parserMonad = new Monad[Parser] {
    def unit[T](t: => T): Parser[T] = Parser.succeed(t)
    def flatMap[T, S](F: Parser[T])(f: T => Parser[S]): Parser[S] = F.flatMap(f)
  }

  /**
    * Exercise 11.1
    */
  val optionMonad = new Monad[Option] {
    def unit[T](t: => T): Option[T] = Some(t)
    def flatMap[T, S](F: Option[T])(f: T => Option[S]): Option[S] = F.flatMap(f)
  }

  /**
    * Exercise 11.1
    */
  val streamMonad = new Monad[Stream] {
    def unit[T](t: => T): Stream[T] = Stream.cons(t, Stream.empty)
    def flatMap[T, S](F: Stream[T])(f: T => Stream[S]): Stream[S] = F.flatMap(f)
  }

  /**
    * Exercise 11.1
    */
  val listMonad = new Monad[List] {
    def unit[T](t: => T): List[T] = List(t)
    def flatMap[T, S](F: List[T])(f: T => List[S]): List[S] = F.flatMap(f)
  }

  /**
    * Writes StateAction as a monad using type lambdas
    * Exercise 11.2
    *
    * replicateM performs the stateAction m times and collects the results in a list of length m,
    * sequence takes a list of potentially different stateActions and then executes them in sequence,
    * passing around the state and collects the results in a list of the same length. map2 performs
    * two actions passing the state from one to the other and then combines the results of each using f.
    * Exercise 11.18
    *
    * In addition to the two monad laws. There are other laws involving also getState and setState.
    *   getState.flatMap(setState) == unit(())
    *   setState(s).flatMap(_ => getState) == unit(s)
    * Exercise 11.19
    */
  def stateActionMonad[State] = new Monad[({type StateFixed[X] = StateAction[State, X]})#StateFixed] {
    def unit[T](t: => T): StateAction[State, T] = StateAction.unit(t)
    def flatMap[T, S](s: StateAction[State, T])(f: T => StateAction[State, S]): StateAction[State, S] = s.flatMap(f)
  }

  /**
    * A simple wrapper class for value, of type T, used
    * to illustrate the meaning of a monad.
    * Exercise. 11.17
    */
  case class Id[T](value: T) {
    def map[S](f: T => S): Id[S] = Id(f(value))
    def flatMap[S](f: T => Id[S]): Id[S] = f(value)
  }

  /**
    * Exercise 11.17
    */
  def idMonad = new Monad[Id] {
    def unit[T](t: => T): Id[T] = Id(t)
    def flatMap[T, S](id: Id[T])(f: T => Id[S]) = id.flatMap(f)
  }

  /**
    * A class to implement the action of reading with a reader R and returning a
    * result of type T. The way the reader is used to return the result is specified
    * by run.
    * Notice that this is different from Parser, in that the state of the reader is
    * not modified by the action of reading and so we don't need to
    * use StateActions to implement this
    */
  case class Reader[R, T](run: R => T)
  object Reader {
    def getR[R]: Reader[R, R] = Reader(r => r)
  }

  /**
    * The implementation of a Monad for Reader[R, T] where the monadic operations
    * affect only the T-side. The unit operation doesn't use the reader at all and
    * just returns the specified value. The flatMap operation uses the reader twice,
    * first to perform the reading action of r, and then after a new reading action
    * is chosen on the basis of the result of the first reading action using f, to
    * run that second reading action. In addition to the monadic operations the only
    * other primitive operation is getR, which allows one to get the reader R itself.
    * This is necessary, since the monadic operations are orthogonal to R.
    * Here is the meaning of some of the usual operations:
    *   sequence: given a list of reading actions, it performs each in sequence and
    *             collects the results in a list.
    *   join: given a reader whose action returns a reader, it returns the reader inside
    *   replicateM: the same as sequence, but all of the reading actions are identical, so
    *               it generates a list of size m filled with the values of a single call
    *               to run.
    * The identity laws: left means that if you start with a unit reader which always returns
    *                    t and then on the basis of t you choose a new reader, you might as well
    *                    choose the new reader on the basis of the value t, without running the
    *                    unit reader.
    *                    right means that if you run a reader and obtain a result and then run
    *                    the unit reader on the basis of that result, the overall reading action is
    *                    the same as that of the first reader.
    * The associativity laws means that if you read t and then on the basis of t you choose a reader
    *   that gives s and on the basis of s you choose a reader that gives u, that is the same as reading
    *   t and then choosing a new reader with the property that when it receives t it returns u.
    * Exercise 11.20
    */
  def readerMonad[R] = new Monad[({type ReaderFixed[x] = Reader[R, x]})#ReaderFixed] {
    def unit[T](t: => T): Reader[R, T] = Reader(_ => t)
    def flatMap[T, S](r: Reader[R, T])(f: T => Reader[R, S]): Reader[R, S] = Reader(
      (reader: R) => f(r.run(reader)).run(reader)
    )
  }
}
