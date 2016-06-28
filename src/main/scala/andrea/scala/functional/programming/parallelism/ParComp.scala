package andrea.scala.functional.programming.parallelism

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

/**
  * Created by andreapasqua on 10/10/2016.
  */


/**
  * First Implementation:
  *   ParComp represents a computation that could be run on its own thread.
  *           Given a thread manager it generates a future. Upon instantiating
  *           no computation starts.
  *
  *   ExecutorService: it manages the threads. It has a submit method. Whenever
  *                    it submits something the process is run on a thread in a
  *                    pool (or some other mechanism)
  *
  *   Future: Instantiating the future, starts the computation. If you instantiate
  *           it with a submit command then a thread gets assigned, else the
  *           execution is on the thread that created the future.
  *   fork: it takes a ParComp and creates a new ParComp with an extra call to
  *         the Executor service.
  */

/**
  * This class represents a potentially concurrent computation and it
  * contains all you need to wrap evaluation in a Future. When the Future
  * is actually instantiated the computations starts.
  */
case class ParComp[+T](run: (ExecutorService) => Future[T]) {


  /**
    * Goes from a computation to another by mapping the value through f.
    * The evaluation of f is done in the main thread. The evaluation
    * of the argument of f is carried out according to the specifications
    * of the original ParComp
    */
  def map[S](f: T => S): ParComp[S] = ParComp(es => UnitFuture(f(run(es).get)))

  /**
    * Combines the representations of two calculations into
    * a single one which returns f applied to the results of
    * the two computations. The function f is evaluated in the main thread.
    * The arguments to f, according to the recipe in ParComp
    * Exercise 7.1
    * Exercise 7.3
    */
  def map2[S, U](other: ParComp[S])(f: (T, S) => U): ParComp[U] = ParComp(
    es => {
      val (t, s) = PairedFutures(run(es), other.run(es)).get
      UnitFuture(f(t, s))
    }
  )

  /**
    * Combines three ParComps through a ternary function f of their contents
    */
  def map3[T1, T2, S](p1: ParComp[T1], p2: ParComp[T2])(f: (T, T1, T2) => S): ParComp[S] =
    map2(p1)((t, t1) => (t2: T2) => f(t, t1, t2)).map2(p2)((g, t2) => g(t2))

  /**
    * Combines four ParComps through a ternary function f of their contents
    */
  def map4[T1, T2, T3, S](p1: ParComp[T1], p2: ParComp[T2], p3: ParComp[T3])(f: (T, T1, T2, T3) => S): ParComp[S] =
    map3(p1, p2)((t, t1, t2) => (t3: T3) => f(t, t1, t2, t3)).map2(p3)((g, t3) => g(t3))

  /**
    * Combines five ParComps through a ternary function f of their contents
    */
  def map5[T1, T2, T3, T4, S](p1: ParComp[T1], p2: ParComp[T2], p3: ParComp[T3], p4: ParComp[T4])
                             (f: (T, T1, T2, T3, T4) => S): ParComp[S] =
    map4(p1, p2, p3)((t, t1, t2, t3) => (t4: T4) => f(t, t1, t2, t3, t4)).map2(p4)((g, t4) => g(t4))

}

object ParComp {

  /**
    * Constructs a representation for a calculation that just
    * returns the already evaluated argument t without
    * interacting with the ExecutorService. The computation is
    * carried out in the main thread.
    */
  def unit[T](t: T): ParComp[T] = ParComp(_ => UnitFuture(t))

  /**
    * Takes in a representation for a calculation by-name and
    * creates a new representation which is the previous
    * calculation but forked on a new logical thread. Note
    * that this will in general require two thread to be run.
    * One for the evaluation of par.run(es).get, and one for
    * the outer submit. It seems it makes no sense.
    */
  def fork[T](par: => ParComp[T]): ParComp[T] = ParComp(
    es => es.submit(par.run(es).get)
  )

  /**
    * Differs from unit in that t is not pre-calculated.
    * Instead it creates a forked representation of the
    * calculation.
    */
  def lazyUnit[T](t: => T) = fork(unit(t))

  /**
    * Given a function f, it constructs a new function that
    * creates from the same argument a representation of the
    * computation to obtain the output on a forked thread
    * Exercise 7.4
    */
  def asyncF[T, S](f: T => S): T => ParComp[S] = t => lazyUnit(f(t))

  /**
    * given a list of parallel computation returns a computation for the list of values.
    * This version attempts to do no forking in addition to whatever may be stipulated
    * in the direct definitions of the various pars
    * Exercise 7.5
    */
  def sequence[T](pars: IndexedSeq[ParComp[T]]): ParComp[IndexedSeq[T]] = {
    if (pars.isEmpty) unit(Vector.empty)
    else if (pars.length == 1) pars.head.map(Vector(_))
    else {
      val splitIndex = pars.length / 2
      val (l, r) = pars.splitAt(splitIndex)
      fork(sequence(l)).map2(fork(sequence(r)))(_ ++ _)
    }
  }

  /**
    * given a list of parallel computation returns a computation for the list of values.
    * This version attempts to do no forking in addition to whatever may be stipulated
    * in the direct definitions of the various pars
    * Exercise 7.5
    */
  def sequence[T](pars: List[ParComp[T]]): ParComp[List[T]] = pars.foldRight(ParComp.unit[List[T]](Nil))(
    (par, parList) => par.map2(parList)(_ :: _)
  )


  implicit class ParListExtensions[T](l: List[T]) {

    def parComp: ParList = ParList(l)

    case class ParList(list: List[T]) {

      /**
        * given a ParList object applies the function f in parallel.
        */
      def map[S](f: T => S): ParComp[List[S]] = sequence(list.map(asyncF(f)))

      /**
        * filters a list in parallel given a predicate p.
        * Exercise 7.6
        */
      def filter(p: T => Boolean): ParComp[List[T]] = map(t => if (p(t)) Some(t) else None).map(_.flatten)

    }
  }

  implicit class ParIndexedSeqExtensions[T](l: IndexedSeq[T]) {

    def parComp: ParVector = ParVector(l)

    case class ParVector(seq: IndexedSeq[T]) {

      /**
        * Describe
         */
      def fold[S](f: T => S, combine: (S, S) => S): ParComp[S] = {
        assert(seq.nonEmpty, "fold called on empty sequence")
        if (seq.length == 1) unit(f(seq.head))
        else {
          val splitIndex = seq.length / 2
          val (l, r) = seq.splitAt(splitIndex)
          val parL = fork(ParVector(l).fold(f, combine))
          val parR = fork(ParVector(r).fold(f, combine))
          parL.map2(parR)(combine)
        }
      }

      def sum[TT >: T](implicit num: Numeric[TT]) = fold(identity, num.plus)

      def max[TT >: T](implicit ord: Ordering[TT]) = fold(identity, ord.max)

    }
  }
}

/**
  * This contains a logical thread for execution
  */
trait Future[+T] {

  def get: T

  def get(timeout: Duration): T

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCancelled: Boolean

}

/**
  * This is a future that receives an already evaluated value get and doesn't really
  * interact with the executor service
  */
case class UnitFuture[+T](get: T) extends Future[T] {

  def get(timeout: Duration): T = get

  def cancel(evenIfRunning: Boolean): Boolean = false

  val isDone: Boolean = true

  def isCancelled: Boolean = false

}

/**
  * A future that combines two subfutures fut1 and fut2 returning the tuple of the
  * respective outputs while respecting the contract on timeouts.
  * Exercise 7.3
  */
case class PairedFutures[T, S](fut1: Future[T], fut2: Future[S]) extends Future[(T, S)] {

  @volatile var cache: Option[(T, S)] = None // you cache the value in case you need it more than once

  def get: (T, S) = (fut1.get, fut2.get)

  def get(timeout: Duration): (T, S) = cache match {
    case Some(ret) => ret
    case None =>
      val startTime: Long = System.nanoTime()
      val t = fut1.get(timeout)
      val timeElapsed = Duration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
      val s = fut2.get(timeout - timeElapsed)
      val ret = (t, s)
      cache = Some(ret)
      ret
  }

  def cancel(evenIfRunning: Boolean): Boolean = fut1.cancel(evenIfRunning) || fut2.cancel(evenIfRunning)

  def isDone: Boolean = fut1.isDone && fut2.isDone

  def isCancelled = fut1.isCancelled || fut2.isCancelled

}


/**
  * Assigns threads to each process through the submit command.
  * The submit command creates a Future, with the threading info
  * incorporated. Recall only when you instantiate a Future you
  * start computing
  */
trait ExecutorService {
  def submit[T](t: => T): Future[T]
}