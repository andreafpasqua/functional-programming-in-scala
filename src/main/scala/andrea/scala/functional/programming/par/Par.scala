package andrea.scala.functional.programming.par

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

/**
  * Created by andreapasqua on 10/10/2016.
  */


/**
  * This describes a potentially concurrent calculation that
  * could be wrapped in a Future given an executor service
  */
case class Par[+T](run: (ExecutorService) => Future[T]) {


  def map[S](f: T => S): Par[S] = Par(es => UnitFuture(f(run(es).get)))

  /**
    * Combines the representations of two calculations into
    * a single one which returns f applied to the results of
    * the two computations. Note that all forking
    * has to be done explicitly
    * Exercise 7.1
    * Exercise 7.3
    */
  def map2[S, U](other: Par[S])(f: (T, S) => U): Par[U] = Par(
    es => {
      val (t, s) = PairedFutures(run(es), other.run(es)).get
      UnitFuture(f(t, s))
    }
  )

}

object Par {

  /**
    * Constructs a representation for a calculation that just
    * returns the already evaluated argument t without
    * interacting with the ExecutorService
    */
  def unit[T](t: T): Par[T] = Par(_ => UnitFuture(t))

  /**
    * Takes in a representation for a calculation by-name and
    * creates a new representation which is the previous
    * calculation but forked on a new logical thread.
    */
  def fork[T](par: => Par[T]): Par[T] = Par(
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
  def asyncF[T, S](f: T => S): T => Par[S] = t => lazyUnit(f(t))

}

class ExecutorService {

  /**
    * This assigns a thunk to a logical thread for execution
    */
  def submit[T](t: => T): Future[T] = ???

}

/**
  * This contains a logical thread for execution
  *
  * @tparam T
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
    case None => {
      val startTime: Long = System.nanoTime()
      val t = fut1.get(timeout)
      val timeElapsed = Duration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
      val s = fut2.get(timeout - timeElapsed)
      val ret = (t, s)
      cache = Some(ret)
      ret
    }
  }

  def cancel(evenIfRunning: Boolean): Boolean = fut1.cancel(evenIfRunning) || fut2.cancel(evenIfRunning)

  def isDone: Boolean = fut1.isDone && fut2.isDone

  def isCancelled = fut1.isCancelled || fut2.isCancelled

}