import java.util.concurrent.TimeUnit

import scala.concurrent.duration
import scala.concurrent.duration.{Duration, FiniteDuration, TimeUnit}

/**
  * Created by andrea on 7/6/16.
  */
object Chapter7 {

  // low level implementation
  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = ??? // submit will spawn a thread

  }

  trait Callable[A] { def call: A }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean

  }

  private case class unitFuture[A](a: A) extends Future[A] {
    def get: A = a
    def isCancelled: Boolean = false
    def get(timeout: Long, unit: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isDone: Boolean = true

  }

  //For Exercise 7.3
  private case class map2Future[A, B, C](
    a: Future[A],
    b: Future[B],
    op: (A, B) => C
  ) extends Future[C] {
    def get: C = op(a.get, b.get)
    def isCancelled: Boolean = a.isCancelled || b.isCancelled
    def get(timeout: Long, unit: TimeUnit): C = {
      val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
      val startTime = System.nanoTime()
      val aVal = a.get(timeout, unit)
      val newTime = System.nanoTime()
      val timeElapsed = newTime - startTime
      val bVal = b.get(timeoutNanos - timeElapsed, TimeUnit.NANOSECONDS)
      op(aVal, bVal)
    }
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def isDone: Boolean = a.isDone && b.isDone
  }

  // First Implementation of Par
  type Par[A] = ExecutorService => Future[A]

  implicit class ParExtensions[A](par: Par[A]) {

    def run(es: ExecutorService): Future[A] = par(es)

    def map[B](op: A => B): Par[B] = map2(Par.unit({}))((a, _) => op(a))

    def map2[B, C](other: Par[B])(op: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val thisFut = par(es) // this launches execution in a thread
        val otherFut = other(es) // this also launches execution possibly in a different thread
        unitFuture(op(thisFut.get, otherFut.get)) // no new thread
      }

    // For Exercise 7.3. It also returns the time elapsed in the same units
    def map2WithTimeout[B, C](other: Par[B])
                             (op: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val thisFut = par(es) // this launches execution in a thread
      val otherFut = other(es) // this also launches execution possibly in a different thread
      map2Future(thisFut, otherFut, op)
    }

  }

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => unitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](a: => Par[A]): Par[A] = // unevaluated argument, runs Par in a new thread
      (es: ExecutorService) => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    // Exercise 7.4. It also returns the time elapsed in the same units
    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

    // Exercise 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(Nil: List[A]))((par, list) => par.map2(list)(_ :: _))

    // the same as list for maps but does it in parallel
    def parMap[A, B](as: List[A])(op: A => B): Par[List[B]] =
      sequence(as.map(asyncF(op)))

    // Exercise 7.6
    def parFilter[A](as: List[A])(op: A => Boolean): Par[List[A]] =
      sequence(as.flatMap(a => if (op(a)) Some(lazyUnit(a)) else None))

  }

}
