package andrea.scala.functional.programming

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.TimeUnit

/**
  * Created by andrea on 7/6/16.
  */
object Chapter7 {


  // low level implementation. So far it's completely bogus
  class ExecutorService {
    // submit will spawn a physical thread in a proper implementation
    def submit[A](a: Callable[A]): Future[A] = new UnitFuture(a.call)

  }

  trait Callable[A] {
    def call: A
  }

  trait Future[A] {
    def get: A

    // we expect get to block the thread that is evaluation the content of the future
    def get(timeout: Long, unit: TimeUnit): A

    def cancel(evenIfRunning: Boolean): Boolean

    def isDone: Boolean

    def isCancelled: Boolean

  }

  private case class UnitFuture[A](a: A) extends Future[A] {
    def get: A = a

    def isCancelled: Boolean = false

    def get(timeout: Long, unit: TimeUnit): A = get

    def cancel(evenIfRunning: Boolean): Boolean = false

    def isDone: Boolean = true

  }

  //For Exercise 7.3
  private case class map2Future[A, B, C](a: Future[A], b: Future[B],
                                         op: (A, B) => C) extends Future[C] {
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

    def run(es: ExecutorService): A = par(es).get

    def mapViaFlatMap[B](op: A => B): Par[B] = flatMap(a => Par.unit(op(a)))

    def map[B](op: A => B): Par[B] = map2(Par.unit({}))((a, _) => op(a))

    def map2[B, C](other: Par[B])(op: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val thisFut = par(es) // this launches execution in a thread
        val otherFut = other(es) // this also launches execution possibly in a different thread
        UnitFuture(op(thisFut.get, otherFut.get)) // no new thread. Also notice that get blocks the current thread
      }

    // For Exercise 7.3. It also returns the time elapsed in the same units
    def map2WithTimeout[B, C](other: Par[B])
                             (op: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val thisFut = par(es) // this launches execution in a thread
        val otherFut = other(es) // this also launches execution possibly in a different thread
        map2Future(thisFut, otherFut, op)
      }

    def flatMap[B](op: A => Par[B]): Par[B] = es => op(par.run(es))(es)

  }

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](a: => Par[A]): Par[A] = {
      // unevaluated argument, runs Par in a new thread
      println("New Thread")
      (es: ExecutorService) => es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    // Exercise 7.4. It also returns the time elapsed in the same units
    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

    // Exercise 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(Nil: List[A]))((par, list) => par.map2(list)(_ :: _))

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      n.flatMap(choices)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      cond.flatMap(if (_) t else f)

    def choiceMap[K, V](key: Par[K])(map: Map[K, Par[V]]): Par[V] =
      key.flatMap(map)

    def join[A](pp: Par[Par[A]]): Par[A] =
      es => pp.run(es)(es)

    def flatMapViaJoin[A, B](p: Par[A])(op: A => Par[B]): Par[B] =
      join(p.map(op))

    def joinViaFlatMap[A](pp: Par[Par[A]]): Par[A] =
      pp.flatMap(identity[Par[A]])

    implicit class ListParExt[A](as: List[A]) {
      // the same as list for maps but does it in parallel

      def par7: ListPar[A] = new ListPar(as)

      class ListPar[A](as: List[A]) {

        def map[B](op: A => B): Par[List[B]] =
          sequence(as.map(asyncF(op)))

        // Exercise 7.6
        def filter(op: A => Boolean): Par[List[A]] =
          sequence(as.flatMap(a => if (op(a)) Some(lazyUnit(a)) else None))

      }

      def parMap[B](op: A => B): Par[List[B]] =
        sequence(as.map(asyncF(op)))

      // Exercise 7.6
      def parFilter(op: A => Boolean): Par[List[A]] =
        sequence(as.flatMap(a => if (op(a)) Some(lazyUnit(a)) else None))

    }

  }
}

object TestChapter7 extends App {

  import Chapter7.Par.ListParExt
  import Chapter7._

  val es = new ExecutorService
  val listLen = 15
  val list1: List[Int] = (1 to 15).toList
  println(s"list1 = $list1 ")

  println(s"**** list1.par7.map(_ + 1).run(es).get = ${list1.par7.map(_ + 1).run(es)}")
  println(s"**** list1.par7.filter(_ % 2 == 0).run(es).get = ${list1.par7.filter(_ % 2 == 0).run(es)}")

}
