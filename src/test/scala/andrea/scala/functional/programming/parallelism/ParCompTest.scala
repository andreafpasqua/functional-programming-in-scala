package andrea.scala.functional.programming.parallelism

/**
  * Created by andrea on 10/11/16.
  */

object ParCompTest extends App {

  /**
    * A fake executor service with a
    * revolving thread pool
    */
  class DebugES(numThreads: Int) extends ExecutorService {

    var threadsOpen: Array[Boolean] = Array.fill(numThreads)(true)

    /**
      * This assigns a thunk to a logical thread for execution
      */
    def submit[T](t: => T): Future[T] = {
      threadsOpen.indexWhere(identity) match {
        case -1 => submit(t)
        case n =>
          println(s"Evaluating in thread $n")
          threadsOpen(n) = false
          val ret = UnitFuture(t)
          threadsOpen(n) = true
          ret
      }
    }
  }

}