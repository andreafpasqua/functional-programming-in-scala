package andrea.scala.functional.programming.stream

/**
  * Created by andrea on 10/6/16.
  */
object StreamLogging extends App {

  /**
    * These logs are to ensure that lazy evaluation is
    * proceeding as expected.
    * To check that the logging of the calls in this object is as
    * printed out, just swap consWithLogs for cons
    */

  println("Use the debug versions to check lazy behavior")
  lazy val thunk1 = {println("elem eval. =1"); 1}
  lazy val thunk2 = {println("elem eval. =2"); 2}
  lazy val thunk3 = {println("elem eval. =3"); 3}
  val stream1: Stream[Int] = Stream.cons(thunk1, Stream.cons(thunk2, Stream.cons(thunk3, Stream.empty)))

  println("*** Logs for head and tail")
  println("*** elem eval. =1")
  stream1.head

  println("*** cons called")
  println("*** cons: tail evaluated=...")
  println("*** cons called")
  stream1.head
  stream1.head
  stream1.tail

  println("*** cons: head evaluated=2")
  stream1.head
  stream1.head
  stream1.tail.head
  stream1.tail.head

  println("*** Logs for headOption")
  println("*** cons called")
  println("*** cons: head evaluated=1")
  val stream2: Stream[Int] = Stream(1, 2, 3)
  stream2.headOption.get

  println("*** Logs for take")
  println("*** cons called") // in the creation of stream3
  println("*** cons called") // in the creation of the take stream
  val stream3: Stream[Int] = Stream(1, 2, 3)
  stream3.take(2)

  println("*** Logs for takeWhile")
  println("*** cons called") // in the creation of stream4
  println("*** cons: head evaluated=1")
  println("*** cons called") // in the creation of the take stream
  val stream4: Stream[Int] = Stream(1, 2, 3, 4)
  stream4.takeWhile(_ < 3)

  println("*** Logs for drop")
  println("*** cons called") // in the creation of stream5
  println("*** cons called") // in the creation of the stream that drops 1
  println("*** tail evaluated=...")
  println("*** cons called") // in the creation of the stream that drops 2
  println("*** tail evaluated=...")
  println("*** cons called") // in the creation of the stream that drops 2
  val stream5: Stream[Int] = Stream(1, 2, 3, 4)
  stream5.drop(2)

  println("*** Logs for exists")
  println("*** cons called") // in the creation of stream6
  println("*** cons: head evaluated=1") // when checking the first element
  println("*** cons called") // in the creation of the tail
  println("*** tail evaluated=...")
  println("*** cons called") // in the creation of the tail
  println("*** cons: head evaluated=2") // when checking the second element
  val stream6: Stream[Int] = Stream(1, 2, 3, 4)
  stream6.exists(_ > 1)

  println("*** Logs for forall")
  println("*** cons called") // in the creation of stream7
  println("*** cons: head evaluated=1") // when checking the first element
  println("*** cons called") // in the creation of the tail
  println("*** tail evaluated=...")
  println("*** cons called") // in the creation of the tail
  println("*** cons: head evaluated=2") // when checking the second element
  val stream7: Stream[Int] = Stream(1, 2, 3, 4)
  stream7.forall(_ < 2)

  println("*** Logs for foldRight")
  println("*** cons called") // in the creation of stream8
  println("*** cons: head evaluated=1") // when checking the first element
  println("*** cons called") // in the creation of the tail
  println("*** tail evaluated=...")
  println("*** cons called")
  println("*** cons: head evaluated=2") // when checking the second element
  val stream8: Stream[Int] = Stream(1, 2, 3, 4)
  def foldRightFunc(i: Int, j: => Int): Int = if (i == 2) 0 else i + j
  stream8.foldRight(0)(foldRightFunc)
}

object StreamTest extends App {
  val streamEmpty = Stream[Int]()
  val streamOne = Stream(1)
  val streamThree = Stream(1, 2, 3)

  println("Test head and tail")
  assert(streamOne.head == 1)
  assert(streamThree.tail.toList == List(2, 3))
  assert(streamThree.tail.head == 2)

  println("Test headOption")
  assert(streamEmpty.headOption.isEmpty)
  assert(streamThree.headOption.get == 1)

  println("Test toList")
  assert(streamThree.toList == List(1, 2, 3))
  assert(streamEmpty.toList.isEmpty)

  println("Test take")
  assert(streamEmpty.take(1).toList.isEmpty)
  assert(streamThree.take(2).toList == List(1, 2))

  println("Test takeWhile")
  assert(streamEmpty.takeWhile(_ < 3).toList.isEmpty)
  assert(streamThree.takeWhile(_ < 3).toList == List(1, 2))
  assert(streamThree.takeWhile(_ < 0).toList.isEmpty)

  println("Test drop")
  assert(streamEmpty.drop(1).toList.isEmpty)
  assert(streamThree.drop(2).toList == List(3))
  assert(streamThree.drop(3).toList.isEmpty)
  assert(streamThree.drop(4).toList.isEmpty)

  println("Test exists")
  assert(!streamEmpty.exists(_ > 0))
  assert(streamThree.exists(_ > 1))
  assert(!streamThree.exists(_ < 0))

  println("Test forall")
  assert(streamEmpty.forall(_ > 0))
  assert(!streamThree.forall(_ > 1))
  assert(streamEmpty.forall(_ > 0))

  println("Test foldRight")
  def foldRightFunc(i: Int, j: => Int): Int = if (i == 2) 0 else i + j
  assert(streamEmpty.foldRight(0)(foldRightFunc) == 0)
  assert(streamThree.foldRight(0)(foldRightFunc) == 1)

}
