package andrea.scala.functional.programming.stream

/**
  * Created by andrea on 10/6/16.
  */
object StreamTest extends App {

  val streamEmpty = Stream[Int]()
  val streamOne = Stream(1)
  val streamThree = Stream(1, 2, 3)

  def generateStreamFromThunks(is: List[Int]): Stream[Int] =
    is.foldRight(Stream.empty[Int])(
      (i, list) => Stream.cons[Int]({println(s"elem. eval.=$i"); i}, list)
    )

  println("Test  head and tail")
  assert(streamOne.head == 1)
  assert(streamThree.tail.toList == List(2, 3))
  assert(streamThree.tail.head == 2)
  // logs
  val stream1: Stream[Int] = generateStreamFromThunks(List(1, 2, 3))
  println("*** elem eval.=1")
  stream1.head
  stream1.head // nothing evaluated
  stream1.tail // nothing evaluated
  println("*** elem eval.=2")
  stream1.tail.head
  stream1.tail.head // nothing evaluated


  println("Test headOption")
  assert(streamEmpty.headOption.isEmpty)
  assert(streamThree.headOption.get == 1)
  // logs
  val stream2: Stream[Int] = generateStreamFromThunks(List(1, 2, 3))
  println("*** elem eval.=1")
  stream2.headOption.get
  stream2.headOption.get // nothing evaluated

  println("Test toList")
  assert(streamThree.toList == List(1, 2, 3))
  assert(streamEmpty.toList.isEmpty)


  println("Test take")
  assert(streamEmpty.take(1).toList.isEmpty)
  assert(streamThree.take(2).toList == List(1, 2))
  // logs
  val stream3: Stream[Int] = generateStreamFromThunks(List(1, 2, 3))
  stream3.take(2) // nothing evaluated

  println("Test takeWhile")
  assert(streamEmpty.takeWhile(_ < 3).toList.isEmpty)
  assert(streamThree.takeWhile(_ < 3).toList == List(1, 2))
  assert(streamThree.takeWhile(_ < 0).toList.isEmpty)
  // logs
  val stream4: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4, 5))
  println("*** elem eval.=1")
  stream4.takeWhile(_ < 3) // unfortunately the first item has to be evaluated but none of the others

  println("Test drop")
  assert(streamEmpty.drop(1).toList.isEmpty)
  assert(streamThree.drop(2).toList == List(3))
  assert(streamThree.drop(3).toList.isEmpty)
  assert(streamThree.drop(4).toList.isEmpty)
  // logs
  val stream5: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  stream5.drop(2) // nothing evaluated

  println("Test exists")
  assert(!streamEmpty.exists(_ > 0))
  assert(streamThree.exists(_ > 1))
  assert(!streamThree.exists(_ < 0))
  // logs
  val stream6: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1")
  println("*** elem eval.=2")
  stream6.exists(_ > 1) // it evaluates nothing after it satisfies the condition

  println("Test forall")
  assert(streamEmpty.forall(_ > 0))
  assert(!streamThree.forall(_ > 1))
  assert(streamEmpty.forall(_ > 0))
  // logs
  val stream7: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1")
  println("*** elem eval.=2")
  stream7.forall(_ < 2) // it evaluates nothing after it violates the condition

  println("Test foldRight")
  def foldRightFunc(i: Int, j: => Int): Int = if (i == 2) 0 else i + j
  assert(streamEmpty.foldRight(0)(foldRightFunc) == 0)
  assert(streamThree.foldRight(0)(foldRightFunc) == 1)
  //logs
  val stream8: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1")
  println("*** elem eval.=2")
  stream8.foldRight(0)(foldRightFunc) // foldRightFunction stops the recursion if i==2, so only two elements evaluated

}
