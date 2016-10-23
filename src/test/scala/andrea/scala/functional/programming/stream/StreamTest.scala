package andrea.scala.functional.programming.stream

/**
  * Created by andrea on 10/6/16.
  */
object StreamTest extends App {

  val streamEmpty = Stream[Int]()
  val streamOne = Stream(1)
  val streamTwo = Stream(1, 2)
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

  println("Test map")
  assert(streamEmpty.map(_ * 2).toList.isEmpty)
  assert(streamThree.map(_ * 2).toList == List(2, 4, 6))
  // logs
  val stream9: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1") // mapping requires the first element of the stream to be evaluated
  val mappedStream = stream9.map((n: Int) => {println(s"map($n)"); n * 2})
  println("*** map(1)")
  mappedStream.head
  println("*** elem eval.=2")
  mappedStream.tail
  println("*** map(2)")
  mappedStream.tail.head

  println("Test filter")
  assert(streamEmpty.filter(_ != 2).toList.isEmpty)
  assert(streamThree.filter(_ != 2).toList == List(1, 3))
  // logs
  val stream10: Stream[Int] = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1") // filtering requires the first element of the stream to be evaluated
  println("*** filter(1)")
  val filteredStream = stream10.filter((n: Int) => {println(s"filter($n)"); n != 2})
  filteredStream.head
  println("*** elem eval.=2")
  println("*** filter(2)")
  println("*** elem eval.=3")
  println("*** filter(3)")
  filteredStream.tail
  filteredStream.tail.head

  println("Test flatmap")
  def intToStreamTwice(n: Int) = Stream.cons(n, Stream.cons(n, Stream.empty))
  def intToStreamTwiceWithLogging(n: Int) = {println(s"flatMap($n)"); intToStreamTwice(n)}
  assert(streamEmpty.flatMap(intToStreamTwice).isEmpty)
  assert(streamThree.flatMap(intToStreamTwice).toList == List(1, 1, 2, 2, 3, 3))
  // logs
  val stream11 = generateStreamFromThunks(List(1, 2, 3, 4))
  println("*** elem eval.=1") // filtering requires the first element of the stream to be evaluated
  println("*** flatMap(1)")
  val flatMappedStream = stream11.flatMap(intToStreamTwiceWithLogging)
  flatMappedStream.head
  flatMappedStream.tail
  flatMappedStream.tail.head
  println("*** elem eval.=2")
  println("*** flatMap(2)")
  flatMappedStream.tail.tail.head

  println("Test append")
  assert(streamEmpty.append(streamEmpty).isEmpty)
  assert(streamEmpty.append(streamThree).toList == List(1, 2, 3))
  assert(streamThree.append(streamEmpty).toList == List(1, 2, 3))
  assert(streamThree.append(streamThree).toList == List(1, 2, 3, 1, 2, 3))
  // logs
  val stream12a = generateStreamFromThunks(List(1, 2))
  val stream12b = generateStreamFromThunks(List(1, 2))
  println("*** elem eval.=1")
  val stream12 = stream12a.append(stream12b) // appending requires the first element of the stream to be evaluated
  stream12.head
  println("*** elem eval.=2")
  stream12.tail
  stream12.tail.head

  println("Test find")
  assert(streamEmpty.find(_ == 2).isEmpty)
  assert(streamThree.find(_ == 2).get == 2)
  assert(streamThree.find(_ > 3).isEmpty)

  println("Test constant")
  assert(Stream.constant("a").take(4).toList == List.fill(4)("a"))

  println("Test from")
  assert(Stream.from(3).take(4).toList == List(3, 4, 5, 6))

  println("Test unfold")
  def terminatingFunc(n: Int): Option[(Int, Int)] = if (n == 3) None else Some(n, n + 1)
  def nonTerminatingFunc(n: Int): Option[(Int, Int)] = Some(n, n + 1)
  assert(Stream.unfold(0)(terminatingFunc).toList == List(0, 1, 2))
  assert(Stream.unfold(0)(nonTerminatingFunc).take(4).toList == List(0, 1, 2, 3))

  println("Test zip")
  assert(streamEmpty.zip(streamEmpty).isEmpty)
  assert(streamEmpty.zip(streamThree).isEmpty)
  assert(streamThree.zip(streamEmpty).isEmpty)
  assert(streamThree.zip(streamThree).toList == List((1, 1), (2, 2), (3, 3)))
  assert(streamTwo.zip(streamThree).toList == List((1, 1), (2, 2)))

  println("Test zipWith")
  def twoIntsToAString(i: Int, j: Int): String = (i + j).toString
  assert(streamEmpty.zipWith(streamEmpty)(twoIntsToAString).isEmpty)
  assert(streamEmpty.zipWith(streamThree)(twoIntsToAString).isEmpty)
  assert(streamThree.zipWith(streamEmpty)(twoIntsToAString).isEmpty)
  assert(streamThree.zipWith(streamThree)(twoIntsToAString).toList == List("2", "4", "6"))
  assert(streamThree.zipWith(streamTwo)(twoIntsToAString).toList == List("2", "4"))

  println("test zipAll")
  assert(streamEmpty.zipAll(streamEmpty).isEmpty)
  assert(streamEmpty.zipAll(streamThree).toList == List((None, Some(1)), (None, Some(2)), (None, Some(3))))
  assert(streamThree.zipAll(streamEmpty).toList == List((Some(1), None), (Some(2), None), (Some(3), None)))
  assert(streamThree.zipAll(streamThree).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3))))
  assert(streamTwo.zipAll(streamThree).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3))))

  println("Test zipWithAll")
  def twoIntOpsToAString(op1: Option[Int], op2: Option[Int]): String = (op1, op2) match {
    case (Some(i), Some(j)) => (i + j).toString
    case (Some(i), _) => i.toString + " no j"
    case (_, Some(j)) => j.toString + " no i"
    case _ => "no ij"
  }
  assert(streamEmpty.zipWithAll(streamEmpty)(twoIntOpsToAString).isEmpty)
  assert(streamEmpty.zipWithAll(streamThree)(twoIntOpsToAString).toList == List("1 no i", "2 no i", "3 no i"))
  assert(streamThree.zipWithAll(streamEmpty)(twoIntOpsToAString).toList == List("1 no j", "2 no j", "3 no j"))
  assert(streamThree.zipWithAll(streamThree)(twoIntOpsToAString).toList == List("2", "4", "6"))
  assert(streamTwo.zipWithAll(streamThree)(twoIntOpsToAString).toList == List("2", "4", "3 no i"))

  println("Test startsWith")
  assert(streamEmpty.startsWith(streamEmpty))
  assert(!streamEmpty.startsWith(streamThree))
  assert(streamThree.startsWith(streamEmpty))
  assert(streamThree.startsWith(streamTwo))
  assert(!streamTwo.startsWith(streamThree))

  println("Test tails")
  assert(streamEmpty.tails.toList.map(_.toList) == List(Nil))
  assert(streamThree.tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), Nil))

  println("Test scanRight")
  assert(streamEmpty.scanRight(0)(_ + _).toList == List(0))
  assert(streamThree.scanRight(0)(_ + _).toList == List(6, 5, 3, 0))

  def generateStreamFromThunks2(is: List[Int]): scala.collection.immutable.Stream[Int] =
    is.foldRight(scala.collection.immutable.Stream.empty[Int])(
      (i, list) =>scala.collection.immutable.Stream.cons[Int]({println(s"elem. eval.=$i"); i}, list)
    )

}
