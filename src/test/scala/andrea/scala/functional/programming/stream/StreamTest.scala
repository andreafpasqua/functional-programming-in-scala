package andrea.scala.functional.programming.stream

import org.scalatest.FunSuite
import andrea.scala.functional.programming.option.{None, Option, Some}

/**
  * Created by andrea on 10/6/16.
  */

class StreamTest extends FunSuite {

  val streamEmpty = Stream[Int]()
  val streamOne = Stream(1)
  val streamTwo = Stream(1, 2)
  val streamThree = Stream(1, 2, 3)
  val streamLong = Stream.unfold(1)( s => if (s < 100001) Some(s, s + 1) else None)

  def generateStreamFromList(is: List[Int]): Stream[Int] =
    is.foldRight(Stream.empty[Int])(
      (i, list) => Stream.cons[Int]({println(s"elem. eval.=$i"); i}, list)
    )

  test("* Test head and tail") {
    assert(streamOne.head == 1)
    assert(streamThree.tail.toList == List(2, 3))
    assert(streamThree.tail.head == 2)
    assert(streamLong.head == 1)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3))
    println("\telem eval.=1")
    stream.head
    stream.head // nothing evaluated
    stream.tail // nothing evaluated
    println("\telem eval.=2")
    stream.tail.head
    stream.tail.head // nothing evaluated
  }

  test("* Test foldRight") {
    def lazySum(i: Int, accum: => Int): Int = if (i > 2) 0 else i + accum
    assert(streamEmpty.foldRight(0)(lazySum) == 0)
    assert(streamThree.foldRight(0)(lazySum) == 3)
    assert(streamLong.foldRight(0)(lazySum) == 3)
    //logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    println("\tlazySum stops the recursion if i>2, so only three elements are evaluated")
    println("\telem eval.=1")
    println("\telem eval.=2")
    println("\telem eval.=3")
    stream.foldRight(0)(lazySum)

    /**
      * Note that foldRight is not tail-recursive, so if you use a non-terminating
      * condition on a long list, it will give Stack-overflow, e.g.
      * streamLong.foldRight(0) (_ + _)
      */
  }

  test("* Test foldLeft") {
    def lazySum(accum: Int, i: => Int) = if (accum > 2) 3 else i + accum
    assert(streamEmpty.foldLeft(0)(lazySum) == 0)
    assert(streamThree.foldLeft(0)(lazySum) == 3)
    assert(streamLong.foldLeft(0)(lazySum) == 3)
    //logs
    val stream: Stream[Int] = generateStreamFromList((1 to 10).toList)
    println("\tlazySum2 stops the recursion if accum >2, so only two elements are evaluated")
    println("\telem eval.=1")
    println("\telem eval.=2")
    stream.foldLeft(0)(lazySum)
  }

  test("* Test headOption") {
    assert(streamEmpty.headOption.isEmpty)
    assert(streamThree.headOption.get == 1)
    assert(streamLong.headOption.get == 1)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3))
    println("\telem eval.=1")
    stream.headOption.get
    stream.headOption.get // nothing evaluated
  }

  test("* Test toList") {
    assert(streamThree.toList == List(1, 2, 3))
    assert(streamEmpty.toList.isEmpty)
    assert(streamLong.toList == (1 to 100000).toList)
  }

  test("* Test take") {
    assert(streamEmpty.take(1).toList.isEmpty)
    assert(streamThree.take(2).toList == List(1, 2))
    assert(streamLong.take(99999).toList == (1 until 100000).toList)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3))
    stream.take(2) // nothing evaluated
  }

  test("* Test takeWhile") {
    assert(streamEmpty.takeWhile(_ < 3).toList.isEmpty)
    assert(streamThree.takeWhile(_ < 3).toList == List(1, 2))
    assert(streamThree.takeWhile(_ < 0).toList.isEmpty)
    assert(streamLong.takeWhile(_ < 100000).toList == (1 until 100000).toList)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4, 5))
    println("\telem eval.=1")
    stream.takeWhile(_ < 3) // unfortunately the first item has to be evaluated but none of the others
  }

  test("* Test drop") {
    assert(streamEmpty.drop(1).toList.isEmpty)
    assert(streamThree.drop(2).toList == List(3))
    assert(streamThree.drop(3).toList.isEmpty)
    assert(streamThree.drop(4).toList.isEmpty)
    assert(streamLong.drop(99999).toList == List(100000))
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    stream.drop(2) // nothing evaluated
  }

  test("* Test exists") {
    assert(!streamEmpty.exists(_ > 0))
    assert(streamThree.exists(_ > 1))
    assert(!streamThree.exists(_ < 0))
    assert(!streamLong.exists(_ > 100000))
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    println("\texists evaluates nothing after an element satisfies the condition")
    println("\telem eval.=1")
    println("\telem eval.=2")
    stream.exists(_ > 1)
  }

  test("* Test forall") {
    assert(streamEmpty.forall(_ > 0))
    assert(!streamThree.forall(_ > 1))
    assert(streamEmpty.forall(_ > 0))
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    println("\tforall evaluates nothing after an element violates the condition")
    println("\telem eval.=1")
    println("\telem eval.=2")
    stream.forall(_ < 2)
  }

  test("* Test map") {
    assert(streamEmpty.map(_ * 2).toList.isEmpty)
    assert(streamThree.map(_ * 2).toList == List(2, 4, 6))
    assert(streamLong.map(_ + 1).toList == (2 to 100001).toList)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    println("\tmap requires the first element of the original stream to be evaluated")
    println("\telem eval.=1")
    val mappedStream = stream.map((n: Int) => {
      println(s"map($n)"); n * 2
    })
    println("\tmap(1)")
    mappedStream.head
    println("\telem eval.=2")
    mappedStream.tail
    println("\tmap(2)")
    mappedStream.tail.head
  }

  test("* Test filter") {
    assert(streamEmpty.filter(_ != 2).toList.isEmpty)
    assert(streamThree.filter(_ != 2).toList == List(1, 3))
    assert(streamLong.filter(_ != 2).toList == 1 :: (3 to 100000).toList)
    // logs
    val stream: Stream[Int] = generateStreamFromList(List(1, 2, 3, 4))
    println("\telem eval.=1") // filtering requires the first element of the stream to be evaluated
    println("\tfilter(1)")
    val filteredStream = stream.filter((n: Int) => {
      println(s"filter($n)"); n != 2
    })
    filteredStream.head
    println("\telem eval.=2")
    println("\tfilter(2)")
    println("\telem eval.=3")
    println("\tfilter(3)")
    filteredStream.tail
    filteredStream.tail.head
  }

  test("* Test flatMap") {
    def intToStreamTwice(n: Int) = Stream.cons(n, Stream.cons(n, Stream.empty))
    def intToStreamTwiceWithLogging(n: Int) = {
      println(s"flatMap($n)"); intToStreamTwice(n)
    }
    assert(streamEmpty.flatMap(intToStreamTwice).isEmpty)
    assert(streamThree.flatMap(intToStreamTwice).toList == List(1, 1, 2, 2, 3, 3))
    assert(streamLong.flatMap(intToStreamTwice).toList == (1 to 100000).toList.flatMap(i => List(i, i)))
    // logs
    val stream = generateStreamFromList(List(1, 2, 3, 4))
    println("\telem eval.=1") // filtering requires the first element of the stream to be evaluated
    println("\tflatMap(1)")
    val flatMappedStream = stream.flatMap(intToStreamTwiceWithLogging)
    flatMappedStream.head
    flatMappedStream.tail
    flatMappedStream.tail.head
    println("\telem eval.=2")
    println("\tflatMap(2)")
    flatMappedStream.tail.tail.head
  }

  test("* Test append") {
    assert(streamEmpty.append(streamEmpty).isEmpty)
    assert(streamEmpty.append(streamThree).toList == List(1, 2, 3))
    assert(streamThree.append(streamEmpty).toList == List(1, 2, 3))
    assert(streamThree.append(streamThree).toList == List(1, 2, 3, 1, 2, 3))
    assert(streamLong.append(streamLong).toList == (1 to 100000).toList ++ (1 to 100000).toList)
    // logs
    val streama = generateStreamFromList(List(1, 2))
    val streamb = generateStreamFromList(List(1, 2))
    println("\telem eval.=1")
    val stream = streama.append(streamb) // appending requires the first element of the stream to be evaluated
    stream.head
    println("\telem eval.=2")
    stream.tail
    stream.tail.head
  }

  test("* Test find") {
    assert(streamEmpty.find(_ == 2).isEmpty)
    assert(streamThree.find(_ == 2).get == 2)
    assert(streamThree.find(_ > 3).isEmpty)
    assert(streamLong.find(_ > 100000).isEmpty)
    assert(streamLong.find(_ == 99999).get == 99999)
  }

  test("* Test constant") {
    assert(Stream.constant("a").take(4).toList == List.fill(4)("a"))
  }

  test("* Test from") {
    assert(Stream.from(3).take(4).toList == List(3, 4, 5, 6))
  }

  test("* Test unfold") {
    def terminatingFunc(n: Int): Option[(Int, Int)] = if (n == 3) None else Some(n, n + 1)
    def nonTerminatingFunc(n: Int): Option[(Int, Int)] = Some(n, n + 1)
    assert(Stream.unfold(0)(terminatingFunc).toList == List(0, 1, 2))
    assert(Stream.unfold(0)(nonTerminatingFunc).take(4).toList == List(0, 1, 2, 3))
  }

  test("* Test zip") {
    assert(streamEmpty.zip(streamEmpty).isEmpty)
    assert(streamEmpty.zip(streamThree).isEmpty)
    assert(streamThree.zip(streamEmpty).isEmpty)
    assert(streamThree.zip(streamThree).toList == List((1, 1), (2, 2), (3, 3)))
    assert(streamTwo.zip(streamThree).toList == List((1, 1), (2, 2)))
    assert(streamLong.zip(streamThree).toList == List((1, 1), (2, 2), (3, 3)))
    assert(streamLong.zip(streamLong).toList == (1 to 100000).toList.map(i => (i, i)))
  }

  test("* Test zipWith") {
    def twoIntsToAString(i: Int, j: Int): String = (i + j).toString
    assert(streamEmpty.zipWith(streamEmpty)(twoIntsToAString).isEmpty)
    assert(streamEmpty.zipWith(streamThree)(twoIntsToAString).isEmpty)
    assert(streamThree.zipWith(streamEmpty)(twoIntsToAString).isEmpty)
    assert(streamThree.zipWith(streamThree)(twoIntsToAString).toList == List("2", "4", "6"))
    assert(streamThree.zipWith(streamTwo)(twoIntsToAString).toList == List("2", "4"))
    assert(streamLong.zipWith(streamThree)(twoIntsToAString).toList == List("2", "4", "6"))
    assert(streamLong.zipWith(streamLong)(twoIntsToAString).toList == (1 to 100000).toList.map(i => (i * 2).toString))
  }

  test("test zipAll") {
    assert(streamEmpty.zipAll(streamEmpty).isEmpty)
    assert(streamEmpty.zipAll(streamThree).toList == List((None, Some(1)), (None, Some(2)), (None, Some(3))))
    assert(streamThree.zipAll(streamEmpty).toList == List((Some(1), None), (Some(2), None), (Some(3), None)))
    assert(streamThree.zipAll(streamThree).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3))))
    assert(streamTwo.zipAll(streamThree).toList == List((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3))))
    assert(streamLong.zipAll(streamThree).toList == (1 to 100000).toList.map(
      i => if (i < 4) (Some(i), Some(i)) else (Some(i), None)))
    assert(streamLong.zipAll(streamLong).toList == (1 to 100000).toList.map(i => (Some(i), Some(i))))
  }

  test("* Test zipWithAll") {
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
    assert(streamLong.zipWithAll(streamThree)(twoIntOpsToAString).toList == (1 to 100000).toList.map(
      i => if (i < 4) (i * 2).toString else i.toString + " no j"))
    assert(streamLong.zipWithAll(streamLong)(twoIntOpsToAString).toList == (1 to 100000).toList.map(
      i => (i * 2).toString))
  }

  test("* Test startsWith") {
    assert(streamEmpty.startsWith(streamEmpty))
    assert(!streamEmpty.startsWith(streamThree))
    assert(streamThree.startsWith(streamEmpty))
    assert(streamThree.startsWith(streamTwo))
    assert(!streamTwo.startsWith(streamThree))
    assert(streamLong.startsWith(streamThree))
    assert(!streamThree.startsWith(streamLong))
    assert(streamLong.startsWith(streamLong))
  }

  test("* Test tails") {
    assert(streamEmpty.tails.toList.map(_.toList) == List(Nil))
    assert(streamThree.tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), Nil))
    assert(streamLong.tails.take(2).toList.map(_.toList) == List((1 to 100000).toList, (2 to 100000).toList))
  }

  test("* Test scanRight") {
    assert(streamEmpty.scanRight(0)(_ + _).toList == List(0))
    assert(streamThree.scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }

}
