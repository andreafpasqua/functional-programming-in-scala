package andrea.scala.functional.programming

/**
  * Created by andrea on 6/27/16.
  */


object Chapter4 {

  trait Option[+A] {
    def map[B](f: A => B): Option[B]

    def flatMap[B](f: A => Option[B]): Option[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option[B]): Option[B]

    def filter(f: A => Boolean): Option[A]
    def unapply(): Option[A]
    def get: A
  }

  class Some[A](a: A) extends Option[A]  {
   def map[B] (f: A => B): Option[B] = {
     new Some(f(a))
   }
   def flatMap[B](f: A => Option[B]): Option[B] = {
     f(a)
   }
   def getOrElse[B >: A] (default: => B): B = a
   def orElse[B >: A] (ob: => Option[B]): Option[B] = this
   def filter(f: A => Boolean): Option[A] = {
     if (f(a)) {
       this
     }
     else {
       None
     }
   }

    def apply(a: A): Option[A] = new Some(a)
    def unapply(): Option[A] = new Some(a)
    def get: A = a
  }

  object None extends Option[Nothing] {
    def map[B] (f: Nothing => B): Option[B] = None
    def flatMap[B](f: Nothing => Option[B]): Option[B] = None

    def getOrElse[B](default: => B): B = default

    def orElse[B](ob: => Option[B]): Option[B] = ob

    def filter(f: Nothing => Boolean): Option[Nothing] = None
    def unapply(): Option[Nothing] = None
    def get: Nothing = throw new NoSuchElementException
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else new Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val mu: Option[Double] = mean(xs)
    mu.flatMap {
      m => {
        val demeaned = xs.map(x => math.pow(x - m, 2))
        mean(demeaned)
      }
    }
  }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b:Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap( x => b.map(f(x, _)))

  def Try[A](a: => A): Option[A] = {
    try new Some(a)
    catch { case e: Exception => None}
  }

  // Exercise 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => new Some(Nil)
    case None :: rest => None
    case a :: rest => sequence(rest).map(list => a.get :: list)
  }


    // Exercise 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => new Some(Nil)
    case a :: rest => f(a).flatMap {
      b => traverse(rest)(f).map(list => b :: list)
      }
    }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(x => x)
}

object TestChapter4 extends App {
  
  class Parent(val a: Int)
  class Child(override val a: Int) extends Parent(a)

  val p: Chapter4.Option[Parent] = new Chapter4.Some(new Parent(1))
  val c: Chapter4.Option[Parent] = new Chapter4.Some(new Child(2))
  val p1: Chapter4.Option[Parent] = Chapter4.None
  val c1: Chapter4.Option[Parent] = Chapter4.None

  val p2: Chapter4.Some[Parent] = new Chapter4.Some(new Parent(3))
  val c2: Chapter4.Some[Parent] = new Chapter4.Some(new Child(4))

  val mappedP = p.orElse(new Chapter4.Some("a"))
  val mappedC = c.orElse(new Chapter4.Some("a"))
  val mappedP1 = p1.orElse(new Chapter4.Some("a"))
  val mappedC1 = c1.orElse(new Chapter4.Some("a"))

  val xs = Vector(1D, 2D, 3D)
  val ys = Vector(1D)
  val zs = Vector()

  val meanX = Chapter4.mean(xs)
  val meanY = Chapter4.mean(ys)
  val meanZ = Chapter4.mean(zs)
  val varX = Chapter4.variance(xs)
  val varY = Chapter4.variance(ys)
  val varZ = Chapter4.variance(zs)

  println(s"meanX=$meanX, meanY=$meanY, meanZ=$meanZ")

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Chapter4.Option[Double] = {
    val ageInt = Chapter4.Try(age.toInt)
    val ticketsInt = Chapter4.Try(numberOfSpeedingTickets.toInt)
    Chapter4.map2(ageInt, ticketsInt)(insuranceRateQuote)
  }

  val optionList = List(p, c, p1)
  val optionList2 = List(p, p1, c)
  val optionList3 = List(p, c, p2, c2)

  val optionalList = Chapter4.sequence(optionList)
  val optionalList2 = Chapter4.sequence(optionList2)
  val optionalList3 = Chapter4.sequence(optionList3)
  val optionalListA = Chapter4.sequence2(optionList)
  val optionalListA2 = Chapter4.sequence2(optionList2)
  val optionalListA3 = Chapter4.sequence2(optionList3)

  val temp = 1

  List(1,2,3).sum
}