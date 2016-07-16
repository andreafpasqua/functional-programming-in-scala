package andrea.scala.functional.programming

/**
  * Created by andrea on 7/2/16.
  */

object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)

    def nonNegativeInt: (Int, RNG) = {
      val (n, newRNG) = nextInt
      val finalN =
        if (n == Int.MinValue) 0
        else if (n < 0) -n
        else n
      (finalN, newRNG)
    }

    def double: (Double, RNG) = {
      val (n, newRNG) = nonNegativeInt
      val d = -n.toDouble / Int.MinValue
      (d, newRNG)
    }

  }

  object RNG {

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng1) = rng.nextInt
      val (d, rng2) = rng1.double
      ((i, d), rng2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), newRng) = intDouble(rng)
      ((d, i), newRng)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = rng.double
      val (d2, rng2) = rng1.double
      val (d3, rng3) = rng2.double
      ((d1, d2, d3), rng3)
    }

    def ints(n: Int)(rng: RNG): (List[Int], RNG) =
      (0 until n).toList.foldLeft((Nil: List[Int], rng)) {
        case ((l, r), _) =>
          val (newI, newR) = r.nextInt
          (l :+ newI, newR)
      }

  }

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[A] = State[RNG, A]
  case class RandOld[A](run: RNG => (A, RNG)) {

    def flatMap[B](op: A => RandOld[B]): RandOld[B] = {
      val newStateChange =
        (rng: RNG) => {
          val (a, rng1) = this.run(rng)
          op(a).run(rng1)
        }
      new RandOld[B](newStateChange)
    }

    def map[B](op: A => B): RandOld[B] =
      this.flatMap {
        a: A => Chapter6.RandOld.unit(op(a))
      }

    def map2[B, C](other: RandOld[B])(op: (A, B) => C): RandOld[C] =
      this.flatMap {
        a: A =>
          other.map(op(a, _))
      }

    def mapNoFlatMap[B](op: A => B): RandOld[B] = {
      val newStateChange =
        (rng: RNG) => {
          val (a, rng1) = this.run(rng)
          (op(a), rng1)
        }
      new RandOld[B](newStateChange)
    }

    def map2NoFlatMap[B, C](other: RandOld[B])(op: (A, B) => C): RandOld[C] = {
      val newStateChange =
        (rng: RNG) => {
          val (a, rng1) = this.run(rng)
          val (b, rng2) = other.run(rng1)
          (op(a, b), rng2)
        }
      new RandOld[C](newStateChange)
    }

    def apply(rng: RNG): A = run(rng)._1

  }

  object RandOld {

    def unit[A](a: A): RandOld[A] = new RandOld[A]((a, _))

    def nonNegativeInt(n: Int): RandOld[Int] = new RandOld[Int](_.nonNegativeInt)

    def nonNegativeEven: RandOld[Int] = {
      val nonNegative: RandOld[Int] = new RandOld[Int](_.nonNegativeInt)
      nonNegative.map(i => i - i % 2)
    }

    def nonNegativeLessThan(n: Int): RandOld[Int] = {
      nonNegativeInt(n).flatMap {
        i =>
          val mod = i % n
          if (i + (n - 1) - mod >= 0)
            unit(mod)
          else
            nonNegativeInt(n)
      }
    }

    def int: RandOld[Int] = new RandOld[Int](_.nextInt)

    def double: RandOld[Double] = {
      val nonNegative: RandOld[Int] = new RandOld[Int](_.nonNegativeInt)
      nonNegative.map(-_.toDouble / Int.MinValue)
    }

    def both[A, B](ra: RandOld[A], rb: RandOld[B]): RandOld[(A, B)] =
      ra.map2(rb)((_, _))

    def randIntDouble: RandOld[(Int, Double)] = both(int, double)

    def randDoubleInt: RandOld[(Double, Int)] = both(double, int)

    def sequence[A](rl: List[RandOld[A]]): RandOld[List[A]] =
      rl.foldRight(unit(Nil: List[A])) {
        (r, l) => r.map2(l)(_ :: _)
      }

    def ints(n: Int)(rng: RNG): List[Int] = sequence(List.fill(n)(int))(rng)

    def rollDie: RandOld[Int] = nonNegativeLessThan(6).map(_ + 1)

  }

  case class State[S, +A](run: S => (A, S)) {

    def map[B](op: A => B): State[S, B] =
      flatMap(a => State.unit(op(a)))

    def map2[B, C](other: State[S, B])(op: (A, B) => C): State[S, C] =
      flatMap(
        a => other.map(b => op(a, b))
      )

    def flatMap[B](op: A => State[S, B]): State[S, B] = {
      val newRun =
        (s: S) => {
          val (a, newS) = run(s)
          op(a).run(newS)
        }
      State(newRun)
    }
  }

  object State {

    def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

    def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
      l.foldRight(unit[S, List[A]](Nil)) {
        (state1, state2) => state1.map2(state2)(_ :: _)
      }

  }

}

object TestChapter6 extends App {

  import Chapter6.RandOld._
  import Chapter6._

  val smallNum = 10
  val largeNum = 100
  val seed = 1

  val rng1: RNG = SimpleRNG(seed)
  val manyNonNegativeLessThan =
    sequence(List.fill(smallNum)(nonNegativeLessThan(largeNum)))

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) =>
      val (n, newRng) = rng.nonNegativeInt
      println(s"$n")
      newRng
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (d, newRng) = rng.double
      println(s"$d")
      newRng
    }
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (iD, newRng) = Chapter6.RNG.intDouble(rng)
      println(s"$iD")
      newRng
    }
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (dI, newRng) = Chapter6.RNG.doubleInt(rng)
      println(s"$dI")
      newRng
    }
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (ddd, newRng) = Chapter6.RNG.double3(rng)
      println(s"$ddd")
      newRng
    }
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (ddd, newRng) = Chapter6.RNG.double3(rng)
      println(s"$ddd")
      newRng
    }
  }

  (0 until smallNum).foldLeft(rng1) {
    (rng, _) => {
      val (ddd, newRng) = Chapter6.RNG.double3(rng)
      println(s"$ddd")
      newRng
    }
  }

  println(s"ints(10, rng1) = ${Chapter6.RNG.ints(10)(rng1)}")
  println(s"ints(10, rng1) = ${Chapter6.RandOld.ints(10)(rng1)}")
  println(s"rng1.nextInt = ${rng1.nextInt}")
  println(s"manyNonNegativeLessThan(rng1)= ${manyNonNegativeLessThan(rng1)}")
  println(
    s"sequence(List.fill(100)(rollDie))(rng1) = ${sequence(List.fill(100)(rollDie))(rng1)}"
  )

  val list1 = List(1, 2, 3, 4)
  println(list1.sum)

}

