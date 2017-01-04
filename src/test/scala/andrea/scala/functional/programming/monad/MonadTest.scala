package andrea.scala.functional.programming.monad

import org.scalatest.FunSuite
import andrea.scala.functional.programming.list.List
import andrea.scala.functional.programming.option.{Option, Some, None}

/**
  * Created by andrea on 12/29/16.
  */

class MonadTest extends FunSuite {
  import Monad._

  /**
    * Exercise 11.5
    */
  test("replicateM on list monads") {
    val list = List(1, 2, 3)
    val listCombinations = for {
      l1 <- list
      l2 <- list
      l3 <- list
      l4 <- list
    } yield List(l1, l2, l3, l4)
    assert(listMonad.replicateM(list)(4) === listCombinations)
  }

  /**
    * Exercise 11.5
    */
  test("replicateM on option monads") {
    val inside = 1
    val n = 4
    assert(optionMonad.replicateM(Some(inside))(n) === Some(List.fill(n)(inside)))
    assert(optionMonad.replicateM(None)(n).isEmpty)
  }

  test("product on list monads") {
    val list1 = List(1, 2, 3)
    val list2 = List('a', 'b')
    val tupleCombinations = for {
      i <- list1
      c <- list2
    } yield (i, c)
    assert(listMonad.product(list1, list2) === tupleCombinations)
  }

  test("product on option monads") {
    val option1 = Some(1)
    val option2 = None
    val option3 = Some('a')
    assert(optionMonad.product(option1, option2).isEmpty)
    assert(optionMonad.product(option1, option3) === Some((1, 'a')))
    assert(optionMonad.product(option2, option2).isEmpty)
  }

  test("filterM on list monads") {
    val listToFilter = List(1, 2, 3)
    def mapToBoolean(i: Int): List[Boolean] = i match {
      case 1 => List(false, true)
      case 2 => List(false, true, true, true)
      case 3 => List(true, false)
    }

    val result = for {
      bool1 <- mapToBoolean(1)
      bool2 <- mapToBoolean(2)
      bool3 <- mapToBoolean(3)
    } yield List((1, bool1), (2, bool2), (3, bool3)).filter(_._2).map(_._1)

    assert(listMonad.filterM(listToFilter)(mapToBoolean) === result)
  }

  test("filterM on option monads") {
    val listToFilter = List(1, 2, 3)
    def mapToBooleanNoNone(i: Int): Option[Boolean] = i match {
      case 1 => Some(true)
      case 2 => Some(false)
      case 3 => Some(true)
    }
    def mapToBooleanWithNone(i: Int): Option[Boolean] = i match {
      case 1 => None
      case 2 => Some(false)
      case 3 => Some(true)
    }
    assert(optionMonad.filterM(listToFilter)(mapToBooleanNoNone) === Some(List(1, 3)))
    assert(optionMonad.filterM(listToFilter)(mapToBooleanWithNone).isEmpty)
  }
}
