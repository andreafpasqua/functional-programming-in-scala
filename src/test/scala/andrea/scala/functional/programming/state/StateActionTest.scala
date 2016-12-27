package andrea.scala.functional.programming.state

import org.scalatest.FunSuite

/**
  * Created by andrea on 10/8/16.
  */
class StateActionTest extends FunSuite {
  import andrea.scala.functional.programming.state.StateAction._

  val rs: RandState = SimpleRandomState(10L)
  val rs1: RandState = SimpleRandomState(15L)
  val one = unit[RandState, Int](1)
  val minusOne = unit[RandState, Int](-1)
  val nextNonNegInt = nextNonNegativeInt

  case class TestState(state: Int)
  val ts: TestState = TestState(0)
  val ts1: TestState = TestState(1)
  val testAction1: StateAction[TestState, String] = StateAction {
    case TestState(state) => (s"${state + 1}+", TestState(state + 1))}
  val testAction2: StateAction[TestState, String] = StateAction {
    case TestState(state) => (s"${2 * state}+", TestState(2 * state))}


  test("Test double3") {
    assert(RandState.double3(rs)._1 ==(0.0017916266806423664, 0.6213264381513, 0.6923740776255727))
    assert(RandState.double3(rs1)._1 ==(0.002687439788132906, 0.06899573700502515, 0.9969301144592464))
  }

  test("Test nextInts") {
    assert(nextInts(5).runAndGetValue(rs) == List(3847489, 1334288366, 1486862010, 711662464, -1453296530))
    assert(nextInts(5).runAndGetValue(rs1) == List(5771233, -148167218, 2140891119, 688156510, -815438513))
  }

  test("Test nextInt") {
    assert(nextInt.runAndGetValue(rs) == 3847489)
    assert(nextInt.runAndGetValue(rs1) == 5771233)
  }

  test("Test nextDouble") {
    assert(nextDouble.runAndGetValue(rs) == 0.0017916266806423664)
    assert(nextDouble.runAndGetValue(rs1) == 0.002687439788132906)
  }

  test("Test map") {
    assert(nextInt.map(_ % 2).runAndGetValue(rs) == nextInt.runAndGetValue(rs) % 2)
    assert(testAction1.map(_.length).runAndGetValue(ts) == testAction1.runAndGetValue(ts).length)
    assert(testAction1.map(_.length).runAndGetValue(ts1) == testAction1.runAndGetValue(ts1).length)
  }

  test("Test map2") {
    assert(nextInt.map2(nextInt)(_ + _).runAndGetValue(rs) == nextInt.runAndGetValue(rs) + nextInt.run(nextInt.run(rs)._2)._1)
    assert(nextInt.map2(one)(_ + _).runAndGetValue(rs) == nextInt.runAndGetValue(rs) + 1)
    assert(testAction1.map2(testAction2)(_ + _).runAndGetValue(ts) == s"${ts.state + 1}+${2 * (ts.state + 1)}+")
    assert(testAction2.map2(testAction1)(_ + _).runAndGetValue(ts) == s"${2 * ts.state}+${2 * ts.state + 1}+")
  }

  test("Test both") {
    val nextIntDouble = nextInt.both(nextDouble)
    val nextDoubleInt = nextDouble.both(nextInt)
    assert(nextIntDouble.runAndGetValue(rs) ==(nextInt.runAndGetValue(rs), nextDouble.run(nextInt.run(rs)._2)._1))
    assert(nextIntDouble.runAndGetValue(rs1) ==(nextInt.runAndGetValue(rs1), nextDouble.run(nextInt.run(rs1)._2)._1))
    assert(nextDoubleInt.runAndGetValue(rs) ==(nextDouble.runAndGetValue(rs), nextInt.run(nextDouble.run(rs)._2)._1))
    assert(testAction1.both(testAction2).runAndGetValue(ts) ==(s"${ts.state + 1}+", s"${2 * (ts.state + 1)}+"))
    assert(testAction2.both(testAction1).runAndGetValue(ts) ==(s"${2 * ts.state}+", s"${2 * ts.state + 1}+"))
  }

  test("Test sequence") {
    val explicitComposition = nextInt.map2(nextInt.map2(unit(List[Int]()))(_ :: _))(_ :: _)
    assert(sequence(List.fill(2)(nextInt)).runAndGetValue(rs) == explicitComposition.runAndGetValue(rs))
    assert(
      sequence(List(testAction1, testAction2, testAction2, testAction1, testAction2)).runAndGetValue(ts) ==
        List("1+", "2+", "4+", "5+", "10+"))
    assert(
      sequence(List(testAction2, testAction1, testAction2, testAction2, testAction1)).runAndGetValue(ts) ==
        List("0+", "1+", "2+", "4+", "5+"))

    /**
      * TODO: sequence gives stack overflow on large lists. Fix it.
      *     sequence(List.fill(100000)(nextInt)).run(SimpleRandomState(0L))
      */
  }

  test("Test flatMap") {
    def flatMapFunc(n: Int): RandAction[String] =
      if (n <= 0) unit("negative") else unit("positive")
    assert(one.flatMap(flatMapFunc).runAndGetValue(rs) == "positive")
    assert(minusOne.flatMap(flatMapFunc).runAndGetValue(rs) == "negative")
  }

  test("Test simulateMachine") {
    import andrea.scala.functional.programming.state.StateAction.{Turn, Coin}
    val lockedFull = MachineState(locked = true, coins = 10, candies = 3)
    val lockedEmpty = MachineState(locked = true, coins = 10, candies = 0)
    val unlockedFull = MachineState(locked = false, coins = 10, candies = 3)
    val unlockedEmpty = MachineState(locked = false, coins = 10, candies = 0)
    val noAction: List[MachineInput] = List.empty
    val insertCoin = List(Coin)
    val turnKnob = List(Turn)
    val keepTurning = List.fill(4)(Turn)
    val keepInserting = List.fill(4)(Coin)
    val mixed1 = List(Turn, Turn, Coin, Coin, Turn, Turn, Coin, Turn)


    assert(simulateMachine(noAction).runAndGetValue(lockedFull) ==(10, 3))
    assert(simulateMachine(noAction).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(noAction).runAndGetValue(unlockedFull) ==(10, 3))
    assert(simulateMachine(noAction).runAndGetValue(unlockedEmpty) ==(10, 0))

    assert(simulateMachine(insertCoin).runAndGetValue(lockedFull) ==(11, 3))
    assert(simulateMachine(insertCoin).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(insertCoin).runAndGetValue(unlockedFull) ==(10, 3))
    assert(simulateMachine(insertCoin).runAndGetValue(unlockedEmpty) ==(10, 0))

    assert(simulateMachine(turnKnob).runAndGetValue(lockedFull) ==(10, 3))
    assert(simulateMachine(turnKnob).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(turnKnob).runAndGetValue(unlockedFull) ==(10, 2))
    assert(simulateMachine(turnKnob).runAndGetValue(unlockedEmpty) ==(10, 0))

    assert(simulateMachine(keepTurning).runAndGetValue(lockedFull) ==(10, 3))
    assert(simulateMachine(keepTurning).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(keepTurning).runAndGetValue(unlockedFull) ==(10, 2))
    assert(simulateMachine(keepTurning).runAndGetValue(unlockedEmpty) ==(10, 0))

    assert(simulateMachine(keepInserting).runAndGetValue(lockedFull) ==(11, 3))
    assert(simulateMachine(keepInserting).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(keepInserting).runAndGetValue(unlockedFull) ==(10, 3))
    assert(simulateMachine(keepInserting).runAndGetValue(unlockedEmpty) ==(10, 0))

    assert(simulateMachine(mixed1).runAndGetValue(lockedFull) ==(12, 1))
    assert(simulateMachine(mixed1).runAndGetValue(lockedEmpty) ==(10, 0))
    assert(simulateMachine(mixed1).runAndGetValue(unlockedFull) ==(12, 0))
    assert(simulateMachine(mixed1).runAndGetValue(unlockedEmpty) ==(10, 0))
  }
}
