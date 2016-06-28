package andrea.scala.functional.programming.state

import andrea.scala.functional.programming.state.StateAction.MachineInput

/**
  * Created by andrea on 10/8/16.
  */
object StateActionTest extends App {

  val rs: RandState = SimpleRandomState(10L)
  val rs1: RandState = SimpleRandomState(15L)
  val one = StateAction.unit[RandState, Int](1)
  val minusOne = StateAction.unit[RandState, Int](-1)
  val nextInt = StateAction.nextInt
  val nextNonNegInt = StateAction.nextNonNegativeInt
  val nextDouble = StateAction.nextDouble


  println("Test double3")
  assert(RandState.double3(rs)._1 == (0.0017916266806423664,0.6213264381513,0.6923740776255727))
  assert(RandState.double3(rs1)._1 == (0.002687439788132906,0.06899573700502515,0.9969301144592464))

  println("Test nextInts")
  assert(StateAction.nextInts(5).runAndGetValue(rs) == List(3847489, 1334288366, 1486862010, 711662464, -1453296530))
  assert(StateAction.nextInts(5).runAndGetValue(rs1) == List(5771233, -148167218, 2140891119, 688156510, -815438513))

  println("Test nextInt")
  assert(nextInt.runAndGetValue(rs) == 3847489)
  assert(nextInt.runAndGetValue(rs1) == 5771233)

  println("Test nextDouble")
  assert(nextDouble.runAndGetValue(rs) == 0.0017916266806423664)
  assert(nextDouble.runAndGetValue(rs1) == 0.002687439788132906)

  println("Test map")
  assert(nextInt.map(_ % 2).runAndGetValue(rs) == nextInt.runAndGetValue(rs) % 2)

  println("Test map2")
  assert(nextInt.map2(nextInt)(_ + _).runAndGetValue(rs) == nextInt.runAndGetValue(rs) + nextInt.run(nextInt.run(rs)._2)._1)
  assert(nextInt.map2(one)(_ + _).runAndGetValue(rs) == nextInt.runAndGetValue(rs) + 1)

  println("Test both")
  val nextIntDouble = nextInt.both(nextDouble)
  val nextDoubleInt = nextDouble.both(nextInt)
  assert(nextIntDouble.runAndGetValue(rs) == (nextInt.runAndGetValue(rs), nextDouble.run(nextInt.run(rs)._2)._1))
  assert(nextIntDouble.runAndGetValue(rs1) == (nextInt.runAndGetValue(rs1), nextDouble.run(nextInt.run(rs1)._2)._1))
  assert(nextDoubleInt.runAndGetValue(rs) == (nextDouble.runAndGetValue(rs), nextInt.run(nextDouble.run(rs)._2)._1))

  println("Test sequence")
  val explicitComposition = nextInt.map2(nextInt.map2(StateAction.unit(List[Int]()))(_ :: _))(_ :: _)
  assert(StateAction.sequence(List.fill(2)(nextInt)).runAndGetValue(rs) == explicitComposition.runAndGetValue(rs))

  println("Test flatMap")
  def flatMapFunc(n: Int): StateAction.RandAction[String] =
    if (n <= 0) StateAction.unit("negative") else StateAction.unit("positive")
  assert(one.flatMap(flatMapFunc).runAndGetValue(rs) == "positive")
  assert(minusOne.flatMap(flatMapFunc).runAndGetValue(rs) == "negative")

  println("Test simulateMachine")
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

  assert(StateAction.simulateMachine(noAction).runAndGetValue(lockedFull) == (10, 3))
  assert(StateAction.simulateMachine(noAction).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(noAction).runAndGetValue(unlockedFull) == (10, 3))
  assert(StateAction.simulateMachine(noAction).runAndGetValue(unlockedEmpty) == (10, 0))

  assert(StateAction.simulateMachine(insertCoin).runAndGetValue(lockedFull) == (11, 3))
  assert(StateAction.simulateMachine(insertCoin).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(insertCoin).runAndGetValue(unlockedFull) == (10, 3))
  assert(StateAction.simulateMachine(insertCoin).runAndGetValue(unlockedEmpty) == (10, 0))

  assert(StateAction.simulateMachine(turnKnob).runAndGetValue(lockedFull) == (10, 3))
  assert(StateAction.simulateMachine(turnKnob).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(turnKnob).runAndGetValue(unlockedFull) == (10, 2))
  assert(StateAction.simulateMachine(turnKnob).runAndGetValue(unlockedEmpty) == (10, 0))

  assert(StateAction.simulateMachine(keepTurning).runAndGetValue(lockedFull) == (10, 3))
  assert(StateAction.simulateMachine(keepTurning).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(keepTurning).runAndGetValue(unlockedFull) == (10, 2))
  assert(StateAction.simulateMachine(keepTurning).runAndGetValue(unlockedEmpty) == (10, 0))

  assert(StateAction.simulateMachine(keepInserting).runAndGetValue(lockedFull) == (11, 3))
  assert(StateAction.simulateMachine(keepInserting).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(keepInserting).runAndGetValue(unlockedFull) == (10, 3))
  assert(StateAction.simulateMachine(keepInserting).runAndGetValue(unlockedEmpty) == (10, 0))

  assert(StateAction.simulateMachine(mixed1).runAndGetValue(lockedFull) == (12, 1))
  assert(StateAction.simulateMachine(mixed1).runAndGetValue(lockedEmpty) == (10, 0))
  assert(StateAction.simulateMachine(mixed1).runAndGetValue(unlockedFull) == (12, 0))
  assert(StateAction.simulateMachine(mixed1).runAndGetValue(unlockedEmpty) == (10, 0))

}
