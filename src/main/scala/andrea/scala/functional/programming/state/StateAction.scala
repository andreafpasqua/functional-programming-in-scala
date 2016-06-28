package andrea.scala.functional.programming.state

/**
  * Created by andrea on 10/8/16.
  */

/**
  * Represents an action that returns a value but also affects a state
  * and returns the modified state along its return value. The action is
  * run through run(state) and a the return value is extracted through
  * runAndGetValue(state)
  */
case class StateAction[State, +T](run: (State) => (T, State)) {

  /**
    * Constructs an action from the value of another action by means
    * of a function f.
    * Exercise 6.8
    */
  def flatMap[S](f: T => StateAction[State, S]): StateAction[State, S] = StateAction(
    state => {
      val (t, state1) = run(state)
      f(t).run(state1)
    }
  )

  /**
    * Transforms the action by applying f to the return value without
    * further affecting the state
    * Exercise 6.9
    */
  def map[S](f: T => S): StateAction[State, S] = flatMap(t => StateAction.unit(f(t)))

  /**
    * Applies one action after the other (keeping track of the state) and returns
    * the single value obtained by applying f to the result of each action
    * Exercise 6.9
    */
  def map2[S, U](other: StateAction[State, S])(f: (T, S) => U): StateAction[State, U] = flatMap(
    t => other.flatMap(s => StateAction.unit(f(t, s)))
  )

  /**
    * Combines two actions in an action that returns the tuple of the values generated by
    * the individual actions
    */
  def both[S](other: StateAction[State, S]): StateAction[State, (T, S)] = map2(other)((_, _))

  /**
    * Same as both.
    */
  def **[S](other: StateAction[State, S]): StateAction[State, (T, S)] = map2(other)((_, _))

  /**
    * Combines two actions but it only retains the value of the other action.
    * However the first action will still affect the state
    * @return
    */
  def >*[S](other: StateAction[State, S]): StateAction[State, S] = map2(other){case (_, s) => s}

  /**
    * Given a state runs the action and extracts the value
    */
  def runAndGetValue(state: State): T = run(state)._1

  /**
    * Given a state runs the actions and extracts the state
    */
  def runAndGetState(state: State): State = run(state)._2

  /******************** OLDER IMPLEMENTATIONS
    */
  /**
    * Transforms the action by applying f to the return value without
    * further affecting the state
    */
  def mapOld[S](f: T => S): StateAction[State, S] = StateAction(
    state => {
      val (t, state1) = run(state)
      (f(t), state1)
    }
  )

  /**
    * Applies one action after the other (keeping track of the state) and returns
    * the single value obtained by applying f to the result of each action
    * Exercise 6.6
    */
  def map2Old[S, U](other: StateAction[State, S])(f: (T, S) => U): StateAction[State, U] = StateAction(
    state => {
      val (t, state1) = run(state)
      val (s, state2) = other.run(state1)
      (f(t, s), state2)
    }
  )
}

object StateAction {

  /**
    * RandAction is just a type alias for a certain StateAction
    * Exercise 6.10
    */
  type RandAction[T] = StateAction[RandState, T]

  /**
    * Implements an action that-given a state-just returns it as a value without changing it
    */
  def getState[State]: StateAction[State, State] = StateAction(state => (state, state))

  /**
    * Implements an action that returns no value but sets the next state as the one specified
    */
  def setState[State](state: State): StateAction[State, Unit] = StateAction(_ => ((), state))

  /**
    * Implements an action that returns no value but transforms the state it received using
    * the function f
    */
  def modifyState[State](f: State => State): StateAction[State, Unit] =
    StateAction.getState.flatMap(state => StateAction.setState(f(state)))

  /**
    * Implements an action that leaves the state unaltered while always returning t.
    */
  def unit[State, T](t: T): StateAction[State, T] = StateAction(state => (t, state))

  /**
    * Combines actions in a list in a single action that returns the list of results
    * Exercise 6.7
    */
  def sequence[State, T](actions: List[StateAction[State, T]]): StateAction[State, List[T]] =
    actions.foldRight(unit(Nil): StateAction[State, List[T]])(
      (action, result) => action.map2(result)(_ :: _)
    )

  /**
    * Implements an action that returns the next integer from Int.MinValue to Int.MaxValue
    */
  def nextInt: RandAction[Int] = StateAction(_.nextInt)

  /**
    * Implements an action that returns the next non negative integer in [0, Int.MaxValue]
    */
  def nextNonNegativeInt: RandAction[Int] = nextInt.map(i => if (i < 0 ) -(i + 1) else i)

  /**
    * Implements an action that returns the next double in [0, 1)
    * Exercise 6.5
    */
  def nextDouble: RandAction[Double] =
    nextNonNegativeInt.map(i => i / (Int.MaxValue.toDouble + 1))

  /**
    * An action that returns a random list of n integers
    */
  def nextInts(n: Int): RandAction[List[Int]] = sequence(List.fill(n)(nextInt))

  /**
    * An action that returns a random integer non negative and less than n.
    * Exercise 6.8
    */
  def nextNonNegativeIntLessThan(n: Int): RandAction[Int] = nextNonNegativeInt.flatMap(
    i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nextNonNegativeIntLessThan(n)
    }
  )


  /**
    * Express MachineActions
    */

  type MachineAction[+T] = StateAction[MachineState, T]


  /**
    * This converts a machine input to an action. If there are no candies, or if you turn a locked
    * machine or if you insert a coin in an unlocked machine, there is no change to the state.
    * But inserting a coin in an unlocked machine unlocks it and increases the amount of coins.
    * Conversely turning the knob when the machine is unlocked locks the machine and decreases
    * the amount of candies.
    * Exercise 6.11
    */
  def inputToAction(machineInput: MachineInput): MachineAction[Unit] = {
    modifyState(
      (state: MachineState) =>
        (machineInput, state) match {
          case (_, MachineState(_, 0, _)) => state
          case (Turn, MachineState(true, _, _)) => state
          case (Coin, MachineState(false, _, _)) => state
          case (Coin, MachineState(true, candies, coins)) => MachineState(locked = false, candies, coins + 1)
          case (Turn, MachineState(false, candies, coins)) => MachineState(locked = true, candies -1, coins)
      }
    )
  }

  sealed trait MachineInput
  case object Coin extends MachineInput
  case object Turn extends MachineInput

  /**
    * Simulates the overall machine state changes given machine inputs. There are
    * two type of inputs Coin and Turn, resulting in actions insertCoin and turnKnob
    * respectively
    * Exercise 6.11
    */
  def simulateMachine(inputs: List[MachineInput]): MachineAction[(Int, Int)] = {
    val actions = inputs.map(inputToAction)
    for {
      _ <- sequence(actions)
      finalState <- getState
    } yield (finalState.coins, finalState.candies)
  }

}

/**
  * A Random State to be used in random number generation
  */
trait RandState {
  /**
    * Returns a random integer between Int.MinValue and Int.MaxValue
    */
  def nextInt: (Int, RandState)

  /************************* OLD IMPLEMENTATION OF SOME ROUTINES
    */

  /**
    * Returns a random nonnegative integer between 0 and Int.MaxValue. This works
    * because Int.MinValue is larger by 1 than Int.MaxValue in absolute value
    * Exercise 6.1
    */
  def nextNonNegativeInt: (Int, RandState) = {
    val (int, rng) = nextInt
    (if (int < 0) -(int + 1) else int, rng)
  }

  /**
    * Generates a random double from 0 to 1
    * Exercise 6.2
    */
  def nextDouble: (Double, RandState) = {
    val (int, rng) = nextNonNegativeInt
    (int / (Int.MaxValue.toDouble + 1), rng)
  }

  /**
    * returns n random integers from Int.MinValue to Int.MaxValue
    * Exercise 6.4
    */
  def nextInts(n: Int): (List[Int], RandState) = {
    def go(n: Int, rng: RandState, list: List[Int]): (List[Int], RandState) =
      if (n == 0) (list, rng)
      else {
        val (i, newRng) = rng.nextInt
        go(n - 1, newRng, list ++ List(i))
      }
    go(n, this, List.empty)
  }

}

object RandState {

  /**
    * Given a random number generator, it returns an int and a double as well as the updated rng
    * Exercise 6.3
    */
  def intDouble(rng: RandState): ((Int, Double), RandState) = {
    val (int, rng1) = rng.nextInt
    val (double, rng2) = rng1.nextDouble
    ((int, double), rng2)
  }

  /**
    * Given a random number generator, it returns a double and an int as well as the updated rng
    * Exercise 6.3
    */
  def doubleInt(rng: RandState): ((Double, Int), RandState) = {
    val ((int, double), rng1) = intDouble(rng)
    ((double, int), rng1)
  }

  /**
    * Given a random number generator, it returns three doubles as well as the updated rng
    * Exercise 6.3
    */
  def double3(rng: RandState): ((Double, Double, Double), RandState) = {
    val (d1, rng1) = rng.nextDouble
    val (d2, rng2) = rng1.nextDouble
    val (d3, rng3) = rng2.nextDouble
    ((d1, d2, d3), rng3)
  }

}

case class SimpleRandomState(seed: Long) extends RandState {

  def nextInt: (Int, RandState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRandomState(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}

/**
  * Expresses the state of a machine. The machine dispenses
  * candies for coins. It can be locked or unlocked
  */
case class MachineState(locked: Boolean, candies: Int, coins: Int)