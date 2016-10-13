package andrea.scala.functional.programming.testgeneration

/**
  * Created by andrea on 10/13/16.
  */

/**
  * Generates examples of the type T
  */
trait GenExamples[+T] {

  def forall(p: T => Boolean): Prop

}

object GenExamples {

  /**
    * Generates examples of T (with an ordering ord) in the interval [lowerbound, upperbound]
    */
  def chooseInInterval[T](lowerBound: T, upperBound: T)
                         (implicit ord: Ordering[T]): GenExamples[T] = ???

  /**
    * Given a way to generate examples of type T, it generates examples of List[T]
    */
  def listOf[T](examples: GenExamples[T]): GenExamples[List[T]] = ???
}

/**
  * Contains all the information about a property that
  * may or may not hold for an object and how to test
  * it against a set of examples.
  */
trait Prop {

  import Prop._
  /**
    * If the property holds for all examples returns how many
    * examples were tested, if it doesn't it returns a failed case
    * as a string.
    */
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(other: => Prop): Prop = new Prop {
    def check = ???
  }


  /**
    * True if the property holds for all examples, false if it
    * can be falsified by at least one of them
    */
  def checkOld: Boolean

  /**
    * Combines two Props in a Prop that checks true if both props check true
    * Exercise 8.3
    */
  def oldAndAnd(other: => Prop): Prop = new Prop {
    def checkOld = this.checkOld && other.checkOld
  }

  /**
    * Combines two Props in a Prop that checks true if at least one of the props check true
    */
  def ||(other: => Prop): Prop = new Prop {
    def checkOld = this.checkOld || other.checkOld
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}