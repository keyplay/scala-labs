package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
import sys._

object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.foldLeft(0)((a, b) => if (a > b) a else b)
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (List(), l) => l
      case (l, List()) => l
      case (x1 :: rest1, x2 :: rest2) => (x1 + x2) :: sumOfTwo(rest1, rest2)
    }
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
    def sumOfManyNestedLists(l: List[List[Int]]): List[Int] = {
      l match {
        case head :: tail => sumOfTwo(head, sumOfManyNestedLists(tail))
        case Nil => Nil
      }
    }
    sumOfManyNestedLists(l.toList)
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  //  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {
  //    var boys: ListBuffer[Person] = new ListBuffer[Person]()
  //    var men: ListBuffer[Person] = new ListBuffer[Person]()
  //    var validBoyNames: ListBuffer[String] = new ListBuffer[String]()
  //    var validMenNames: ListBuffer[String] = new ListBuffer[String]()
  //
  //    for (person <- persons) {
  //      if (person.age < 18) {
  //        boys += person
  //      } else {
  //        men += person
  //      }
  //    }
  //
  //    var sortedBoys = boys.toList.sortBy(_.age)
  //    var sortedMen = men.toList.sortBy(_.age)
  //
  //    for (boy <- sortedBoys) {
  //      validBoyNames += boy.firstName
  //    }
  //    for (man <- sortedMen) {
  //      validMenNames += man.firstName
  //    }
  //    List(validBoyNames.toList, validMenNames.toList)
  //  }

  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {
    val (boys, men) = persons.sortBy(_.age).partition(_.age < 18)
    List(boys.map(_.firstName), men.map(_.firstName))
  }

}