package org.scalalabs.basic.lab03

import sys._

/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement,
 * even though it provides many more possibilites. Whereas the Java switch statmenet
 * lets you 'match' primitive types up to int's, Scala's pattern matching goes much
 * further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * Pattern matching is also often used in combination with recursive algorithms.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the
 * corresponding unit test work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

/**
 * ***********************************************************************
 * pattern matching exercises
 * For expected solution see unittest @PatternMatchingExerciseTest
 * ***********************************************************************
 */
object PatternMatchingExercise01 {

  case class Person(name: String, age: Int)

  def matchOnInputType(in: Any) = {
    in match {
      case str: String => s"A string with length ${str.length}"
      case int: Int if (int > 0) => "A positive integer"
      case Person(name, _) => s"A person with name: $name"
      case seq: Seq[_] if (seq.length > 10) => "Seq with more than 10 elements"
      case head :: second :: tail => s"first: $head, second: $second, rest: $tail"
      case _: Option[_] => "A Scala Option subtype"
      case null => "A null value"
      case _ => "Some Scala class"
    }
  }

}

/**
 * ***********************************************************************
 * Partial functions exercise.
 * The MessageTransformer must use the PartialFunction[Any, Any] called transform to transform messages in its process(msg:Any) method
 * - For every input message that can be transformed update the count using updateCount(...)
 * - Messages that cannot be transformed must be returned as is, no count is updated.
 * Provide an implementation only using PartialFunctions (if statements are not allowed) to make the unittest succeed.
 * For expected behaviour see @PatternMatchingExerciseTest
 * ***********************************************************************
 */
object PatternMatchingExercise02 {

  class MessageTransformer(private val transform: PartialFunction[Any, Any]) {

    private var transformationCount: Map[Class[_], Int] = Map().withDefaultValue(0)

    //    def process(message: Any): Any = {
    //      if (transform.isDefinedAt(message)) {
    //        updateCount(message)
    //        transform(message)
    //      } else message
    //    }

    def process(message: Any): Any = {
      transform.andThen(res => {
        updateCount(message)
        res
      }).applyOrElse(message, (_: Any) => message)
    }

    private def updateCount(message: Any) = {
      transformationCount = transformationCount + (message.getClass -> (transformationCount(message.getClass) + 1))
    }

    def transformationCountBy(clazz: Class[_]): Int = transformationCount(clazz)

  }

}

