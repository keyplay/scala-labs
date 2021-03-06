package org.scalalabs.basic.lab03

import java.io.{ IOException, InputStream }

import scala.io.Source
import scala.util.control._
import scala.util.{ Success, Try }
import sys._

object OptionExercise {

  /**
   * This map contains sample testdata to clarify this exercise.
   * It contains key value pairs where:
   * - the key is a room number
   * - the value can be:
   * -- the amount of people in the room (filled: Some("10"), empty: None)
   * -- the room is not available (Some("locked"))
   */
  val sampleRooms = Map(1 -> Some("12"), 2 -> None, 3 -> Some("locked"), 4 -> Some("14"), 5 -> Some("8"), 6 -> Some("locked"))

  /**
   * Implement the room state method that should return the state of a room as a String as follows:
   * - filled: return total people:     E.g: Some("12") is "12"
   * - locked: return "not available"   E.g. Some("locked") is "not available"
   * - empty:  return "empty"	        E.g. None is "empty"
   * - does not exist: 					"not existing"
   */
  def roomState(rooms: Map[Int, Option[String]], room: Int): String = {
    rooms.getOrElse(room, Some("not existing")).map(value =>
      if (value == "locked") "not available" else value).getOrElse("empty")
  }

}

object EitherExercise {

  /**
   * Implement the reciprocal method that should return the reciprocal of a number. Every number has
   * a reciprocal, defined by the formula 1/number, except 0 (1/0 is undefined).
   * Use Either to make it explicit that this function can fail in case of:
   * - unparseable input
   * - 0 as an input
   *
   * Expected output for inputs:
   * - Right(5) -> Right(0.2)
   * - Right(0) -> Left(IllegalArgumentException("Reciprocal of 0 does not exist!"))
   * - Left("2") -> Right(0.5)
   * - Left("foo") -> Left(NumberFormatException)
   *
   */
  def reciprocal(input: Either[String, Int]): Either[Throwable, Double] = {
    input.fold(
      str =>
        Exception.catching(classOf[NumberFormatException]).either(str.toInt), // try parse to an int
      i => Right(i))
      .filterOrElse(i => i != 0, new IllegalArgumentException("Reciprocal of 0 does not exist!"))
      .map(1.0 / _)
  }

}

object TryExercise {

  /**
   * Rewrite the the method implementation of print(...) using {@code Try} instead of try/catch.
   * Make sure all tests keep succeeding.
   *
   * Hint: You can make use {@code Try}'s convenience methods such as recover, flatMap, transform, foreach etc.
   */

  //  def print(inputStream: InputStream): Unit = {
  //    val readResult = try {
  //      Source.fromInputStream(inputStream).mkString
  //    } catch {
  //      case e: IOException => "Couldn't read input stream!"
  //    }
  //    val result = try {
  //      inputStream.close()
  //      readResult
  //    } catch {
  //      case throwable: Throwable => s"Error: Failed to close! $readResult"
  //    }
  //    println(result)
  //  }

  def print(inputStream: InputStream): Unit = {
    Try(Source.fromInputStream(inputStream).mkString).recover {
      case e: IOException => "Couldn't read input stream!"
    }.flatMap {
      str => Try(inputStream.close()).transform(_ => Success(str), _ => Success(s"Error: Failed to close! $str"))
    }.foreach(println)
  }
}