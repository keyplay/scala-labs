package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
/**
 * In this Lab you will implement a Specs2 testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a Specs2 specification to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class Specs2ExerciseTest extends Specification {
  "Exercise: Euro with divider method" should {
    "divide correctly when divider is > 0" in {
      val res = new Euro(2, 4) / 2
      res.euro ==== 1
      res.cents ==== 2
    }

    "throw an IllegalArgumentException when divider is == 0" in {
      new Euro(2, 4) / 0 must throwA[IllegalArgumentException]
    }

    "throw an IllegalArgumentException when divider is < 0" in {
      new Euro(2, 4) / -1 must throwA[IllegalArgumentException]
    }
  }
}
