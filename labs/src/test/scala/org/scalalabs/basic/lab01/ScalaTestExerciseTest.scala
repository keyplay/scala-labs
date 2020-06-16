package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
/**
 * In this Lab you will implement a ScalaTest testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a ScalaTest using a Spec of your choice to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
//@RunWith(classOf[JUnitRunner])
class ScalaTestExerciseTest extends AnyFunSpecLike {
  describe("Exercise: Euro with divider method") {
    describe("when divider is > 0") {
      it("should divide correctly") {
        val res = new Euro(2, 4) / 2
        assert(res.euro === 1)
        assert(res.cents === 2)
      }
    }

    describe("when divider is == 0") {
      it("should throw an IllegalArgumentException") {
        assertThrows[IllegalArgumentException] { new Euro(2, 4) / 0 }
      }
    }

    describe("when divider is < 0") {
      it("should throw an IllegalArgumentException") {
        assertThrows[IllegalArgumentException] { new Euro(2, 4) / -1 }
      }
    }
  }
}
