package com.regex4s.adt

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "AnyCharacter pattern" should "match any arbitrary characters" in {
    val r = AnyCharacter.pattern

    forAll("strings") { aStr: String =>
      whenever(aStr.nonEmpty) {
        r.findFirstIn(aStr) shouldBe Some(aStr.head.toString)
      }
    }
  }

  "AnyDigit pattern" should "match any digit" in {
    val r = AnyDigit.pattern
    forAll("integers") { number: Int =>
      val numStr = number.toString
      val result = r.findFirstIn(numStr)
      if (number >= 0) {
        result shouldBe Some(numStr.head.toString)
      }
      else {
        result shouldBe Some(numStr.charAt(1).toString)
      }

    }
  }
}
