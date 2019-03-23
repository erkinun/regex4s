package com.regex4s.adt

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Wildcard pattern" should "match any arbitrary characters" in {
    forAll("strings") { aStr: String =>
      whenever(aStr.nonEmpty) {
        Wildcard.pattern.findFirstIn(aStr) shouldBe Some(aStr.head.toString)
      }
    }
  }

  "AnyDigit pattern" should "match any digit" in {
    forAll("integers") { number: Int =>
      val numStr = number.toString
      val result = AnyDigit.pattern.findFirstIn(numStr)
      if (number >= 0) {
        result shouldBe Some(numStr.head.toString)
      }
      else {
        result shouldBe Some(numStr.charAt(1).toString)
      }
    }
  }

  "AnyNonDigit" should "match any non digit" in {
    forAll("non digits") { nonDigit: String =>
      whenever(nonDigit.nonEmpty && !nonDigit.matches(AnyDigit.pattern.toString())) {
        AnyNonDigit.pattern.findFirstIn(nonDigit) shouldBe Some(nonDigit.firstChar)
      }
    }
  }

  "Only" should "match only the characters it has" in {
    val onlyAbc = Only("abc")

    onlyAbc.pattern.findFirstIn("any") shouldBe Some("a")
    onlyAbc.pattern.findFirstIn("car") shouldBe Some("c")
    onlyAbc.pattern.findFirstIn("bus") shouldBe Some("b")
  }

  it should "not match other characters than specified" in {
    val onlyAbc = Only("abc")

    onlyAbc.pattern.findFirstIn("other") shouldBe None
  }

  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }
}
