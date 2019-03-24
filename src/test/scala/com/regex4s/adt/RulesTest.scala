package com.regex4s.adt

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Wildcard pattern" should "match any arbitrary characters" in {
    forAll("strings") { aStr: String =>
      whenever(aStr.nonEmpty) {
        Wildcard.matches(aStr) shouldBe Some(aStr.head.toString)
      }
    }
  }

  "AnyDigit pattern" should "match any digit" in {
    forAll("integers") { number: Int =>
      val numStr = number.toString
      val result = AnyDigit.matches(numStr)
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
        AnyNonDigit.matches(nonDigit) shouldBe Some(nonDigit.firstChar)
      }
    }
  }

  // TODO think about escaping chars in the string cases
  // or other cases that might fail
  "Only" should "match only the characters it has" in {
    val onlyAbc = Only("abc")
    val abcGen = Gen.listOf(Gen.oneOf("a", "b", "c")).flatMap(_.mkString(""))
    forAll(abcGen) { abc: String =>
      whenever(abc.nonEmpty) {
        onlyAbc.matches(abc) shouldBe Some(abc.firstChar)
      }
    }
  }

  it should "not match other characters than specified" in {
    val onlyAbc = Only("abc")

    onlyAbc.matches("other") shouldBe None
  }

  "Not" should "not match characters other than it specifies" in {
    val notAbc = Not("abc")
    val abcGen = Gen.listOf(Gen.oneOf("a", "b", "c")).flatMap(_.mkString(""))

    forAll(abcGen) { abc: String =>
      whenever(abc.nonEmpty) {
        notAbc.matches(abc) shouldBe None
      }
    }
  }

  // TODO generator for excluding a set
  it should "match other characters" in {
    val notAbc = Not("abc")
    notAbc.matches("other") shouldBe Some("other".firstChar)
  }

  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }

  implicit class RuleMatchExtender(r: Rule) {
    def matches(s: String): Option[String] = r.pattern.findFirstIn(s)
  }
}
