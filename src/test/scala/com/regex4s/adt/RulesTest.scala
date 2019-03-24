package com.regex4s.adt

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val abcGen = Gen.listOf(Gen.oneOf("a", "b", "c"))
    .flatMap(_.mkString(""))
    .filter(_.nonEmpty)

  "Wildcard pattern" should "match any arbitrary characters" in {
    forAll(Gen.alphaNumStr.filter(_.nonEmpty)) { aStr: String =>
      Wildcard.matches(aStr) shouldBe true
    }
  }

  "AnyDigit pattern" should "match any digit" in {
    val intGen = Gen.numStr.filter(_.nonEmpty)
    forAll(intGen) { number: String =>
      AnyDigit.matches(number) shouldBe true
    }
  }

  "AnyNonDigit" should "match any non digit" in {
    forAll(Gen.alphaStr.filter(_.nonEmpty)) { nonDigit: String =>
      AnyNonDigit.matches(nonDigit) shouldBe true
    }
  }

  // TODO think about escaping chars in the string cases
  // or other cases that might fail
  "Only" should "match only the characters it has" in {
    val onlyAbc = Only("abc")

    forAll(abcGen) { abc: String =>
      onlyAbc.matches(abc) shouldBe true
    }
  }

  it should "not match other characters than specified" in {
    val onlyAbc = Only("abc")
    onlyAbc.matches("other") shouldBe false
  }

  "Not" should "not match characters other than it specifies" in {
    val notAbc = Not("abc")
    forAll(abcGen) { abc: String =>
      notAbc.matches(abc) shouldBe false
    }
  }

  it should "match other characters" in {
    val notAbc = Not("abc")
    val notAbcGen = Gen.alphaChar
      .filter(c => c != 'a' && c != 'b' && c != 'c').map(_.toString)
      .filter(_.nonEmpty)

    forAll(notAbcGen) { abcExcluded: String =>
      notAbc.matches(abcExcluded) shouldBe true
    }
  }

  "RangeMatch" should "match a range of characters" in {
    val rangeMatch = RangeMatch('a' -> 'z')

    forAll(Gen.alphaStr.map(_.trim).filter(_.nonEmpty)) { str: String =>
      rangeMatch.matches(str) shouldBe true
    }
  }
  // not matching test

  // multiple ranges test

  // range negative TODO refactor Not rule

  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }

  implicit class RuleMatchExtender(r: Rule) {
    def matches(s: String): Boolean = r.pattern.findFirstIn(s).nonEmpty
  }
}
