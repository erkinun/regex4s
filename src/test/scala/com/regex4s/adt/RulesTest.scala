package com.regex4s.adt

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Wildcard pattern" should "match any arbitrary characters" in {
    forAll("strings") { aStr: String =>
      whenever(aStr.nonEmpty) {
        Wildcard.matches(aStr) shouldBe true
      }
    }
  }

  "AnyDigit pattern" should "match any digit" in {
    val intGen = Gen.numStr
    forAll(intGen) { number: String =>
      whenever(number.nonEmpty) {
        AnyDigit.matches(number) shouldBe true
      }
    }
  }

  "AnyNonDigit" should "match any non digit" in {
    forAll("non digits") { nonDigit: String =>
      whenever(nonDigit.nonEmpty && !nonDigit.matches(AnyDigit.pattern.toString())) {
        AnyNonDigit.matches(nonDigit) shouldBe true
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
        onlyAbc.matches(abc) shouldBe true
      }
    }
  }

  it should "not match other characters than specified" in {
    val onlyAbc = Only("abc")

    onlyAbc.matches("other") shouldBe false
  }

  "Not" should "not match characters other than it specifies" in {
    val notAbc = Not("abc")
    val abcGen = Gen.listOf(Gen.oneOf("a", "b", "c")).flatMap(_.mkString(""))

    forAll(abcGen) { abc: String =>
      whenever(abc.nonEmpty) {
        notAbc.matches(abc) shouldBe false
      }
    }
  }

  it should "match other characters" in {
    val notAbc = Not("abc")
    val notAbcGen = Gen.alphaChar.filter(c => c != 'a' && c != 'b' && c != 'c').map(_.toString)

    forAll(notAbcGen) { abcExcluded: String =>
      whenever(abcExcluded.nonEmpty) {
        notAbc.matches(abcExcluded) shouldBe true
      }
    }
  }

  // TODO find a way to get rid of whenever check
  "RangeMatch" should "match a range of characters" in {
    val rangeMatch = RangeMatch(Range('a', 'z'))

    forAll(Gen.alphaStr.map(_.trim)) { str: String =>
      whenever(str.nonEmpty) {
        rangeMatch.matches(str) shouldBe true
      }
    }
  }
  // not matching test

  // multiple ranges test

  // range negative TODO refactor Not rule

  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }

  // TODO maybe this can return a bool?
  implicit class RuleMatchExtender(r: Rule) {
    def matches(s: String): Boolean = r.pattern.findFirstIn(s).nonEmpty
  }
}
