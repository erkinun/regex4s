package com.regex4s.adt

import com.regex4s.adt.Implicits._
import com.regex4s.adt.Generators._
import com.regex4s.adt.Ranges.{NegativeRange, RangeMatch}
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RangesSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "RangeMatch" should "match a range of characters" in {
    val rangeMatch = RangeMatch('a' -> 'z')

    forAll(Gen.alphaStr.map(_.trim).filter(_.nonEmpty)) { str: String =>
      rangeMatch.matches(str) shouldBe true
    }
  }

  it should "not match ranges outside it represent" in {
    val rangeMatch = RangeMatch('d' -> 'z')

    forAll(abcGen) { abc =>
      rangeMatch.matches(abc) shouldBe false
    }
  }

  it should "match numeric ranges" in {
    val rangeMatch = RangeMatch('0' -> '9')
    forAll(Gen.numStr.filter(_.nonEmpty)) { numStr =>
      rangeMatch.matches(numStr) shouldBe true
    }
  }

  it should "match multiple ranges" in {
    val range = RangeMatch('a' -> 'c', '0' -> '3')
    val digitGen = Gen.oneOf("0", "1", "2", "3")
    val gen = Gen.pick(2, abcGen, digitGen).flatMap(_.mkString(""))

    forAll(gen) { multi =>
      range.matches(multi) shouldBe true
    }
  }

  // negative range TODO refactor Not rule
  "NegativeRange" should "match anything except it specifies as range" in {
    val negRange = NegativeRange('d' -> 'z')

    forAll(abcGen) { abc =>
      negRange.matches(abc) shouldBe true
    }
  }

  it should "not match a pattern it specifies" in {
    val negRange = NegativeRange('a' -> 'c')

    forAll(abcGen) { abc =>
      negRange.matches(abc) shouldBe false
    }
  }

  it should "not match over multiple ranges" in {
    val multiNegRange = NegativeRange('a' -> 'c', '0' -> '9')
    val abcOrNum = Gen.oneOf(abcGen, Gen.numStr)

    forAll(abcOrNum) { s =>
      multiNegRange.matches(s) shouldBe false
    }
  }
}
