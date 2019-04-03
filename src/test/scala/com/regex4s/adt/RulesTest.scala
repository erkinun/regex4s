package com.regex4s.adt

import com.regex4s.adt.Implicits._
import com.regex4s.adt.Generators._
import com.regex4s.adt.Ranges.{NegativeRange, RangeMatch}
import com.regex4s.adt.SimpleRules._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Repeat" should "match the length of the specified pattern" in {
    val repeat = Repeat(Only("abc"), Exactly(5))
    val abcFiveLength = abcGen.filter(_.length > 5).map(_.take(5))

    forAll(abcFiveLength) { s => repeat.matches(s) shouldBe true }
  }

  it should "match n digits in a string" in {
    val repeat = Repeat(AnyDigit, Exactly(3))
    val threeDigits = Gen.numStr.filter(_.nonEmpty).filter(_.length > 3)

    forAll(threeDigits) { d => repeat.matches(d) shouldBe true }
  }

  it should "match a range of characters multiple times" in {
    val abcThree = abcGen.filter(_.length > 3).map(_.take(3))
    val repeat = Repeat(RangeMatch('a' -> 'c'), Exactly(3))

    forAll(abcThree) { s => repeat.matches(s) shouldBe true }
  }

  it should "match wildcard n times" in {
    val repeat = Repeat(Wildcard, Exactly(3))
    forAll(Gen.alphaStr.filter(_.length > 3)) { s => repeat.matches(s) shouldBe true }
  }

  it should "match non digits n times" in {
    val repeat = Repeat(AnyNonDigit, Exactly(3))
    forAll(Gen.alphaStr.filter(_.length > 3)) { s => repeat.matches(s) shouldBe true }
  }

  it should "match not pattern n times" in {
    val repeat = Repeat(Not("abc"), Exactly(3))
    forAll(notAbc) { s => repeat.matches(s) shouldBe true }
  }

  it should "match negative range n times" in {
    val repeat = Repeat(NegativeRange('a' -> 'c'), Exactly(3))
    forAll(notAbc) { s => repeat.matches(s) shouldBe true }
  }

  it should "match n any non digits" in {
    val repeat = Repeat(AnyNonDigit, Exactly(3))
    val threeDigits = Gen.alphaStr.filter(_.nonEmpty).filter(_.length > 3)

    forAll(threeDigits) { d => repeat.matches(d) shouldBe true }
  }

  it should "match between 2 and 4 times if specified" in {
    val repeat = Repeat(Only("z"), Between(2, 4))
    val wazzzup = "wazzzup"

    repeat.matches(wazzzup) shouldBe true
  }

  it should "not match if requested pattern is found fewer times than requested" in {
    val repeat = Repeat(Only("z"), Between(2, 4))
    val wazup = "wazup"

    repeat.matches(wazup) shouldBe false
  }



  "Combined" should "create a combination of the rules it is supplied" in {
    val combined = Combined(
                    Start(Sequence("img")),
                    ZeroOrMore(AnyDigit),
                    Sequence("\\."),
                    End(Sequence("png")))
    val expected = "^img\\d*\\.png$"
    combined.rawPattern shouldBe expected
  }

  it should "match a complex string" in {
    val combined = Combined(
                    Start(Sequence("img")),
                    ZeroOrMore(AnyDigit),
                    Sequence("\\."),
                    End(Sequence("png")))

    combined.matches("img3456.png") shouldBe true
  }
}
