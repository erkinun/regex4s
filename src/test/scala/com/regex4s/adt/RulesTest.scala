package com.regex4s.adt

import com.regex4s.adt.Implicits._
import com.regex4s.adt.Generators._
import com.regex4s.adt.SimpleRules._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

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

  "Repetition" should "match the length of the specified pattern" in {
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

  // TODO do we have to test with each repeatable in these cases?
  "KleeneStar" should "match the pattern zero or more times" in {
    val zeroOrMore = ZeroOrMore(Only("abc"))
    forAll(abcGen) { s => zeroOrMore.matches(s) shouldBe true }
  }

  "KleenePlus" should "match the pattern at least once or more" in {
    val onceOrMore = OnceOrMore(Only("abc"))
    forAll(abcGen) { s => onceOrMore.matches(s) shouldBe true }
  }

  it should "not match if the pattern does not match at all" in {
    val onceOrMore = OnceOrMore(Only("abc"))
    forAll(notAbc) { s => onceOrMore.matches(s) shouldBe false }
  }

  "Optional" should "match the pattern" in {
    val optional = Optional(Only("abc"))
    forAll(abcGen) { s => optional.matches(s) shouldBe true }
  }

  it should "match even if the pattern is missing" in {
    val optional = Optional(Only("abc"))
    forAll(notAbc) { s => optional.matches(s) shouldBe true }
  }

  "Start" should "match at the start of a string" in {
    val startingTripleAs = Start(Repeat(Only("a"), Exactly(3)))
    val sentence = "aaa! what are you doing?"
    startingTripleAs.matches(sentence) shouldBe true
  }

  it should "not match if the pattern is not at the start" in {
    val startingTripleAs = Start(Repeat(Only("a"), Exactly(3)))
    val sentence = "what are you doing? aaa!"
    startingTripleAs.matches(sentence) shouldBe false
  }

  "End" should "match only at the end of the string" in {
    val endScreaming = End(Repeat(Only("a"), Between(2, 4)))
    val sentence = "what are you doing? !aaa"
    endScreaming.matches(sentence) shouldBe true
  }

  it should "not match if the pattern is not at the end" in {
    val endScreaming = End(Repeat(Only("a"), Between(2, 4)))
    val sentence = "what are you doing? aaa!"
    endScreaming.matches(sentence) shouldBe false
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
