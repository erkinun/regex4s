package com.regex4s.adt

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val abcGen = Gen.listOf(Gen.oneOf("a", "b", "c"))
    .flatMap(_.mkString(""))
    .filter(_.nonEmpty)

  private val notAbcChar: Gen[Char] = Gen.alphaChar
    .filter(c => c != 'a' && c != 'b' && c != 'c')
  private val notAbc = Gen.listOfN(3, notAbcChar).flatMap(_.mkString(""))

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
    val notAbcGen = notAbcChar.map(_.toString)
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

  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }

  implicit class RuleMatchExtender(r: Rule) {
    def matches(s: String): Boolean = r.pattern.findFirstIn(s).nonEmpty
  }
}
