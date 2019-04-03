package com.regex4s.adt

import com.regex4s.adt.SimpleRules._
import com.regex4s.adt.Generators.{abcGen, notAbc, notAbcChar}
import com.regex4s.adt.Implicits._
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SimpleRulesSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks  {

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

  "Whitespace" should "match any kind of space, tab, newline" in {
    val whitespaceGen = Gen.oneOf('\n', '\t', ' ', '\r').map(_.toString)
    forAll(whitespaceGen) { s => Whitespace.matches(s) shouldBe true }
  }

  it should "not match if there are no whitespace in a test" in {
    forAll(Gen.alphaStr) { s => Whitespace.matches(s) shouldBe false }
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

  "Sequence" should "match only the string it has" in {
    val seq = Sequence("abc")
    seq.matches("abc and some other stuff") shouldBe true
  }

  it should "not match sequences which does not have the pattern" in {
    val seq = Sequence("abc")
    seq.matches("def and some other stuff") shouldBe false
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
}
