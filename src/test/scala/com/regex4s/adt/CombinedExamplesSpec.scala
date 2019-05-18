package com.regex4s.adt

import com.regex4s.adt.Combined.{Decimals, File, SimpleEmail}
import com.regex4s.adt.Implicits._
import com.regex4s.adt.SimpleRules.{AnyDigit, Sequence, ZeroOrMore}
import org.scalatest.{Matchers, WordSpec}

class CombinedExamplesSpec extends WordSpec with Matchers {

  "Examples" when {
    "File" should {
      val imageFileR = File(
        prefix = Combined(
          Sequence("img"),
          ZeroOrMore(AnyDigit)
        ),
        fileType = Sequence("png"))

      "produce the expected raw regex string" in {
        val expected = "(img\\d*\\.png)"
        imageFileR.rawPattern shouldBe expected
      }

      "match the appropriate text in a given string" in {
        val sample = "here the file name is buried img35.png in the text"
        val fetcher = imageFileR.entire

        sample match {
          case fetcher(name) => name shouldBe "img35.png"
          case _ => throw new Exception("pattern should have matched the text")
        }
      }
    }

    "SimpleEmail" should {
      val email = SimpleEmail
      "produce the expected raw regex string" in {
        val expected = """(\S+@\w+\.(com|net))"""
        email.rawPattern shouldBe expected
      }

      "match the appropriate text in a given string" in {
        val sample = "here is an email: foobar@hotmail.com"
        val fetcher = email.entire

        sample match {
          case fetcher(foundEmail, _) => foundEmail shouldBe "foobar@hotmail.com"
          case _ => throw new Exception("pattern should have match the mail in text")
        }
      }
    }

    "Decimals" should {
      val decimal = Decimals

      "produce the expected raw regex string" in {
        val expected = """(-?\d+(,\d+)*\.?\de?\d*)"""
        decimal.rawPattern shouldBe expected
      }

      "match the different numbers" in {
        decimal.matches("3.14529") shouldBe true
        decimal.matches("-255.34") shouldBe true
        decimal.matches("128") shouldBe true
        decimal.matches("1.9e10") shouldBe true
        decimal.matches("123,340.00") shouldBe true
      }
    }
  }
}
