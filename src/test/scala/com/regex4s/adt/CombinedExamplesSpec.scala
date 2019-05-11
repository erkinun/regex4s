package com.regex4s.adt

import com.regex4s.adt.Combined.ImageFile
import com.regex4s.adt.SimpleRules.{AnyDigit, Sequence, ZeroOrMore}
import org.scalatest.{Matchers, WordSpec}

class CombinedExamplesSpec extends WordSpec with Matchers {

  "Examples" when {
    "ImageFile" should {

      val imageFileR = ImageFile(
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
  }
}
