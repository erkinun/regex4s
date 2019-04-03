package com.regex4s.adt

import com.regex4s.adt.Implicits._
import com.regex4s.adt.SimpleRules._

class CombinedSpec extends RegexSpec {

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
