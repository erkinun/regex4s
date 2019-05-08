package com.regex4s.adt

import com.regex4s.adt.SimpleRules._

class GroupSpec extends RegexSpec {

  val group = Group(
    Combined(
      Sequence("img"),
      ZeroOrMore(AnyDigit),
      Sequence("\\."),
      Sequence("png")))

  "Group" should "create a group to match a portion of the text" in {
    val expected = """(img\d*\.png)"""
    group.rawPattern shouldBe expected
  }

  it should "extract the information out of the text if available" in {
    val sample = "here the file name is buried img35.png in the text"
    val file = group.entire

    sample match {
      case file(name) => name shouldBe "img35.png"
      case _ =>
        throw new Exception("pattern should have matched the text")
    }
  }
}
