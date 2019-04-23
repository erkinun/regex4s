package com.regex4s.adt

import com.regex4s.adt.SimpleRules._

class GroupSpec extends RegexSpec {

  "Group" should "create a group to match a portion of the text" in {
    val group = Group(
      Combined(
        Sequence("img"),
        ZeroOrMore(AnyDigit),
        Sequence("\\."),
        Sequence("png")))

    val expected = "(img\\d*\\.png)"
    group.rawPattern shouldBe expected
  }

  // TODO test for actual matching on group, extracting info
}
