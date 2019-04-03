package com.regex4s.adt

object Implicits {
  implicit class StringFirstChar(s: String) {
    def firstChar: String = s.head.toString
  }

  implicit class RuleMatchExtender(r: Rule) {
    def matches(s: String): Boolean = r.pattern.findFirstIn(s).nonEmpty
  }
}
