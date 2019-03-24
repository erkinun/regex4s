package com.regex4s.adt

import scala.util.matching.Regex

sealed trait Rule {
  def pattern: Regex
}
case object Wildcard extends Rule {
  override def pattern: Regex = ".".r
}

case object AnyDigit extends Rule {
  override def pattern: Regex = "\\d".r
}

case object AnyNonDigit extends Rule {
  override def pattern: Regex = "\\D".r
}

case class Only(these: String) extends Rule {
  override def pattern: Regex = s"[$these]".r
}

case class Not(these: String) extends Rule {
  override def pattern: Regex = s"[^$these]".r
}

// TODO implement implicit conversion from Tuple
case class Range(start: Character, end: Character)

case class RangeMatch(range: Range) extends Rule {
  override def pattern: Regex = s"[${range.start}-${range.end}]".r
}