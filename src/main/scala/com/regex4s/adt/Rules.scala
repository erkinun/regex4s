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
