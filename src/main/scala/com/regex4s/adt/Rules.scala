package com.regex4s.adt

import scala.util.matching.Regex

sealed trait Rule {
  def pattern: Regex
}
case object AnySingle extends Rule {
  override def pattern: Regex = ".".r
}

case object AnyDigit extends Rule {
  override def pattern: Regex = "\\d".r
}
