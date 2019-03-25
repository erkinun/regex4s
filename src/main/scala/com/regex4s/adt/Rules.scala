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

case class RangeMatch(ranges: Range*) extends Rule {
  override def pattern: Regex = s"[$rangeText]".r
  protected def rangeText = ranges.map(r => s"${r.start}-${r.end}").mkString("")
}

case class NegativeRange(ranges: Range*) extends Rule {
  override def pattern: Regex = s"[^$rangeText]".r
  protected def rangeText = ranges.map(r => s"${r.start}-${r.end}").mkString("")
}

case class Repetition(these: String, times: Int) extends Rule {
  override def pattern: Regex = s"[$these]{$times}".r
}

// TODO maybe we can have abnormal range checks?
// like z -> 5, 2 -> a, b->a

case class Range(start: Char, end: Char)

object RangeMatch {
  // DummyImplicit is to prevent same types after type erasure
  def apply(tps: (Char, Char)*)(implicit d: DummyImplicit): RangeMatch = {
    val ranges = tps.map(tp => Range(tp))
    RangeMatch(ranges:_*)
  }
}

object NegativeRange {
  // DummyImplicit is to prevent same types after type erasure
  def apply(tps: (Char, Char)*)(implicit d: DummyImplicit): NegativeRange = {
    val ranges = tps.map(tp => Range(tp))
    NegativeRange(ranges:_*)
  }
}

object Range {
  def apply(tp: (Char, Char)): Range = new Range(tp._1, tp._2)
}