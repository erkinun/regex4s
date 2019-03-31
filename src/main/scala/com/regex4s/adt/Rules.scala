package com.regex4s.adt

import scala.util.matching.Regex

sealed trait Rule {
  def rawPattern: String
  def pattern: Regex = rawPattern.r
}
sealed trait Repeatable extends Rule

case object Wildcard extends Repeatable {
  override def rawPattern: String = "."
}

case object AnyDigit extends Repeatable {
  override def rawPattern: String = "\\d"
}

case object AnyNonDigit extends Repeatable {
  override def rawPattern: String = "\\D"
}

case object Whitespace extends Repeatable {
  override def rawPattern: String = "\\s"
}

case class Only(these: String) extends Repeatable {
  override def rawPattern: String = s"[$these]"
}

case class Not(these: String) extends Repeatable {
  override def rawPattern: String = s"[^$these]"
}

case class RangeMatch(ranges: Range*) extends Repeatable {
  override def rawPattern: String = s"[$rangeText]"
  protected def rangeText = ranges.map(r => s"${r.start}-${r.end}").mkString("")
}

case class NegativeRange(ranges: Range*) extends Repeatable {
  override def rawPattern: String = s"[^$rangeText]"
  protected def rangeText = ranges.map(r => s"${r.start}-${r.end}").mkString("")
}
sealed trait Times
case class Exactly(times: Int) extends Times
case class Between(from: Int, to: Int) extends Times

case class Repeat(these: Repeatable, times: Times) extends Rule {
  override def rawPattern: String = {
    val repeatTimes = times match {
      case Exactly(t) => s"{$t}"
      case Between(f, t) => s"{$f,$t}"
    }
    s"${these.rawPattern}$repeatTimes"
  }
}

case class ZeroOrMore(rule: Repeatable) extends Rule {
  override def rawPattern: String = s"${rule.rawPattern}*"
}

case class OnceOrMore(rule: Repeatable) extends Rule {
  override def rawPattern: String = s"${rule.rawPattern}+"
}

case class Optional(rule: Repeatable) extends Rule {
  override def rawPattern: String = s"${rule.rawPattern}?"
}

case class Start(rule: Rule) extends Rule {
  override def rawPattern: String = s"^${rule.rawPattern}"
}

case class End(rule: Rule) extends Rule {
  override def rawPattern: String = s"${rule.rawPattern}$$"
}

// TODO maybe we can have abnormal range checks?
// like z -> 5, 2 -> a, b->a

// TODO think about compound regexes like phone numbers emails etc
// TODO In order to do this, we need to have a builder dsl to link things together

case class Range(start: Char, end: Char)
object Range {
  def apply(tp: (Char, Char)): Range = new Range(tp._1, tp._2)
}

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