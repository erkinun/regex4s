package com.regex4s.adt

import com.regex4s.adt.SimpleRules.Not

// TODO maybe we can have abnormal range checks?
// like z -> 5, 2 -> a, b->a

object Ranges {
  case class RangeMatch(ranges: Range*) extends Rule {
    override def rawPattern: String = s"[${RangeMatch.rangeText(ranges)}]"
  }

  case class RangeExclude(ranges: Range*) extends Rule {
    override def rawPattern: String = Not(RangeMatch.rangeText(ranges)).rawPattern
  }

  object RangeMatch {
    // DummyImplicit is to prevent same types after type erasure
    def apply(tps: (Char, Char)*)(implicit d: DummyImplicit): RangeMatch = {
      val ranges = tps.map(tp => Range(tp))
      RangeMatch(ranges:_*)
    }

    def rangeText(ranges: Seq[Range]): String = ranges.map(r => s"${r.start}-${r.end}").mkString("")
  }

  object RangeExclude {
    // DummyImplicit is to prevent same types after type erasure
    def apply(tps: (Char, Char)*)(implicit d: DummyImplicit): RangeExclude = {
      val ranges = tps.map(tp => Range(tp))
      RangeExclude(ranges:_*)
    }
  }

  case class Range(start: Char, end: Char)
  object Range {
    def apply(tp: (Char, Char)): Range = new Range(tp._1, tp._2)
  }
}

