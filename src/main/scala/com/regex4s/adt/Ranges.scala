package com.regex4s.adt

object Ranges {
  case class RangeMatch(ranges: Range*) extends Repeatable {
    override def rawPattern: String = s"[$rangeText]"
    protected def rangeText: String = ranges.map(r => s"${r.start}-${r.end}").mkString("")
  }

  case class NegativeRange(ranges: Range*) extends Repeatable {
    override def rawPattern: String = s"[^$rangeText]"
    protected def rangeText: String = ranges.map(r => s"${r.start}-${r.end}").mkString("")
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

  case class Range(start: Char, end: Char)
  object Range {
    def apply(tp: (Char, Char)): Range = new Range(tp._1, tp._2)
  }
}

