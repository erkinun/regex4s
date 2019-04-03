package com.regex4s.adt

object SimpleRules {
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

  case class Sequence(these: String) extends Rule {
    override def rawPattern: String = these
  }
}
