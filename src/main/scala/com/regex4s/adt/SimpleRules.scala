package com.regex4s.adt

object SimpleRules {
  // TODO some of these case objects could be converted to vals
  case object Wildcard extends Repeatable {
    override def rawPattern: String = "."
  }

  case object Dot extends Repeatable {
    override def rawPattern: String = "\\."
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

  case object NonWhitespace extends Repeatable {
    override def rawPattern: String = "\\S"
  }

  case object AnyWordCharacter extends Repeatable {
    override def rawPattern: String = "\\w"
  }

  case object NonWordCharacter extends Repeatable {
    override def rawPattern: String = "\\W"
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

  case class ZeroOrMore(rule: Rule) extends Rule {
    override def rawPattern: String = s"${rule.rawPattern}*"
  }

  case class OnceOrMore(rule: Repeatable) extends Rule {
    override def rawPattern: String = s"${rule.rawPattern}+"
  }

  case class Optional(rule: Rule) extends Rule {
    override def rawPattern: String = s"${rule.rawPattern}?"
  }

  case class Start(rule: Rule) extends Rule {
    override def rawPattern: String = s"^${rule.rawPattern}"
  }

  case class End(rule: Rule) extends Rule {
    override def rawPattern: String = s"${rule.rawPattern}$$"
  }
}
