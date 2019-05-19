package com.regex4s.adt

object SimpleRules {
  // TODO some of these case objects could be converted to vals
  case object Wildcard extends Rule {
    override def rawPattern: String = "."
  }

  case object Dot extends Rule {
    override def rawPattern: String = "\\."
  }

  case object AnyDigit extends Rule {
    override def rawPattern: String = "\\d"
  }

  case object AnyNonDigit extends Rule {
    override def rawPattern: String = "\\D"
  }

  case object Whitespace extends Rule {
    override def rawPattern: String = "\\s"
  }

  case object NonWhitespace extends Rule {
    override def rawPattern: String = "\\S"
  }

  case object AnyWordCharacter extends Rule {
    override def rawPattern: String = "\\w"
  }

  case object NonWordCharacter extends Rule {
    override def rawPattern: String = "\\W"
  }

  // TODO create a factory for accepting rule, or just change it to rule
  case class Only(these: String) extends Rule {
    override def rawPattern: String = s"[$these]"
  }

  case class Not(these: String) extends Rule {
    override def rawPattern: String = s"[^$these]"
  }

  case class Sequence(these: String) extends Rule {
    override def rawPattern: String = these
  }

  case class ZeroOrMore(rule: Rule) extends Rule {
    override def rawPattern: String = s"${rule.rawPattern}*"
  }

  case class OnceOrMore(rule: Rule) extends Rule {
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
