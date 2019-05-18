package com.regex4s.adt

import com.regex4s.adt.SimpleRules._

// TODO think about compound regexes like phone numbers emails etc
// TODO In order to do this, we need to have a builder dsl to link things together
// TODO to make it easier maybe

case class Combined(rules: Rule*) extends Rule {
  override def rawPattern: String = rules.map(_.rawPattern).mkString("")
}

object Combined {
  case class File(prefix: Rule, fileType: Rule) extends Rule {
    val rule = Group(Combined(
      prefix,
      Sequence("\\."),
      fileType
    ))
    override def rawPattern: String = rule.rawPattern
  }

  case object SimpleEmail extends Rule {
    val rule = Group(Combined(
      OnceOrMore(NonWhitespace),
      Sequence("@"),
      OnceOrMore(AnyWordCharacter),
      Dot,
      Or(Sequence("com"), Sequence("net"))
    ))
    override def rawPattern: String = rule.rawPattern
  }

  case object Decimals extends Rule {
    val rule = Group(Combined(
      Optional(Sequence("-")),
      OnceOrMore(AnyDigit),
      ZeroOrMore(Group(Combined(
        Sequence(","),
        OnceOrMore(AnyDigit)
      ))),
      Optional(Dot),
      AnyDigit,
      Optional(Sequence("e")),
      ZeroOrMore(AnyDigit)
    ))

    override def rawPattern: String = rule.rawPattern
  }
}