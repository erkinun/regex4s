package com.regex4s.adt

import scala.util.matching.Regex

trait Rule {
  def rawPattern: String
  def pattern: Regex = rawPattern.r
}
trait Repeatable extends Rule

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

// TODO maybe we can have abnormal range checks?
// like z -> 5, 2 -> a, b->a

// TODO think about compound regexes like phone numbers emails etc
// TODO In order to do this, we need to have a builder dsl to link things together
// TODO to make it easier maybe
case class Combined(rules: Rule*) extends Rule {
  override def rawPattern: String = rules.map(_.rawPattern).mkString("")
}

// TODO can we provide a difference operator if the match is not a success?
