package com.regex4s.adt

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
