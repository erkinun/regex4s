package com.regex4s.adt

import com.regex4s.adt.Or.Pipe
import com.regex4s.adt.SimpleRules.Sequence

case class Or(rules: Rule*) extends Rule {
  val h: Seq[Rule] = rules.flatMap(r => List(r, Pipe)).init
  val rule = Group(Combined(h:_*))
  override def rawPattern: String = rule.rawPattern
}

object Or {
  val Pipe = Sequence("|")
}
