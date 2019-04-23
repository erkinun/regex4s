package com.regex4s.adt

case class Group(rule: Rule) extends Rule {
  override def rawPattern: String = s"(${rule.rawPattern})"
}
