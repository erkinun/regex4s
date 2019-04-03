package com.regex4s.adt

// TODO think about compound regexes like phone numbers emails etc
// TODO In order to do this, we need to have a builder dsl to link things together
// TODO to make it easier maybe

case class Combined(rules: Rule*) extends Rule {
  override def rawPattern: String = rules.map(_.rawPattern).mkString("")
}
