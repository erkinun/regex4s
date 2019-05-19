package com.regex4s.adt

import scala.util.matching.{Regex, UnanchoredRegex}

// TODO publish to maven
// TODO detailed read me for tutorial
// TODO can we create postfix regex
// such as [\s\w]* shouldBe
//              (AnyWordCharacter,
//              Whitespace) . ZeroOrMore
trait Rule {
  def rawPattern: String
  def pattern: Regex = rawPattern.r
  def entire: UnanchoredRegex = pattern.unanchored
}