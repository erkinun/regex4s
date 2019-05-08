package com.regex4s.adt

import scala.util.matching.{Regex, UnanchoredRegex}

trait Rule {
  def rawPattern: String
  def pattern: Regex = rawPattern.r
  def entire: UnanchoredRegex = pattern.unanchored
}