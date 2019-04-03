package com.regex4s.adt

import scala.util.matching.Regex

trait Rule {
  def rawPattern: String
  def pattern: Regex = rawPattern.r
}

// TODO can we provide a difference operator if the match is not a success?
