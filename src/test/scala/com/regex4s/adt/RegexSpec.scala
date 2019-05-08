package com.regex4s.adt

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait RegexSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks

// TODO can we provide a difference operator if the match is not a success?
