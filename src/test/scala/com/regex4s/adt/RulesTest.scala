package com.regex4s.adt

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RulesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "AnySingle rule" should "match any arbitrary characters" in {
    val r = AnySingle.pattern

    forAll("a") { aStr: String =>
      whenever(aStr.nonEmpty) {
        r.findFirstIn(aStr) shouldBe Some(aStr.head.toString)
      }
    }
  }
}
