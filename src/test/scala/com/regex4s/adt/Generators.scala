package com.regex4s.adt

import org.scalacheck.Gen

object Generators {
  val abcGen: Gen[String] = Gen.listOf(Gen.oneOf("a", "b", "c"))
    .flatMap(_.mkString(""))
    .filter(_.nonEmpty)

  val notAbcChar: Gen[Char] = Gen.alphaChar
    .filter(c => c != 'a' && c != 'b' && c != 'c')
  val notAbc: Gen[String] = Gen.listOfN(3, notAbcChar).flatMap(_.mkString(""))
}
