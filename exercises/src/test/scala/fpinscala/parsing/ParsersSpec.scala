package fpinscala.parsing

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Shrink}
import org.scalacheck.Gen.{chooseNum, posNum}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ParsersSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val parser = MyParser.IteratingParser
  import parser._

  private implicit val noStringShrink = Shrink.shrinkAny[String]
  private implicit val noIntShrink    = Shrink.shrinkAny[Int]

  "exact string" in forAll(arbitrary[String]) { a =>
    parser.run(string(a))(a) shouldBe Right(a)
  }

  "sub-string" in forAll(arbitrary[String], arbitrary[String]) { (a, b) =>
    parser.run(string(a))(a + b) shouldBe Right(a)
  }

  "exact char" in forAll(arbitrary[Char]) { a =>
    parser.run(char(a))(a.toString) shouldBe Right(a)
  }

  "sub-char" in forAll(arbitrary[String].filter(_.nonEmpty)) { a =>
    val c = a.charAt(0)
    parser.run(char(c))(a) shouldBe Right(c)
  }

  "listOfN(3, abc)" in {
    parser.run(listOfN(3, string("abc")))("abcabcabc") shouldBe Right(List("abc", "abc", "abc"))
  }

  "listOfN(1, abc)" in {
    parser.run(listOfN(1, string("abc")))("abcabcabc") shouldBe Right(List("abc"))
  }

  "listOfN(0, abc)" in {
    parser.run(listOfN(0, string("abc")))("abcabcabc") shouldBe Right(List())
  }

  "listOfN" in forAll(arbitrary[String].filter(_.nonEmpty), chooseNum[Int](1, 10)) { (a, n) =>
    parser.run(listOfN(n, string(a)))(a * n) shouldBe Right(List.fill(n)(a))
  }
}
