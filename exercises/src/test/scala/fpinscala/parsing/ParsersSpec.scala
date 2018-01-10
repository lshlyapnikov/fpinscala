package fpinscala.parsing

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.chooseNum
import org.scalacheck.Shrink
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ParsersSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val parser = MyParser.IteratingParser
  import parser._

  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int]       = Shrink.shrinkAny[Int]

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

  "or" in forAll(arbitrary[String].filter(_.nonEmpty), arbitrary[String].filter(_.nonEmpty)) {
    (a, b) =>
      parser.run(string(a) or string(b))(a) shouldBe Right(a)
      parser.run(string(a) or string(b))(b) shouldBe Right(b)
  }

  "or with one arg empty" in forAll(arbitrary[String].filter(_.nonEmpty)) { a =>
    parser.run(string("") or string(a))("") shouldBe Right("")
    parser.run(string("") or string(a))(a) shouldBe Right("")
    parser.run(string(a) or string(""))("") shouldBe Right("")
    parser.run(string(a) or string(""))(a) shouldBe Right(a)
  }

  "listOfN or " in {
    parser.run(listOfN(3, string("ab") | string("cad")))("ababcad") shouldBe Right(
      List("ab", "ab", "cad"))
    parser.run(listOfN(3, string("ab") | string("cad")))("cadabab") shouldBe Right(
      List("cad", "ab", "ab"))
    parser.run(listOfN(3, string("ab") | string("cad")))("ababab") shouldBe Right(
      List("ab", "ab", "ab"))
  }

  "forall a b. map2(point(a), point(b))((_, _)) = point((a, b))" in forAll(arbitrary[String],
                                                                           arbitrary[String],
                                                                           arbitrary[String]) {
    (a, b, c) =>
      val f = map2(point(a), point(b))((_, _))
      val g = point((a, b))

      val fc = parser.run(f)(c)
      val gc = parser.run(g)(c)

      fc shouldBe gc
  }

  "forall a fb. map2(point(a), fb)((x, y) => y) = fb" in forAll(arbitrary[String],
                                                                arbitrary[String],
                                                                arbitrary[String]) { (a, b, c) =>
    def fb = string(b)

    val g = map2(point(a), fb)((x, y) => y)

    parser.run(g)(c) shouldBe parser.run(fb)(c)
  }

  "forall fa b. map2(fa, point(b))((x, y) => x) = fa" in forAll(arbitrary[String],
                                                                arbitrary[String],
                                                                arbitrary[String]) { (a, b, c) =>
    def fa = string(a)

    val g = map2(fa, point(b))((x, y) => x)

    parser.run(g)(c) shouldBe parser.run(fa)(c)
  }
}
