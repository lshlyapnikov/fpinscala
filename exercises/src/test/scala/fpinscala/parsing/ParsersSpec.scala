package fpinscala.parsing

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ParsersSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val parser = MyParser.IteratingParser

  import parser._

  private implicit val noCharShrink: Shrink[Char] = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int] = Shrink.shrinkAny[Int]

  "exact string" in forAll(nonEmptyString) { a =>
    parser.run(string(a))(a) shouldBe Right(a)
  }

  "sub-string" in forAll(nonEmptyString, nonEmptyString) { (a, b) =>
    parser.run(string(a))(a + b) shouldBe Right(a)
  }

  "exact char" in forAll(arbitrary[Char]) { a =>
    parser.run(char(a))(a.toString) shouldBe Right(a)
  }

  "sub-char" in forAll(nonEmptyString) { a =>
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

  "listOfN" in forAll(nonEmptyString, chooseNum[Int](1, 10)) { (a, n) =>
    parser.run(listOfN(n, string(a)))(a * n) shouldBe Right(List.fill(n)(a))
  }

  "or" in forAll(nonEmptyString, nonEmptyString) { (a, b) =>
    parser.run(string(a) or string(b))(a) shouldBe Right(a)
    parser.run(string(a) or string(b))(b) shouldBe Right(b)
  }

  "or with one arg empty" in forAll(nonEmptyString) { a =>
    parser.run(string("") or string(a))("") shouldBe Left(Location("").toError(a))
    parser.run(string("") or string(a))(a) shouldBe Right(a)
    parser.run(string(a) or string(""))("") shouldBe Left(Location("").toError(""))
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

  "forall a b. map2(point(a), point(b))((_, _)) = point((a, b))" in forAll(nonEmptyString,
    nonEmptyString,
    nonEmptyString) {
    (a, b, c) =>
      val f = map2(point(a), point(b))((_, _))
      val g = point((a, b))

      val fc = parser.run(f)(c)
      val gc = parser.run(g)(c)

      fc shouldBe gc
  }

  "forall a fb. map2(point(a), fb)((x, y) => y) = fb" in forAll(nonEmptyString,
    nonEmptyString,
    nonEmptyString) { (a, b, c) =>
    def fb = string(b)

    val g = map2(point(a), fb)((x, y) => y)

    parser.run(g)(c) shouldBe parser.run(fb)(c)
  }

  "forall fa b. map2(fa, point(b))((x, y) => x) = fa" in forAll(nonEmptyString,
    nonEmptyString,
    nonEmptyString) { (a, b, c) =>
    def fa = string(a)

    val g = map2(fa, point(b))((x, y) => x)

    parser.run(g)(c) shouldBe parser.run(fa)(c)
  }

  "many" in forAll(nonEmptyString, chooseNum(1, 10)) { (s, n) =>
    val input = s * n
    parser.run(many(string(s)))(input) shouldBe Right(List.fill(n)(s))
  }

  "count char" in forAll(arbitrary[Char], chooseNum[Int](1, 10)) { (c, n) =>
    val str: String = List.fill(n)(c).mkString
    parser.run(count(c))(str) shouldBe Right(n)
  }

  "count string" in forAll(nonEmptyString.filter(_.nonEmpty), chooseNum[Int](1, 10)) { (s, n) =>
    val str: String = s * n
    parser.run(count(s))(str) shouldBe Right(n)
  }

  "count 0" in forAll(arbitrary[Char], nonEmptyString) { (c, str) =>
    whenever(str.isEmpty || str.charAt(0) != c) {
      parser.run(count(c))(str) shouldBe Right(0)
    }
  }

  "product" in forAll(nonEmptyString, nonEmptyString) { (a, b) =>
    parser.run(product(string(a), string(b)))(a + b) shouldBe Right((a, b))
  }

  // forall a b. product(point(a), point(b)) == point((a, b))
  "product law" in forAll(nonEmptyString, nonEmptyString, nonEmptyString) { (a, b, c) =>
    parser.run(product(point(a), point(b)))(c) shouldBe
      parser.run(point((a, b)))(c)
  }

  "product with count" in forAll(nonEmptyString, nonEmptyString, chooseNum[Int](1, 10)) {
    (a, b, n) =>
      val str = a + b * n
      parser.run(product(string(a), count(b)))(str) shouldBe Right((a, n))
  }

  "forall xs. traverse(xs)(point) = point(xs)" in forAll(Gen.listOf(nonEmptyString), nonEmptyString) { (xs, input) =>
    parser.run(traverse(xs)(point))(input) shouldBe parser.run(point(xs))(input)
  }

  def nonEmptyString = arbitrary[String].filter(_.nonEmpty)
}
