package fpinscala.parsing

import fpinscala.parsing.MyParser.Parser
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{chooseNum, posNum}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import Generators._

class ParsersSpec
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import p._

  private implicit val noCharShrink: Shrink[Char] = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int] = Shrink.shrinkAny[Int]

  "exact string" in forAll(nonEmptyStringGen) { a =>
    p.run(string(a))(a) shouldBe Right(a)
  }

  "sub-string" in forAll(nonEmptyStringGen, nonEmptyStringGen) { (a, b) =>
    p.run(string(a))(a + b) shouldBe Right(a)
  }

  "exact char" in forAll(arbitrary[Char]) { a =>
    p.run(char(a))(a.toString) shouldBe Right(a)
  }

  "sub-char" in forAll(nonEmptyStringGen) { a =>
    val c = a.charAt(0)
    p.run(char(c))(a) shouldBe Right(c)
  }

  "listOfN(3, abc)" in {
    p.run(listOfN(3, string("abc")))("abcabcabc") shouldBe Right(
      List("abc", "abc", "abc"))
  }

  "listOfN(1, abc)" in {
    p.run(listOfN(1, string("abc")))("abcabcabc") shouldBe Right(List("abc"))
  }

  "listOfN(0, abc)" in {
    p.run(listOfN(0, string("abc")))("abcabcabc") shouldBe Right(List())
  }

  "listOfN" in forAll(nonEmptyStringGen, chooseNum[Int](1, 10)) { (a, n) =>
    p.run(listOfN(n, string(a)))(a * n) shouldBe Right(List.fill(n)(a))
  }

  "or" in forAll(nonEmptyStringGen, nonEmptyStringGen) { (a, b) =>
    p.run(string(a) or string(b))(a) shouldBe Right(a)
    p.run(string(a) or string(b))(b) shouldBe Right(b)
  }

  "or with one empty arg" in forAll(nonEmptyStringGen) { a =>
    p.run2(string("") or string(a))("") shouldBe Right((Location(""), ""))
    p.run2(string("") or string(a))(a) shouldBe Right((Location(a), ""))
    p.run2(string(a) or string(""))("") shouldBe Right((Location(""), ""))
    p.run2(string(a) or string(""))(a) shouldBe Right(
      (Location(a, a.length), a))
  }

  "listOfN or " in {
    p.run(listOfN(3, string("ab") | string("cad")))("ababcad") shouldBe Right(
      List("ab", "ab", "cad"))
    p.run(listOfN(3, string("ab") | string("cad")))("cadabab") shouldBe Right(
      List("cad", "ab", "ab"))
    p.run(listOfN(3, string("ab") | string("cad")))("ababab") shouldBe Right(
      List("ab", "ab", "ab"))
  }

  "forall a b. map2(point(a), point(b))((_, _)) = point((a, b))" in forAll(
    nonEmptyStringGen,
    nonEmptyStringGen,
    nonEmptyStringGen) { (a, b, c) =>
    val f = map2(point(a), point(b))((_, _))
    val g = point((a, b))

    val fc = p.run(f)(c)
    val gc = p.run(g)(c)

    fc shouldBe gc
  }

  "forall a fb. map2(point(a), fb)((x, y) => y) = fb" in forAll(
    nonEmptyStringGen,
    nonEmptyStringGen,
    nonEmptyStringGen) { (a, b, c) =>
    def fb = string(b)

    val g = map2(point(a), fb)((_, y) => y)

    p.run(g)(c) shouldBe p.run(fb)(c)
  }

  "forall fa b. map2(fa, point(b))((x, y) => x) = fa" in forAll(
    nonEmptyStringGen,
    nonEmptyStringGen,
    nonEmptyStringGen) { (a, b, c) =>
    def fa = string(a)

    val g = map2(fa, point(b))((x, y) => x)

    p.run(g)(c) shouldBe p.run(fa)(c)
  }

  "many" in forAll(nonEmptyStringGen, chooseNum(1, 10)) { (s, n) =>
    val input = s * n
    p.run(many(string(s)))(input) shouldBe Right(List.fill(n)(s))
  }

  "count char" in forAll(arbitrary[Char], chooseNum[Int](1, 10)) { (c, n) =>
    val str: String = List.fill(n)(c).mkString
    p.run(count(c))(str) shouldBe Right(n)
  }

  "count string" in forAll(nonEmptyStringGen.filter(_.nonEmpty),
                           chooseNum[Int](1, 10)) { (s, n) =>
    val str: String = s * n
    p.run(count(s))(str) shouldBe Right(n)
  }

  "count 0" in forAll(arbitrary[Char], nonEmptyStringGen) { (c, str) =>
    whenever(str.isEmpty || str.charAt(0) != c) {
      p.run(count(c))(str) shouldBe Right(0)
    }
  }

  "product" in forAll(nonEmptyStringGen, nonEmptyStringGen) { (a, b) =>
    p.run(product(string(a), string(b)))(a + b) shouldBe Right((a, b))
  }

  "forall a b. product(point(a), point(b)) == point((a, b))" in forAll(
    nonEmptyStringGen,
    nonEmptyStringGen,
    nonEmptyStringGen) { (a, b, c) =>
    p.run(product(point(a), point(b)))(c) shouldBe
      p.run(point((a, b)))(c)
  }

  "product with count" in forAll(nonEmptyStringGen,
                                 nonEmptyStringGen,
                                 chooseNum[Int](1, 10)) { (a, b, n) =>
    val str = a + b * n
    p.run(product(string(a), count(b)))(str) shouldBe Right((a, n))
  }

  "regex number" in forAll(posNum[Int], arbitrary[String]) { (a, s) =>
    val input = s"$a$s"
    p.run(p.regex("\\d+".r))(input) shouldBe Right(a.toString)
  }

  "context sensitivity" in forAll(chooseNum[Int](0, 10), arbitrary[Char]) {
    (a, c) =>
      val str: String = List.fill(a)(c).mkString
      val input = s"$a$str"

      val pCharList: Parser[List[Char]] = p.regex("\\d+".r).flatMap { s =>
        val n: Int = s.toInt
        listOfN(n, char(c))
      }

      p.run(pCharList)(input) shouldBe Right(List.fill(a)(c))
  }

  "skipL" in forAll(whitespacesGen, nonEmptyStringGen) { (ws, s) =>
    val input = s"$ws$s"
    p.run(skipL(whitespaces, string(s)))(input) shouldBe Right(s)
  }

  "skipR" in forAll(nonEmptyStringGen, whitespacesGen) { (s, ws) =>
    val input = s"$s$ws"
    p.run(skipR(string(s), whitespaces))(input) shouldBe Right(s)
  }

  "skipLnR" in forAll(Gen.identifier) { id =>
    val input: String = s""""$id""""
    p.run(skipLnR(char('"'), p.regex("[A-Za-z0-9]*".r), char('"')))(input) shouldBe Right(
      id)
  }

  "token" in forAll(Gen.identifier, whitespacesGen) { (id, ws) =>
    val input: String = s""""$id"$ws"""
    val parser = token(skipLnR(char('"'), p.regex("""\w+""".r), char('"')))
    p.run2(parser)(input) shouldBe Right((Location(input, input.length), id))
  }

  "token 2" in forAll(Gen.identifier, whitespacesGen, Gen.identifier) {
    (id1, ws, id2) =>
      val input: String = s""""$id1"$ws"$id2""""
      val parser = token(skipLnR(char('"'), p.regex("""[^"]*""".r), char('"')))
      p.run2(product(parser, parser))(input) shouldBe
        Right((Location(input, input.length), (id1, id2)))
  }

  "token 3" in forAll(Gen.identifier,
                      whitespacesGen,
                      whitespacesGen,
                      Gen.identifier,
                      whitespacesGen) { (id1, ws1, ws2, id2, ws3) =>
    val input: String = s"$id1$ws1:$ws2$id2$ws3"

    val idP = token(p.regex("""\w+""".r))
    val colonP = token(p.char(':'))
    val valueP = for {
      a <- idP
      b <- colonP
      c <- idP
    } yield (a, b, c)

    val r: Either[ParseError, (Location, (String, Char, String))] =
      p.run2(valueP)(input)
    r shouldBe Right((Location(input, input.length), (id1, ':', id2)))
  }

  "token 4" in {
    val id1 = "x"
    val input: String = s"$id1 : "

    val idP = token(p.regex("""\w+""".r)).map(dump)
    val colonP = token(p.char(':')).map(dump)
    val valueP = for {
      a <- idP
      b <- colonP
    } yield (a, b)

    val r: Either[ParseError, (Location, (String, Char))] =
      p.run2(valueP)(input)
    r shouldBe Right((Location(input, input.length), (id1, ':')))
  }

  private def dump[A](a: A): A = {
    println(s"-- dump: '$a'")
    a
  }
}
