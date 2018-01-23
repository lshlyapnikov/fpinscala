package fpinscala.parsing

import fpinscala.parsing.MyParser.Parser
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class JsonSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private implicit val noCharShrink: Shrink[Char] = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int] = Shrink.shrinkAny[Int]

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import p._
  import Json._

  private val j = jsonParser(p)

  "identifier" in forAll(Gen.identifier, Gen.identifier) { (id, str) =>
    val input = s""""$id"    : "$str"    """

    //    p.run(j)(input) shouldBe Right(JObject(Map(id -> JString(value))))

    val identifier = p.regex("""[A-Za-z][0-9A-Za-z_]*""".r)
    val value = p.regex("""[^"\r\n]*""".r)

    val colon = p.char(':')
    val semicolon = p.char(';')
    val comma = p.char(',')
    val quote = p.char('"')

    val fieldName: Parser[String] = token(skipLnR(quote, identifier, quote))
    val fieldDelim: Parser[Char] = token(colon)
    val fieldValue: Parser[Json] = token(skipLnR(quote, value.map(JString), quote))

    val field: Parser[(String, Json)] = for {
      k <- fieldName
      _ <- fieldDelim
      v <- fieldValue
    } yield (k, v)

    val r = p.run(field)(input)
    println(s"$input => $r")
    r shouldBe Right((id, JString(str)))
  }

}
