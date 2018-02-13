package fpinscala.parsing

import fpinscala.parsing.Generators._
import fpinscala.parsing.MyParser.Parser
import org.scalacheck.Shrink
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class JsonSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private implicit val noCharShrink: Shrink[Char]     = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int]       = Shrink.shrinkAny[Int]

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import Json._

  private val j = jsonParser(p)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(sizeRange = 10)

  "plain object with one field" in forAll(jsonFieldGen) {
    case (input: String, id: String, str: String) =>
      p.run(j)(s"{$input}") shouldBe Right(JObject(Map(id -> JString(str))))
  }

  "empty object" in forAll(Generators.whitespacesGen) { ws =>
    p.run(j)(s"{$ws}") shouldBe Right(JObject(Map.empty))
  }

  "object with two fields" in {
    val input = """{  "s"  :  "111"  ,  "b"  :  "222"  } """
    p.run(j)(input) shouldBe Right(
      JObject(
        Map(
          "s" -> JString("111"),
          "b" -> JString("222")
        )))
  }

  "object with multiple fields" in forAll(jsonObjectGen) {
    case (input, fields) =>
      println(s"---- input: $input")
      val jsonFields = fields.map {
        case (k, v) =>
          k -> JString(v)
      }.toMap
      p.run(j)(input) shouldBe Right(JObject(jsonFields))
  }

  "object with multiple fields of different types" in {
    val input =
      """{"a": 123.45,
        |"b" : 6789,
        |"c" : "abcd",
        | "dtrue": true,
        |  "dfalse" :false
        | }""".stripMargin

    p.run(j)(input) shouldBe Right(
      JObject(
        Map(
          "a"      -> JNumber(123.45d),
          "b"      -> JNumber(6789.0d),
          "c"      -> JString("abcd"),
          "dtrue"  -> JBool(true),
          "dfalse" -> JBool(false)
        )))

  }
}
