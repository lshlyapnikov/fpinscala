package fpinscala.parsing

import fpinscala.parsing.MyParser.Parser
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Generators._

class JsonSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private implicit val noCharShrink: Shrink[Char]     = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int]       = Shrink.shrinkAny[Int]

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import Json._

  private val j = jsonParser(p)

  implicit override val generatorDrivenConfig = PropertyCheckConfig(maxSize = 10)

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

}
