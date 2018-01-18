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

  import Json._

  private val j = jsonParser(p)

  "identifier" in forAll(Gen.identifier, Gen.identifier) { (id, value) =>
    val input = s""""$id" : "$value" """

    p.run(j)(input) shouldBe Right(JObject(Map(id -> JString(value))))
  }


}
