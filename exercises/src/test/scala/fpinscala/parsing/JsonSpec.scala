package fpinscala.parsing

import fpinscala.parsing.MyParser.Parser
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Generators._

class JsonSpec
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  private implicit val noCharShrink: Shrink[Char] = Shrink.shrinkAny[Char]
  private implicit val noStringShrink: Shrink[String] = Shrink.shrinkAny[String]
  private implicit val noIntShrink: Shrink[Int] = Shrink.shrinkAny[Int]

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import Json._

  private val j = jsonParser(p)

  "simple object" in forAll(jsonObjStrGen) {
    case (input: String, id: String, str: String) =>
      p.run(j)(input) shouldBe Right(JObject(Map(id -> JString(str))))
  }

}
