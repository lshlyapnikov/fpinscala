package fpinscala.parsing

import fpinscala.parsing.MyParser.Parser
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class JsonSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val p: Parsers[Parser] = MyParser.IteratingParser

  import Json._

  private val j = jsonParser(p)



}
