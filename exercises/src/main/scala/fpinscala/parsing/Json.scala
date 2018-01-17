package fpinscala.parsing

trait Json

object Json {

  case object JNull                          extends Json
  case class JNumber(get: Double)            extends Json
  case class JString(get: String)            extends Json
  case class JBool(get: Boolean)             extends Json
  case class JArray(get: IndexedSeq[Json])   extends Json
  case class JObject(get: Map[String, Json]) extends Json

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    val identifier = regex("[A-Za-z][0-9A-Za-z\\_]*".r)
    val colon      = char(':')
    val semicolon  = char(';')
    val comma      = char(',')
    val quote      = char('"')

    val propertyName = skipL(quote, skipR(identifier, quote))

    val stringValue = skipL(quote,

//    val jObject = product(token(char('{')
    ???
  }
}
