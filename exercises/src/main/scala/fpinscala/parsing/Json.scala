package fpinscala.parsing

trait Json

object Json {

  case object JNull extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case class JBool(get: Boolean) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JObject(get: Map[String, Json]) extends Json

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    val identifier = regex("""[A-Za-z][0-9A-Za-z_]*""".r)
    val value = regex("""[^"]*""".r)

    val colon = char(':')
    val semicolon = char(';')
    val comma = char(',')
    val quote = char('"')

    val objBegin: Parser[Char] = token(char('{'))
    val objEnd: Parser[Char] = token(char('}'))
    val fieldName: Parser[String] = token(skipLnR(quote, identifier, quote))
    val fieldValue: Parser[Json] = token(
      skipLnR(quote, value.map(JString), quote))

    val field: Parser[(String, Json)] = for {
      _ <- objBegin
      k <- fieldName
      _ <- token(colon)
      v <- fieldValue
      _ <- objEnd
    } yield (k, v)

    field.map(kv => JObject(Seq(kv).toMap))
  }
}
