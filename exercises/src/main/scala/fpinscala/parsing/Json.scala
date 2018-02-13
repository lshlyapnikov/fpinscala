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
    val value      = regex("""[^"]+""".r)
    val number     = regex("""[0-9]+(\.[0-9]+)?""".r)

    val colon = char(':')
    val comma = char(',')
    val quote = char('"')

    val objBegin: Parser[Char]    = token(char('{'))
    val objEnd: Parser[Char]      = token(char('}'))
    val fieldName: Parser[String] = token(skipLnR(quote, identifier, quote))
    val strValue: Parser[Json]    = token(skipLnR(quote, value, quote)).map(JString)
    val numValue: Parser[Json]    = token(number).map(x => JNumber(x.toDouble))
    val boolValue: Parser[Json] =
      token(or(string("true"), string("false"))).map(x => JBool(x.toBoolean))

    val fieldValue: Parser[Json] = or(strValue, or(numValue, boolValue))

    val oneJsonField: Parser[(String, Json)] = for {
      k <- fieldName
      _ <- token(colon)
      v <- fieldValue
    } yield (k, v)

    val jsonFields: Parser[List[(String, Json)]] =
      or(map2(oneJsonField, many(skipL(token(comma), oneJsonField)))(_ :: _),
         oneJsonField.map(List(_)))

    val jsonObj = for {
      _  <- objBegin
      fs <- jsonFields
      _  <- objEnd
    } yield JObject(fs.toMap)

    val emptyObj = for {
      _ <- objBegin
      _ <- objEnd
    } yield JObject(Map.empty)

    or(jsonObj, emptyObj)
  }

  private def dump[A](a: A): A = {
    println(s"-- dump: '$a'")
    a
  }
}
