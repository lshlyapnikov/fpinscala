package fpinscala.parsing

import scala.util.matching.Regex

object MyParser {

  type Parser[+A] = Location => Either[ParseError, (Location, A)]

  object IteratingParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      run2(p)(input).map(t => t._2)

    override def run2[A](p: Parser[A])(
        input: String): Either[ParseError, (Location, A)] =
      p(Location(input))

    def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] =
      location =>
        pa(location) match {
          case Left(_) => pb(location)
          case r @ _   => r // committed failure or success skips running `p2`
      }

    override def string(s: String): Parser[String] = { location: Location =>
      val length = s.length()

      if (s.isEmpty)
        Right((location, s))
      else if (location.offset + length > location.input.length)
        Left(location.toError(s))
      else {
        val result: String =
          location.input.substring(location.offset, location.offset + length)
        if (result == s) Right((location.advanceBy(length), result))
        else Left(location.toError(s))
      }
    }

    override def regex(r: Regex): Parser[String] = l0 => {
      if (l0.end) Left(l0.toError(s"regex: $r"))
      else {
        val str = l0.input.substring(l0.offset)
        r.findPrefixOf(str) match {
          case Some(x) => Right((l0.advanceBy(x.length), x))
          case None    => Left(l0.toError(s"regex: $r"))
        }
      }
    }

    def slice[A](p: Parser[A]): Parser[String] =
      location0 =>
        p(location0).map {
          case (l1, _) =>
            val str = location0.input.substring(location0.offset, l1.offset)
            (l1, str)
      }

    override def flatten[A](ppa: Parser[Parser[A]]): Parser[A] =
      l0 => ppa(l0).flatMap { case (l1, pa) => pa(l1) }

    //    override def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    //      l0 => p(l0).map { case (location1, a) => (location1, f(a)) }

    override def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] =
      l0 => {
        println(s"--- $l0, '${l0.input.substring(l0.offset)}'")
        pa(l0) match {
          case Right((l1, a)) => f(a)(l1)
          case Left(e)        => Left(e) //Left(l0.toError("", e))
        }
      }

    // forall a b. map2(point(a), point(b))((_, _)) = point((a, b))
    // forall a fb. map2(point(a), fb)((x, y) => y) = fb
    // forall fa b. map2(fa, point(b))((x, y) => x) = fa
    override def point[A](a: A): Parser[A] = location => Right((location, a))
  }

}
