package fpinscala.parsing

object MyParser {

  type Parser[+A] = Location => Either[ParseError, (Location, A)]

  object IteratingParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      p(Location(input)).map(t => t._2)

    override def char(c: Char): Parser[Char] =
      location =>
        if (location.offset + 1 > location.input.length)
          Left(ParseError(List(location -> c.toString)))
        else {
          val result: Char = location.input.charAt(location.offset)
          if (result == c) Right((location.advanceBy(1), result))
          else Left(ParseError(List(location -> c.toString)))
      }

    override def string(s: String): Parser[String] = {
      def f(location: Location): Either[ParseError, (Location, String)] = {
        val length = s.length()
        if (location.offset + length > location.input.length) Left(ParseError(List(location -> s)))
        else {
          val result: String = location.input.slice(location.offset, location.offset + length)
          if (result == s) Right((location.advanceBy(length), result))
          else Left(ParseError(List(location -> s)))
        }
      }

      f
    }

    override def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      sequence(List.fill(n)(p))

    // forall a b. map2(point(a), point(b))((_, _)) = point((a, b))
    // forall a fb. map2(point(a), fb)((x, y) => y) = fb
    // forall fa b. map2(fa, point(b))((x, y) => x) = fa
    private def point[A](a: A): Parser[A] = ???

    private def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] = {
      def r(location0: Location): Either[ParseError, (Location, C)] = {
        pa(location0).flatMap {
          case (location1, a) =>
            pb(location1).map {
              case (location2, b) =>
                (location2, f(a, b))
            }
        }
      }

      r
    }

    private def sequence[A](ps: List[Parser[A]]): Parser[List[A]] = {
      def f(location: Location): Either[ParseError, (Location, List[A])] = {
        val z: Either[ParseError, (Location, List[A])] = Right(location -> List.empty)
        ps.foldLeft(z) { (acc, p) =>
          acc.flatMap {
            case (location0, as) =>
              p(location0).map {
                case (location1, a) =>
                  (location1, a :: as)
              }
          }
        }
      }

      f
    }

  }

}
