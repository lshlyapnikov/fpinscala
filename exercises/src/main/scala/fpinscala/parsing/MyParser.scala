package fpinscala.parsing

object MyParser {

  type Parser[+A] = Location => Either[ParseError, (Location, A)]

  object IteratingParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      p(Location(input)).map(t => t._2)

    override def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = { location: Location =>
      val a = p1(location)
      if (a.isRight) a
      else p2(location)
    }

    override def char(c: Char): Parser[Char] =
      location =>
        if (location.end)
          Left(location.toError(c.toString))
        else {
          val result: Char = location.input.charAt(location.offset)
          if (result == c) point(result)(location.advanceBy(1))
          else Left(location.toError(c.toString))
      }

    override def string(s: String): Parser[String] = { location: Location =>
      val length = s.length()
      if (location.offset + length > location.input.length)
        Left(location.toError(s))
      else {
        val result: String = location.input.slice(location.offset, location.offset + length)
        if (result == s)
          point(result)(location.advanceBy(length))
        else Left(location.toError(s))
      }
    }

    def many[A](p: Parser[A]): Parser[List[A]] = { location: Location =>
      def loop(l0: Location, acc: List[A]): (Location, List[A]) =
        p(l0) match {
          case Right((l1, a)) => loop(l1, a :: acc)
          case _              => (l0, acc)
        }

      val r = loop(location, Nil)
      Right((r._1, r._2.reverse))
    }

    def count(c: Char): Parser[Int] = map(many(char(c)))(as => as.length)

//    def count(c: Char): Parser[Int] = { location: Location =>
//      def loop(l: Location, acc: Int): (Location, Int) = {
//        if (!l.end && l.input.charAt(l.offset) == c) loop(l.advanceBy(1), acc + 1)
//        else (l, acc)
//      }
//
//      Right(loop(location, 0))
//    }

    override def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      sequence(List.fill(n)(p))

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = { location0: Location =>
      p(location0).map {
        case (location1, a) =>
          (location1, f(a))
      }
    }

    // forall a b. map2(point(a), point(b))((_, _)) = point((a, b))
    // forall a fb. map2(point(a), fb)((x, y) => y) = fb
    // forall fa b. map2(fa, point(b))((x, y) => x) = fa
    def point[A](a: A): Parser[A] = { location: Location =>
      Right((location, a))
    }

    def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] = {
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
        val r = ps.foldLeft(z) { (acc, p) =>
          acc.flatMap {
            case (location0, as) =>
              p(location0).map {
                case (location1, a) =>
                  (location1, a :: as)
              }
          }
        }
        r.map { case (l, as) => (l, as.reverse) }
      }

      f
    }

  }

}
