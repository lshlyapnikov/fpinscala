package fpinscala.parsing

object MyParser {

  type Parser[+A] = Location => Either[ParseError, (Location, A)]

  object IteratingParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      p(Location(input)).map(t => t._2)

    //    override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = location => {
    //      val a = p1(location)
    //      if (a.isRight) a
    //      else p2(location)
    //    }

    def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] =
      location => pa(location) match {
        case Left(_) => pb(location)
        case r@_ => r // committed failure or success skips running `p2`
      }

    override def string(s: String): Parser[String] = { location: Location =>
      val length = s.length()
      if (s.isEmpty || location.offset + length > location.input.length)
        Left(location.toError(s))
      else {
        val result: String = location.input.substring(location.offset, location.offset + length)
        if (result == s) point(result)(location.advanceBy(length))
        else Left(location.toError(s))
      }
    }

    def slice[A](p: Parser[A]): Parser[String] =
      location0 =>
        p(location0).map {
          case (l1, _) =>
            val str = location0.input.substring(location0.offset, l1.offset)
            (l1, str)
        }

    //    def many[A](p: Parser[A]): Parser[List[A]] = { location: Location =>
    //      @tailrec
    //      def loop(l0: Location, acc: List[A]): (Location, List[A]) =
    //        p(l0) match {
    //          case Right((l1, a)) => loop(l1, a :: acc)
    //          case _              => (l0, acc)
    //        }
    //
    //      val r = loop(location, Nil)
    //      Right((r._1, r._2.reverse))
    //    }

    override def flatten[A](ppa: Parser[Parser[A]]): Parser[A] =
      l0 => ppa(l0).flatMap { case (l1, pa) => pa(l1) }

    override def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      l0 => p(l0).map { case (location1, a) => (location1, f(a)) }

    // forall a b. map2(point(a), point(b))((_, _)) = point((a, b))
    // forall a fb. map2(point(a), fb)((x, y) => y) = fb
    // forall fa b. map2(fa, point(b))((x, y) => x) = fa
    override def point[A](a: A): Parser[A] = location => Right((location, a))

    //    override def sequence[A](ps: List[Parser[A]]): Parser[List[A]] = {
    //      def f(location: Location): Either[ParseError, (Location, List[A])] = {
    //        val z: Either[ParseError, (Location, List[A])] = Right(location -> List.empty)
    //        val r = ps.foldLeft(z) { (acc, p) =>
    //          acc.flatMap {
    //            case (location0, as) =>
    //              p(location0).map {
    //                case (location1, a) =>
    //                  (location1, a :: as)
    //              }
    //          }
    //        }
    //        r.map { case (l, as) => (l, as.reverse) }
    //      }
    //
    //      f
    //    }
  }

}
