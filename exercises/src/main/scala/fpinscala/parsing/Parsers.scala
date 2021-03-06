package fpinscala.parsing

import fpinscala.parsing.MyParser.IteratingParser.{char, many, map, map2, or, point, slice, string}
import fpinscala.parsing.MyParser.Parser

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def run2[A](p: Parser[A])(input: String): Either[ParseError, (Location, A)]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  def slice[A](p: Parser[A]): Parser[String]

  def flatten[A](ppa: Parser[Parser[A]]): Parser[A]

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = flatMap(pa)(a => point(f(a)))

  def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

  //  def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = flatten(map(pa)(f))

  def point[A](a: A): Parser[A]

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- pa
      b <- pb
    } yield f(a, b)

  def many[A](p: Parser[A]): Parser[List[A]] = {
    val r: Parser[List[A]] = map2(p, many(p))((a, as) => a :: as)
    or(r, point(List.empty[A]))
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    val p1: Parser[List[A]] = p.map(x => List(x))
    val r: Parser[List[A]]  = map2(p, many(p))((a, as) => a :: as)
    or(r, p1)
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def string(s: String): Parser[String]

  def regex(r: Regex): Parser[String]

  def traverse[A, B](pas: List[A])(f: A => Parser[B]): Parser[List[B]] = {
    val z: Parser[List[B]]   = point(List.empty[B])
    val pbs: List[Parser[B]] = pas.map(a => f(a))
    pbs.foldRight(z) { (p, acc) =>
      map2(p, acc)((b, bs) => b :: bs)
    }
  }

  def sequence[A](pas: List[Parser[A]]): Parser[List[A]] = traverse(pas)(identity)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = sequence(List.fill(n)(p))

  def product[A, B](pa: => Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    map2(pa, pb)((x, y) => (x, y))

  def count(str: String): Parser[Int] = map(many(string(str)))(_.length)

  def count(c: Char): Parser[Int] = slice(many(char(c))).map(_.length)

  val whitespace  = regex("""\s""".r)
  val whitespaces = slice(many(whitespace))

  def skipL[B](pa: Parser[Any], pb: Parser[B]): Parser[B] =
    map2(pa, pb)((_, b) => b)

  def skipR[A](pa: Parser[A], pb: => Parser[Any]): Parser[A] =
    map2(pa, pb)((a, _) => a)

  def skipLnR[B](pa: Parser[Any], pb: => Parser[B], pc: Parser[Any]): Parser[B] =
    (pa skipL pb) skipR pc

  def token[A](p: Parser[A]): Parser[A] = skipR(p, whitespaces)

  case class ParserOps[A](pa: Parser[A]) {
    def or[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    def |[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    def map[B](f: A => B): Parser[B] = self.map(pa)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(pa)(f)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(pa, pb)(f)

    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)

    def skipL[B](pb: Parser[B]): Parser[B] = self.skipL(pa, pb)

    def skipR(pb: Parser[Any]): Parser[A] = self.skipR(pa, pb)
  }

  object Laws {}

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def toError(msg: String, e: ParseError): ParseError =
    ParseError(List((this, msg)), e.otherFailures)

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  def end: Boolean = offset >= input.length
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {}
