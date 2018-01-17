package fpinscala.parsing

import fpinscala.parsing.MyParser.IteratingParser.{char, many, map, map2, or, point, slice, string}
import fpinscala.parsing.MyParser.Parser

import language.higherKinds

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  def slice[A](p: Parser[A]): Parser[String]

  def flatten[A](ppa: Parser[Parser[A]]): Parser[A]

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B]

  def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = flatten(map(pa)(f))

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

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def string(s: String): Parser[String]

  def traverse[A, B](pas: List[A])(f: A => Parser[B]): Parser[List[B]] = {
    val z: Parser[List[B]] = point(List.empty[B])
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

  case class ParserOps[A](pa: Parser[A]) {
    def or[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    def |[B >: A](pb: => Parser[B]): Parser[B] = self.or(pa, pb)

    def map[B](f: A => B): Parser[B] = self.map(pa)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(pa)(f)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(pa, pb)(f)

    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(pa, pb)
  }

  object Laws {}

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  def end: Boolean = offset >= input.length
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {}
