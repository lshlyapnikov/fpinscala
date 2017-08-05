package test1

import scala.util._

object Main {
  def main(args: Array[String]) {
    io.Source.stdin.getLines.foreach { ln =>
      parse(ln) match {
        case Success(c) => println(checksum(c))
        case Failure(e) => e.printStackTrace()
      }
    }
  }

  private def parse(s: String): Try[Int] = Try(s.toInt)

  private def checksum(i: Int): Int = {
    val cs = constituents(i).reverse
    cs.foldLeft((0, false)) { (b, a) =>
      if (b._2) (b._1 + constituents(a * 2).sum, false)
      else (b._1 + constituents(a).sum, true)
    }._1
  }

  private def constituents(i: Int): List[Int] = {
    def loop(a: Int, acc: List[Int]): List[Int] = {
      if (a == 0) acc
      else loop(a / 10, (a % 10) :: acc)
    }

    loop(i, List())
  }
}