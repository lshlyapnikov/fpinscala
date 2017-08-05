package test2

import scala.util.{Failure, Success, Try}

object Main {
  type Board = Vector[Vector[Int]]

  def main(args: Array[String]) {
    io.Source.stdin.getLines.foreach { ln =>
      parseLine(ln).map { v => validate(board(v)) } match {
        case Success(r) => println(formatResult(r))
        case Failure(e) => e.printStackTrace()
      }
    }
  }

  def parseLine(s: String): Try[Vector[Int]] = Try {
    s.split(",", 9 * 9).toVector.map { s => s.toInt }
  }

  def formatResult(b: Boolean): String = if (b) "True" else "False"

  def board(v: Vector[Int]): Board = {
    def loop(as: Vector[Int], acc: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      if (as.isEmpty) acc
      else {
        val (l, r) = as.splitAt(9)
        loop(r, acc :+ l)
      }
    }

    loop(v, Vector.empty)
  }

  def validate(b: Board): Boolean = allRowsValid(b) && allColumnsValid(b) && allSubGridsValid(b)

  def allRowsValid(b: Board): Boolean = b.forall(a => allUnique(a))

  def allColumnsValid(b: Board): Boolean = allRowsValid(b.transpose)

  def allSubGridsValid(b: Board): Boolean = subGridsStartingFrom(b, 0, 0).forall(allUnique)

  def subGridsStartingFrom(b: Board, m: Int, n: Int): Stream[Seq[Int]] = {
    if (m > 6 || n > 6) Stream.empty
    else {
      val (m1, n1) = nextSubGrid(m, n)
      subGridElements(b, m, n) #:: subGridsStartingFrom(b, m1, n1)
    }
  }

  def nextSubGrid(m: Int, n: Int): (Int, Int) =
    if (n == 6) (m + 3, 0)
    else (m, n + 3)

  def subGridElements(b: Board, m: Int, n: Int): Seq[Int] = {
    def loop(c: Int, acc: Seq[Int]): Seq[Int] = {
      if (c == 3) acc
      else loop(c + 1, acc ++ b(m + c).slice(n, n + 3))
    }
    loop(0, Nil)
  }

  def allUnique(as: Seq[Int]): Boolean = as.size == as.toSet.size
}

// 1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,1,3,4,5,6,7,8,9,1,2,4,5,6,7,8,9,1,2,3,5,6,7,8,9,1,2,3,4,6,7,8,9,1,2,3,4,5,7,8,9,1,2,3,4,5,6,8,9,1,2,3,4,5,6,7,9,1,2,3,4,5,6,7,8
// false

// 1,2,7,5,3,9,8,4,6,4,5,3,8,6,1,7,9,2,8,9,6,4,7,2,1,5,3,2,8,9,3,1,7,4,6,5,3,6,5,2,8,4,9,1,7,7,4,1,9,5,6,3,2,8,9,7,4,6,2,8,5,3,1,5,1,2,7,4,3,6,8,9,6,3,8,1,9,5,2,7,4
// true