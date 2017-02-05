package fpinscala.errorhandling

import org.scalatest.{FreeSpec, Matchers}

class EitherSpec extends FreeSpec with Matchers {
  "exercise 4.6" in {
    Right(10).map { a => a * 10 } shouldBe Right(100)
    (Left("error"): Either[String, Int]).map { a => a * 10 } shouldBe Left("error")

    Right(10).flatMap { a => Right(a * 10) } shouldBe Right(100)
    Left("error").flatMap[String, Int] { a: Int => Right(a * 10) } shouldBe Left("error")

    Right(10).orElse(Right(100)) shouldBe Right(10)
    (Left("error"): Either[String, Int]).orElse(Right(101)) shouldBe Right(101)

    def f(x: Int, y: Int): Int = x + y

    Left("error").map2(Right(10))(f) shouldBe Left("error")
    Right(1).map2(Left("error1"))(f) shouldBe Left("error1")
    Left("error1").map2(Left("error2"))(f) shouldBe Left("error1")
    Right(1).map2(Right(10))(f) shouldBe Right(11)
  }

  "exercise 4.7" in {
    def MyTry[R](a: => R): Either[Throwable, R] =
      try Right(a) catch {
        case e: Throwable => Left(e)
      }

    import fpinscala.errorhandling.Either._

    traverse(List("1", "2", "3")) { s => MyTry(s.toInt) } shouldBe Right(List(1, 2, 3))
    traverse(List("1", "a", "3")) { s => MyTry(s.toInt) } should
      matchPattern { case Left(_: NumberFormatException) => }
    traverse(Nil: List[String]) { s => MyTry(s.toInt) } shouldBe Right(Nil)

    traverse1(List("1", "2", "3")) { s => MyTry(s.toInt) } shouldBe Right(List(1, 2, 3))
    traverse1(List("1", "a", "3")) { s => MyTry(s.toInt) } should
      matchPattern { case Left(_: NumberFormatException) => }
    traverse1(Nil: List[String]) { s => MyTry(s.toInt) } shouldBe Right(Nil)

    traverse2(List("1", "2", "3")) { s => MyTry(s.toInt) } shouldBe Right(List(1, 2, 3))
    traverse2(List("1", "a", "3")) { s => MyTry(s.toInt) } should
      matchPattern { case Left(_: NumberFormatException) => }
    traverse2(Nil: List[String]) { s => MyTry(s.toInt) } shouldBe Right(Nil)

    sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    sequence(List(Right(1), Left(new RuntimeException()), Right(3))) should
      matchPattern { case Left(_: RuntimeException) => }
    sequence(Nil: List[Either[Exception, Int]]) shouldBe Right(Nil)
  }

  "exercise 4.8" in {
    def MyTry[R](a: => R): Either[Throwable, R] =
      try Right(a) catch {
        case e: Throwable => Left(e)
      }

    import fpinscala.errorhandling.Either._

    traverseAll(List("1", "2", "3")) { s => MyTry(s.toInt) } shouldBe Right(List(1, 2, 3))
    traverseAll(List("1", "a", "3")) { s => MyTry(s.toInt) } should
      matchPattern { case Left(List(_: NumberFormatException)) => }
    traverseAll(Nil: List[String]) { s => MyTry(s.toInt) } shouldBe Right(Nil)
    traverseAll(List("1", "a", "2", "b", "3")) { s => MyTry(s.toInt) } should
      matchPattern { case Left(List(_: NumberFormatException, _: NumberFormatException)) => }

    sequenceAll(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    sequenceAll(List(Right(1), Left(new RuntimeException()), Right(3))) should
      matchPattern { case Left(List(_: RuntimeException)) => }
    sequenceAll(Nil: List[Either[Exception, Int]]) shouldBe Right(Nil)
    sequenceAll(List(Right(1), Left(new RuntimeException()), Right(3), Left(new NumberFormatException()))) should
      matchPattern { case Left(List(_: RuntimeException, _: NumberFormatException)) => }
  }
}
