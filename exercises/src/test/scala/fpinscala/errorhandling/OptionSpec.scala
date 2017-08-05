package fpinscala.errorhandling

import fpinscala.errorhandling.Option._
import org.scalatest.{FreeSpec, Matchers}

class OptionSpec extends FreeSpec with Matchers {

  "exercise 4.1" in {
    Some(10).map { a => a * 10 } shouldBe Some(100)
    (None: Option[Int]).map { a => a * 10 } shouldBe None

    Some(10).getOrElse(100) shouldBe 10
    (None: Option[Int]).getOrElse(100) shouldBe 100

    Some(10).flatMap { a => Some(a * 10) } shouldBe Some(100)
    (None: Option[Int]).flatMap { a => Some(a * 10) } shouldBe None

    Some(10).orElse(Some(100)) shouldBe Some(10)
    (None: Option[Int]).orElse(Some(101)) shouldBe Some(101)

    Some(10).filter { _ => true } shouldBe Some(10)
    Some(10).filter { _ => false } shouldBe None
    None.filter { _ => true } shouldBe None
    None.filter { _ => false } shouldBe None
  }

  "exercise 4.2 variance" in {
    variance(Nil) shouldBe None
    variance(Seq(1, 2, 3, 4, 5)) shouldBe Some(2.0)
  }

  "exercise 4.3 map2" in {
    def f(x: Int, y: Int): Int = x + y

    map2(None, Some(10))(f) shouldBe None
    map2(Some(1), None)(f) shouldBe None
    map2(None, None)(f) shouldBe None
    map2(Some(1), Some(10))(f) shouldBe Some(11)
  }

  "map3" in {
    def f(x: Int, y: Int, z: Int): Int = x + y + z

    map3(None, None, None)(f) shouldBe None
    map3(Some(1), Some(2), Some(3))(f) shouldBe Some(1 + 2 + 3)
    map3(None, Some(2), Some(3))(f) shouldBe None
    map3(Some(1), None, Some(3))(f) shouldBe None
    map3(Some(1), Some(2), None)(f) shouldBe None
  }

  "exercise 4.4 sequence" in {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(Nil: List[Option[Int]]) shouldBe Some(Nil)
    sequence(List(Some(1), None, Some(3))) shouldBe None
    sequence(List(None)) shouldBe None
  }

  "exercise 4.5 traverse" in {
    def MyTry[A](a: => A): Option[A] =
      try Some(a) catch {
        case _: Throwable => None
      }

    traverse(List("1", "2", "3")) { s => MyTry(s.toInt) } shouldBe Some(List(1, 2, 3))
    traverse(List("1", "a", "3")) { s => MyTry(s.toInt) } shouldBe None
    traverse(Nil: List[String]) { s => MyTry(s.toInt) } shouldBe Some(Nil)

    sequenceViaTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequenceViaTraverse(Nil: List[Option[Int]]) shouldBe Some(Nil)
    sequenceViaTraverse(List(Some(1), None, Some(3))) shouldBe None
    sequenceViaTraverse(List(None)) shouldBe None
  }
}
