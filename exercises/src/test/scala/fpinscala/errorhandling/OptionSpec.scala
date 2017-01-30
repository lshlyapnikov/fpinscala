package fpinscala.errorhandling

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

}
