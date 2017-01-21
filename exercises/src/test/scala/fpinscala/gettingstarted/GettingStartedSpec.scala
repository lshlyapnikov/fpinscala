package fpinscala.gettingstarted

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}


class GettingStartedSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "factorial" in forAll(Gen.posNum[Int]) { num =>
    import fpinscala.gettingstarted.MyModule.{factorial, factorial3}
    val expected: Int = (1 to num).toStream.foldLeft(1) { (z, a) => z * a }
    factorial(num) shouldBe expected
    factorial3(num) shouldBe expected
  }

  "isSorted" in {
    import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

    def gt(a: Int, b: Int): Boolean = a >= b

    isSorted(Array(1, 2, 3, 4), gt) shouldBe true
    isSorted(Array(2, 2, 3, 4), gt) shouldBe true
    isSorted(Array(2, 2, 2, 2), gt) shouldBe true
    isSorted(Array(3, 2, 3, 4), gt) shouldBe false
  }

  "curry and uncurry" in {
    import fpinscala.gettingstarted.PolymorphicFunctions._
    def isNumberAfterConcat(s: String, i: Int): Boolean = (s + i).forall(_.isDigit)

    isNumberAfterConcat("123", 5) shouldBe true
    isNumberAfterConcat("123a", 5) shouldBe false

    curry(isNumberAfterConcat)("123")(1) shouldBe true
    curry(isNumberAfterConcat)("aaa")(1) shouldBe false

    val curried: String => Int => Boolean = (a: String) => (b: Int) => (a + b).forall(_.isDigit)
    curried("123")(4) shouldBe true
    curried("bbb")(4) shouldBe false
    uncurry(curried)("123", 3) shouldBe true
  }

  "compose" in {
    import fpinscala.gettingstarted.PolymorphicFunctions.compose

    def g(s: String): Boolean = s.forall(_.isDigit)

    def f(a: Int): String = a.toString

    compose(g, f)(10) shouldBe true
    g(f(10)) shouldBe true
    (g _).compose(f)(10) shouldBe true
  }
}
