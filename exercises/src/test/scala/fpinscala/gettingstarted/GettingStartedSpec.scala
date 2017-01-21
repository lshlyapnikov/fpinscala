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
}
