package fpinscala.monads

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalactic.Equivalence
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.forAll

class MonadSpec extends FreeSpec with Matchers with Checkers {

  import Monad._

  def checkAll(props: Properties): Unit = {
    for ((name, prop) <- props.properties) yield {
      check(prop.label(name))
    }
  }

  def myAssert[A](actual: A)(expected: A)(implicit eq: Equivalence[A]): Boolean = {
    val result = eq.areEquivalent(actual, expected)
    if (!result) println(s"ERROR: expected: $expected, actual: $actual")
    result
  }

  "listMonad.traverse(Nil)" in {
    listMonad.traverse(List.empty[Int])(x => List(x)) shouldBe List(List.empty[Int])
  }

  "optionMonad.traverse(Nil)" in {
    optionMonad.traverse(List.empty[Int])(x => Option(x)) shouldBe Some(List.empty[Int])
  }

  "listMonad.replicateM" in check {
    forAll(Gen.listOf(Arbitrary.arbitrary[Int]), Gen.choose(0, 5)) { (input: List[Int], n: Int) =>
      println(s"-- input: $input, n: $n")
      val actual: List[List[Int]] = listMonad.replicateM(n, input)
      if (input.isEmpty) myAssert(actual)(List.empty)
      else myAssert(actual)(List(List.fill(n)(input).flatten))
    }
  }

  "listMonad.repliateM(1, List())" in {
    myAssert(listMonad.replicateM(1, List.empty[Int]))(List.empty)
  }

  "listMonad.repliateM(0, List())" in {
    myAssert(listMonad.replicateM(0, List.empty[Int]))(List.empty)
  }

  "filterM" in {
    val as: List[Int]         = List(1, 2, 3, 4, 5)
    val bs: Option[List[Int]] = optionMonad.filterM(as)(x => Some(x % 2 == 0))
    bs shouldBe Some(List(2, 4))

    val cs: Option[List[Int]] = optionMonad.filterM(as)(_ => None)
    cs shouldBe None
  }

  "flatMapViaCompose" in {
    optionMonad.flatMapViaCompose(Some(10))(x => Some(x + 1)) shouldBe Some(11)
  }
}
