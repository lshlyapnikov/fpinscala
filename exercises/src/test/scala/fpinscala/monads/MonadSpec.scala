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

  def myAssert[A](actual: A, expected: A)(implicit eq: Equivalence[A]): Boolean = {
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
    forAll(Gen.listOf(Arbitrary.arbitrary[Int]), Gen.choose(0, 32)) { (list: List[Int], n: Int) =>
      val actual: List[List[Int]] = listMonad.replicateM(n, list)
      if (list.isEmpty) myAssert(actual, List(List.empty[Int]))
      else myAssert(actual, List.fill(n)(list))
    }
  }

  "listMonad.repliaceM(1, List())" in {
    listMonad.replicateM(1, List.empty[Int]) shouldBe List(List.empty[Int])
  }
}
