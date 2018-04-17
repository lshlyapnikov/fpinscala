package fpinscala.monoids

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalactic.{Equality, Equivalence}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object MonoidLaws {
  def leftIdentity[A](m: Monoid[A])(a: A)(implicit eq: Equivalence[A]): Boolean =
    eq.areEquivalent(m.op(m.zero, a), a)

  def rightIdentity[A](m: Monoid[A])(a: A)(implicit eq: Equivalence[A]): Boolean =
    eq.areEquivalent(m.op(a, m.zero), a)

  def associative[A](m: Monoid[A])(a1: A, a2: A, a3: A)(implicit eq: Equivalence[A]): Boolean =
    eq.areEquivalent(m.op(a1, m.op(a2, a3)), m.op(m.op(a1, a2), a3))
}

class MonoidProperties[A](label: String, monoid: Monoid[A])(implicit g1: Arbitrary[A],
                                                            eq: Equivalence[A])
    extends Properties(label) {

  import MonoidLaws._

  property("left identity law") = forAll { (a: A) =>
    leftIdentity(monoid)(a)
  }

  property("right identity law") = forAll { (a: A) =>
    rightIdentity(monoid)(a)
  }

  property("associative law") = forAll { (a1: A, a2: A, a3: A) =>
    associative(monoid)(a1, a2, a3)
  }
}

class MonoidSpec extends FreeSpec with Matchers with Checkers {

  import Monoid._

  def checkAll(props: Properties): Unit = {
    for ((name, prop) <- props.properties) yield {
      check(prop.label(name))
    }
  }

  "String concatenation monoid laws" in {
    checkAll(new MonoidProperties("stringMonoid", stringMonoid))
  }

  "List fold" in {
    check { list: List[String] =>
      val expected: String = list.mkString
      list.foldLeft(stringMonoid.zero)(stringMonoid.op) === expected &&
      list.foldRight(stringMonoid.zero)(stringMonoid.op) === expected
    }
  }

  "Int addition monoid laws" in {
    checkAll(new MonoidProperties("intAddition", intAddition))
  }

  "Int multiplication monoid laws" in {
    checkAll(new MonoidProperties("intMultiplication", intMultiplication))
  }

  "Boolean Or monoid laws" in {
    checkAll(new MonoidProperties("booleanOr", booleanOr))
  }

  "Boolean And monoid laws" in {
    checkAll(new MonoidProperties("booleanAnd", booleanAnd))
  }

  "Option Monoid" in {
    checkAll(new MonoidProperties("optionMonoid[String]", optionMonoid[String]))
    checkAll(new MonoidProperties("optionMonoid[Int]", optionMonoid[Int]))
    checkAll(new MonoidProperties("optionMonoid[Boolean]", optionMonoid[Boolean]))
  }

  "foldMapV" in {
    import fpinscala.monoids.Monoid.foldMapV
    check { input: IndexedSeq[String] =>
      foldMapV(input, stringMonoid)(identity) === input.mkString
    }
  }

  "foldMap iterate" in {
    import fpinscala.monoids.Monoid.foldMap
    check { input: List[Int] =>
      foldMap(input, listMonoid[Int])(b => List(b)) === input
    }
  }

  "foldRight via foldMap" in {
    import fpinscala.monoids.Monoid.foldRight
    check { input: List[Int] =>
      val actual: List[Int] = foldRight(input)(List.empty[Int])((a, b) => a :: b)
      actual === input
    }
  }

  "foldLeft via foldMap" in {
    import fpinscala.monoids.Monoid.foldLeft
    check { input: List[Int] =>
      val actual: List[Int] = foldLeft(input)(List.empty[Int])((b, a) => a :: b)
      actual === input.reverse
    }
  }

  "foldMapV 2" in {
    import fpinscala.monoids.Monoid.foldMapV
    check { input: IndexedSeq[Int] =>
      foldMapV(input, stringMonoid)(String.valueOf) ===
        input.map(_.toString).mkString
    }
  }

  "parFoldMap" in {
    import fpinscala.monoids.Monoid.parFoldMap
    import scala.concurrent.ExecutionContext.Implicits.global
    check { input: IndexedSeq[String] =>
      val actualF: Future[String] = parFoldMap(input, stringMonoid)(identity)
      val actual: String          = Await.result(actualF, Duration.Inf)
      actual === input.mkString
    }
  }

  "parFoldMap 2" in {
    import fpinscala.monoids.Monoid.parFoldMap
    import scala.concurrent.ExecutionContext.Implicits.global
    check { input: IndexedSeq[Int] =>
      val actualF: Future[String] = parFoldMap(input, stringMonoid)(String.valueOf)
      val actual: String          = Await.result(actualF, Duration.Inf)
      actual === input.map(_.toString).mkString
    }
  }

  "perf" in {
    import fpinscala.monoids.Monoid.foldMapV
    val r                      = new Random(0)
    val input: IndexedSeq[Int] = IndexedSeq.fill(1234567)(r.nextInt())
    val t0                     = System.nanoTime()
    val actual                 = foldMapV(input, stringMonoid)(String.valueOf)
    val t1                     = System.nanoTime()
    println(nanosToMillis(t1 - t0))
    actual === input.map(_.toString).mkString
  }

  "par perf" in {
    import fpinscala.monoids.Monoid.parFoldMap
    import scala.concurrent.ExecutionContext.Implicits.global
    val r                       = new Random(0)
    val input: IndexedSeq[Int]  = IndexedSeq.fill(1234567)(r.nextInt())
    val t0                      = System.nanoTime()
    val actualF: Future[String] = parFoldMap(input, stringMonoid)(String.valueOf)
    val actual: String          = Await.result(actualF, Duration.Inf)
    val t1                      = System.nanoTime()
    println(nanosToMillis(t1 - t0))
    actual === input.map(_.toString).mkString
  }

  def nanosToMillis(d: Double): Double = d / 1000000.0

  "find max monoid" in {
    val findMax: Monoid[Option[Int]] = new Monoid[Option[Int]] {
      override def op(o1: Option[Int], o2: Option[Int]): Option[Int] = {
        if (o1.isEmpty) o2
        else map2Options(o1, o2)((a, b) => if (b >= a) b else a)
      }

      override def zero: Option[Int] = None
    }

    check { input: List[Int] =>
      val actual: Option[Int] = foldMap(input, findMax)(x => Option(x))
      if (input.isEmpty) actual.isEmpty
      else actual === Some(input.max)
    }
  }

  "is ordered" in {
    check { input: IndexedSeq[Int] =>
      if (input.isEmpty) {
        ordered(input) === true
      } else if (input.length == 1) {
        ordered(input) === true
      } else {
        val sortedInput = input.sorted
        ordered(sortedInput) === true

        val shuffledInput = Random.shuffle(input)
        if (shuffledInput != sortedInput) ordered(shuffledInput) === false
        else ordered(shuffledInput) === true
      }
    }
  }

  "semigroup with plus" in {
    val m = semigroupMonoid[Int]((a1, a2) => a1 + a2)
    checkAll(new MonoidProperties("semigroupMonoidWithPlus", m))
  }

  "semigroup with multiply" in {
    val m = semigroupMonoid[Int]((a1, a2) => a1 * a2)
    checkAll(new MonoidProperties("semigroupMonoidWithMultiply", m))
  }

  "wcMonoid" in {
    checkAll(new MonoidProperties("wcMonoid", wcMonoid)(Arbitrary(wcGen), Equality.default[WC]))
  }

  def myAssert[A](actual: A, expected: A)(eq: Equivalence[A]): Boolean = {
    val result = eq.areEquivalent(actual, expected)
    if (!result) println(s"ERROR: expected: $expected, actual: $actual")
    result
  }

  "countWords" in {
    check {
      forAll(textGen) { str: String =>
        val expected: Int = str.split("""\s+""").count(!_.isEmpty)
        val actual: Int   = countWords(str)
        myAssert(actual, expected)(Equality.default[Int])
      }.label("countWords")
    }
  }

  def tokenGen: Gen[String] =
    Gen.oneOf(Gen.const(""),
              Gen.const(" "),
              Gen.const("\n"),
              Gen.const("\r"),
              Gen.const("\t"),
              Gen.identifier)

  def textGen: Gen[String] = Gen.listOf(tokenGen).map(_.mkString(" "))

  def stubGen: Gen[Stub] = textGen.map(Stub)

  def partGen: Gen[Part] =
    for {
      l <- tokenGen
      c <- Gen.posNum[Int]
      r <- tokenGen
    } yield Part(l, c, r)

  def wcGen: Gen[WC] =
    for {
      b    <- Arbitrary.arbitrary[Boolean]
      stub <- stubGen
      part <- partGen
    } yield if (b) stub else part
}
