package fpinscala.monoids

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object MonoidLaws {
  def leftIdentity[A](m: Monoid[A])(a: A): Boolean =
    m.op(m.zero, a) == a

  def rightIdentity[A](m: Monoid[A])(a: A): Boolean =
    m.op(a, m.zero) == a

  def associative[A](m: Monoid[A])(a1: A, a2: A, a3: A): Boolean =
    m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
}

class MonoidProperties[A](monoid: Monoid[A])(implicit g1: Arbitrary[A])
    extends Properties("Monoid") {

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
      println(name)
      check(prop)
    }
  }

  "String concatenation monoid laws" in {
    checkAll(new MonoidProperties(stringMonoid))
  }

  "List fold" in {
    check { list: List[String] =>
      val expected: String = list.mkString
      list.foldLeft(stringMonoid.zero)(stringMonoid.op) === expected &&
      list.foldRight(stringMonoid.zero)(stringMonoid.op) === expected
    }
  }

  "Int addition monoid laws" in {
    checkAll(new MonoidProperties(intAddition))
  }

  "Int multiplication monoid laws" in {
    checkAll(new MonoidProperties(intMultiplication))
  }

  "Boolean Or monoid laws" in {
    checkAll(new MonoidProperties(booleanOr))
  }

  "Boolean And monoid laws" in {
    checkAll(new MonoidProperties(booleanAnd))
  }

  "Option Monoid" in {
    checkAll(new MonoidProperties(optionMonoid[String]))
    checkAll(new MonoidProperties(optionMonoid[Int]))
    checkAll(new MonoidProperties(optionMonoid[Boolean]))
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
    checkAll(new MonoidProperties(m))
  }

  "semigroup with multiply" in {
    val m = semigroupMonoid[Int]((a1, a2) => a1 * a2)
    checkAll(new MonoidProperties(m))
  }

  "wcMonoid" in {
    checkAll(new MonoidProperties(wcMonoid)(Arbitrary(wcGen)))
  }

  def textGen: Gen[String] = Gen.listOf(Arbitrary.arbitrary[String]).map(_.mkString(" "))

  def wordOrEmptyStrGen: Gen[String] =
    for {
      b <- Arbitrary.arbitrary[Boolean]
      s <- Arbitrary.arbitrary[String]
    } yield if (b) s else ""

  def stubGen: Gen[Stub] = textGen.map(Stub)

  def partGen: Gen[Part] =
    for {
      l <- wordOrEmptyStrGen
      c <- Gen.posNum[Int]
      r <- wordOrEmptyStrGen
    } yield Part(l, c, r)

  def wcGen: Gen[WC] =
    for {
      b    <- Arbitrary.arbitrary[Boolean]
      stub <- stubGen
      part <- partGen
    } yield if (b) stub else part
}
