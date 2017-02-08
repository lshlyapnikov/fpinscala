package fpinscala.laziness

import org.scalatest.{FreeSpec, Matchers}
import fpinscala.laziness.Stream._

import scala.util.Random

class StreamSpec extends FreeSpec with Matchers {

  def randomInts: Stream[Int] = cons(Random.nextInt(), randomInts)

  "Stream.apply is not lazy" in {
    var c = 0
    val s: Stream[Int] = Stream({ c += 1; 1 }, { c += 1; 2 }, { c += 1; 3 })
    c shouldBe 3
  }

  "Stream.cons is lazy" in {
    var c = 0
    val s: Stream[Int] = cons({ c += 1; 10 }, cons({ c += 1; 20 }, cons({ c += 1; 30 }, Stream.empty)))
    c shouldBe 0
  }

  "exercise 5.1 toList" in {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
    Stream.empty[Int].toList shouldBe List.empty[Int]
    Stream(1).toList shouldBe List(1)
  }

  "exercise 5.2 take" in {
    randomInts.take(10).toList should have length 10
    Stream(1, 2, 3).take(1).toList shouldBe List(1)
    Stream.empty[Int].take(1).toList shouldBe List.empty[Int]
    Stream(1).take(1).toList shouldBe List(1)

    randomInts.takeWithLoop(10).toList should have length 10
    Stream(1, 2, 3).takeWithLoop(1).toList shouldBe List(1)
    Stream.empty[Int].takeWithLoop(1).toList shouldBe List.empty[Int]
    Stream(1).takeWithLoop(1).toList shouldBe List(1)
  }

  "exercise 5.3 takeWhile" in {
    Stream(1, 2, 3).takeWhile(_ % 2 == 0).toList shouldBe List.empty
    Stream(1).takeWhile(_ % 2 == 0).toList shouldBe List.empty[Int]
    Stream.empty[Int].takeWhile(_ % 2 == 0).toList shouldBe List.empty[Int]
    Stream(2, 4, 1, 2, 3).takeWhile(_ % 2 == 0).toList shouldBe List(2, 4)
    Stream(2).takeWhile(_ % 2 == 0).toList shouldBe List(2)
  }

  "exercise 5.4 forAll" in {
    var counter = 0
    cons({ counter += 1; 10 }, cons({ counter += 1; 21 }, cons({ counter += 1; 30 },
      cons({ counter += 1; 40 }, Stream.empty)))).forAll { i => i % 10 == 0 } shouldBe false
    counter shouldBe 2
    cons(10, cons(20, cons(30, Stream.empty))).forAll { i => i % 10 == 0 } shouldBe true
    Stream.empty[Int].forAll(_ % 2 == 0) shouldBe true
  }

  "exercise 5.5 takeWhileViaFoldRight" in {
    Stream(1, 2, 3).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List.empty
    Stream(1).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List.empty[Int]
    Stream.empty[Int].takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List.empty[Int]
    Stream(2, 4, 1, 2, 3).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List(2, 4)
    Stream(2).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List(2)
  }

  "exercise 5.6 headOption via foldRight" in {
    val testData = new StreamTestData {}
    testData.NonEmpty.headOption shouldBe Option(10)
    testData.counter shouldBe 1
    Stream.empty[Int].headOption shouldBe None
  }

  trait StreamTestData {
    var counter = 0

    val NonEmpty: Stream[Int] = cons({ counter += 1; 10 },
      cons({ counter += 1; 21 },
        cons({ counter += 1; 30 },
          cons({ counter += 1; 40 }, Stream.empty))))
  }

  "exercise 5.7 map" in {
    val testData = new StreamTestData {}
    val bs = testData.NonEmpty.map { a => a * 10 }
    testData.counter shouldBe 1
    bs.toList shouldBe List(100, 210, 300, 400)
    testData.counter shouldBe 4
    Stream.empty[Int].map { a => a * 10 }.toList shouldBe Nil
  }

  "exercise 5.7 flatMap" in {
    val testData = new StreamTestData {}
    val bs = testData.NonEmpty.flatMap { a => Stream(a, a * 10) }
    testData.counter shouldBe 1
    bs.toList shouldBe List(10, 100, 21, 210, 30, 300, 40, 400)
    testData.counter shouldBe 4
    Stream.empty[Int].map { a => a * 10 }.toList shouldBe Nil
  }

  "exercise 5.7 filter" in {
    val testData = new StreamTestData {}
    val bs = testData.NonEmpty.filter(_ % 2 == 0)
    testData.counter shouldBe 1
    bs.toList shouldBe List(10, 30, 40)
    testData.counter shouldBe 4
    Stream.empty[Int].filter(_ % 2 == 0).toList shouldBe Nil
  }

  "one iteration over the stream" in {
    val testData = new StreamTestData {}
    val bs = testData.NonEmpty.map(_ * 10).map(_ + 1).filter(_ > 0).filter(_ > 10)
    testData.counter shouldBe 1
    bs.toList shouldBe List(101, 211, 301, 401)
    testData.counter shouldBe 4
  }

  "infinite streams and ones.exits" in {
    ones.exists(_ % 2 != 0) shouldBe true
    ones.forAll(_ != 1) shouldBe false
    // should never terminate
    a[StackOverflowError] should be thrownBy {
      ones.exists(_ % 2 == 0)
    }
    a[StackOverflowError] should be thrownBy {
      ones.forAll(_ == 1)
    }
  }

  "exercise 5.8 constant" in {
    constant(10).take(3).toList shouldBe List(10, 10, 10)
  }

  "exercise 5.9 from" in {
    from(5).take(3).toList shouldBe List(5, 6, 7)
  }

  "exercise 5.10 fib" in {
    fibs.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
  }

  "exercise 5.11 unfold" in {
    unfold(()) { _ => Some(1, ()) }.take(5).toList shouldBe List(1, 1, 1, 1, 1)
    unfold(5) { s => if (s > 0) Option((s, s - 1)) else None }.toList shouldBe List(5, 4, 3, 2, 1)
  }

  "exercise 5.12" in {
    fibsViaUnfold.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
    constantViaUnfold(10).take(3).toList shouldBe List(10, 10, 10)
    fromViaUnfold(5).take(3).toList shouldBe List(5, 6, 7)
  }
}
