package fpinscala.state

import fpinscala.state.RNG._
import org.scalatest.{FreeSpec, Matchers}

class StateSpec extends FreeSpec with Matchers {

  val SmallSampleSize: Int = 10000

  private trait RngTestSuite[A] {
    def underTest: RNG => (A, RNG)

    def runTest(seed: Int)(valueTest: A => Unit) {
      var distribution = Map[A, Int]().withDefaultValue(0)
      var (v, rng) = underTest(RNG.Simple(seed))
      distribution = distribution.updated(v, 1)

      for (_ <- 1 to SmallSampleSize) {
        val (v1, rng1) = underTest(rng)
        withClue(s"generated value: $v1") {
          valueTest(v1)
        }
        distribution = updateDistribution(distribution, v1)
        rng = rng1
        v = v1
      }

      allGeneratedValuesShouldBeUnique(distribution)
    }

    def updateDistribution(distribution: Map[A, Int], v: A): Map[A, Int] = distribution.updated(v, distribution(v) + 1)

    def allGeneratedValuesShouldBeUnique(distribution: Map[A, Int]): Unit = distribution.foreach { p =>
      p._2 should be(1)
    }
  }

  object RngTestStream {
    def apply[A](seed: Int)(underTest: Rand[A]): Stream[(A, Map[A, Int])] = {
      def loop(rng: RNG, distribution: Map[A, Int]): Stream[(A, Map[A, Int])] = {
        val (v, r) = underTest(rng)
        val d = distribution.updated(v, distribution(v) + 1)
        (v, d) #:: loop(r, d)
      }

      loop(RNG.Simple(seed), Map.empty.withDefaultValue(0))
    }
  }

  "exercise 6.1 nonNegativeInt" in {
    new RngTestSuite[Int] {
      override def underTest: (RNG) => (Int, RNG) = nonNegativeInt
    }.runTest(123) { a: Int => a should be >= 0 }

    new RngTestSuite[Int] {
      override def underTest: (RNG) => (Int, RNG) = nonNegativeIntMyAnswer
    }.runTest(123) { a: Int => a should be >= 0 }
  }

  "exercise 6.2 double" in {
    new RngTestSuite[Double] {
      override def underTest: (RNG) => (Double, RNG) = double
    }.runTest(123) { a: Double =>
      a should be >= 0.0
      a should be < 1.0
    }
  }

  "exercise 6.3 intDouble" in {
    new RngTestSuite[(Int, Double)] {
      override def underTest: (RNG) => ((Int, Double), RNG) = intDouble
    }.runTest(123) { a: (Int, Double) =>
      a._2 should ((be >= 0.0) and (be < 1.0))
    }
  }

  "exercise 6.3 doubleInt" in {
    new RngTestSuite[(Double, Int)] {
      override def underTest: (RNG) => ((Double, Int), RNG) = doubleInt
    }.runTest(123) { a: (Double, Int) =>
      a._1 should ((be >= 0.0) and (be < 1.0))
    }
  }

  "exercise 6.3 double3" in {
    new RngTestSuite[(Double, Double, Double)] {
      override def underTest: (RNG) => ((Double, Double, Double), RNG) = double3
    }.runTest(123) { a =>
      a._1 should ((be >= 0.0) and (be < 1.0))
      a._2 should ((be >= 0.0) and (be < 1.0))
      a._3 should ((be >= 0.0) and (be < 1.0))
    }
  }

  "exercise 6.4 ints" in {
    new RngTestSuite[List[Int]] {
      override def underTest: (RNG) => (List[Int], RNG) = ints(5)
    }.runTest(123) { as =>
      as.size shouldBe 5
    }
  }

  "exercise nonNegativeEven" in {
    new RngTestSuite[Int] {
      override def underTest: (RNG) => (Int, RNG) = nonNegativeEven
    }.runTest(123) { a => a % 2 shouldBe 0 }
  }

  "exercise doubleViaMap" in {
    RngTestStream(123)(doubleViaMap).take(SmallSampleSize).foreach { case (x, d) =>
      x should be >= 0.0
      x should be < 1.0
      d(x) shouldBe 1
    }

    RngTestStream(123)(doubleViaMap).take(SmallSampleSize).last._2.size shouldBe SmallSampleSize
  }

  "exercise doubleViaMyMap" in {
    RngTestStream(123)(doubleViaMyMap).take(SmallSampleSize).foreach { case (x, d) =>
      x should be >= 0.0
      x should be < 1.0
      d(x) shouldBe 1
    }

    RngTestStream(123)(doubleViaMyMap).take(SmallSampleSize).last._2.size shouldBe SmallSampleSize
  }

  "map2 test" in {
    def doubleInt2: Rand[(Double, Int)] = map2(double, int) { (d, i) => (d, i) }

    RngTestStream(123)(doubleInt2).take(SmallSampleSize).foreach { case ((d, i), c) =>
      d should be >= 0.0
      d should be < 1.0
      c((d, i)) shouldBe 1
    }
  }

  "exercise 6.7 sequence" in {
    RngTestStream(123)(ints_(5)).take(SmallSampleSize).foreach { case (list, d) =>
      list.size shouldBe 5
      d(list) shouldBe 1
    }
  }

  "exercise 6.8 nonNegativeLessThan" in {
    RngTestStream(123)(nonNegativeLessThan(10)).take(SmallSampleSize).foreach { case (x, d) =>
      x should be >= 0
      x should be < 10
    }
  }

  "exercise 6.9 map_" in {
    def doubleViaMap_ : Rand[Double] = map_(nonNegativeInt) { i => i / (Int.MaxValue.toDouble + 1) }

    RngTestStream(123)(doubleViaMap_).take(SmallSampleSize).foreach { case (x, d) =>
      x should be >= 0.0
      x should be < 1.0
      d(x) shouldBe 1
    }
  }

  "exercise 6.9 map2_ " in {
    def doubleInt2: Rand[(Double, Int)] = map2_(double, int) { (d, i) => (d, i) }

    RngTestStream(123)(doubleInt2).take(SmallSampleSize).foreach { case ((d, i), c) =>
      d should be >= 0.0
      d should be < 1.0
      c((d, i)) shouldBe 1
    }
  }

  "check for normal distribution" in {
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

    val sampleSize = SmallSampleSize
    val (_, d: Map[Int, Int]) = RngTestStream(123)(rollDie).take(sampleSize).last
    d should have size 6
    val ps = d.values.map { x => x.toDouble / sampleSize }
    val mu = 1.toDouble / 6
    println(d)
    println(mu)
    println(ps)
    ps.foreach { p =>
      math.abs(p - mu) should be < 0.01
    }
  }

  "exercise 6.10 unit, map, map2, flatMap" in {
    val s0: State[Unit, Int] = State.unit(10)
    s0.run(()) shouldBe(10, ())
    val s1 = s0.map(x => x * 100)
    s1.run(()) shouldBe(1000, ())

    def add(x: Int, y: Int): Int = x + y

    val sx: State[Unit, Int] = State.unit(10)
    val sy: State[Unit, Int] = State.unit(20)
    sx.map2(sy)(add).run(()) shouldBe(30, ())

    def incInt: State[Int, Int] = State { s => (s, s + 1) }
    def oddInt: State[Int, Int] = incInt.flatMap { x => if (x % 2 != 0) State.unit(x) else oddInt }
    def evenInt: State[Int, Int] = incInt.flatMap { x => if (x % 2 == 0) State.unit(x) else evenInt }

    def intSequence: State[Int, List[Int]] =
      State.sequence(List(evenInt, evenInt, evenInt, incInt, oddInt, oddInt, oddInt, oddInt, incInt))

    val (lst: List[Int], s) = intSequence.run(0)
    println(lst)
    lst should have length 9
    lst.sliding(2).foreach { a =>
      a.head should be < a.last
    }

    val vec = lst.toVector
    vec(0) % 2 shouldBe 0
    vec(1) % 2 shouldBe 0
    vec(2) % 2 shouldBe 0
    vec(3) shouldBe vec(2) + 1
    vec(4) % 2 shouldBe 1
    vec(5) % 2 shouldBe 1
    vec(6) % 2 shouldBe 1
    vec(7) % 2 shouldBe 1
    vec(8) shouldBe vec(7) + 1
    s shouldBe vec(8) + 1
  }

  "exercise 6.11 Candy Dispenser" in {
    import State._
    simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(
      Machine(locked = true, coins = 10, candies = 5)
    ) should be(
      ((14, 1), Machine(locked = true, coins = 14, candies = 1))
    )

    simulateMachine(List(Turn)).run(
      Machine(locked = true, coins = 10, candies = 5)
    ) should be(
      ((10, 5), Machine(locked = true, coins = 10, candies = 5))
    )

    simulateMachine(List()).run(
      Machine(locked = true, coins = 10, candies = 5)
    ) should be(
      ((10, 5), Machine(locked = true, coins = 10, candies = 5))
    )
  }
}
