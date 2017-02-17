package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  @annotation.tailrec
  def nonNegativeIntMyAnswer(rng: RNG): (Int, RNG) = {
    val r = rng.nextInt
    if (r._1 >= 0) r
    else nonNegativeIntMyAnswer(r._2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d: Double = i.toDouble / (Int.MaxValue.toDouble + 1)
    (d, r)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt) { i => i / (Int.MaxValue.toDouble + 1) }

  def doubleViaMyMap: Rand[Double] = mapMyImpl(nonNegativeInt) { i => i / (Int.MaxValue.toDouble + 1) }

  def mapMyImpl[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng: RNG =>
    val (a, rng1) = s(rng)
    (f(a), rng1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        loop(n - 1, r, i :: acc)
      }
    }

    loop(count, rng, Nil)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - (i % 2))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng0 =>
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng0 =>
    fs.foldRight((Nil: List[A], rng0: RNG)) { case (ra, (list, rng)) =>
      val (a, rng1) = ra(rng)
      (a :: list, rng1)
    }
  }

  def ints_(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng0 =>
    val (a, rng1) = f(rng0)
    g(a)(rng1)
  }

  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb) { b =>
      f(a, b)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map1[B](f: A => B): State[S, B] = State(s0 => {
    val (a, s1) = run(s0)
    (f(a), s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a => sb.map { b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s0 => {
    val (a, s1) = run(s0)
    f(a).run(s1)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { s0 =>
    fs.foldRight((Nil: List[A], s0)) { (a, b) =>
      val (list, s) = b
      val (b1, s1) = a.run(s)
      (b1 :: list, s1)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { s0 =>
    val r: ((Int, Int), Machine) = inputs.foldLeft(((s0.coins, s0.candies), s0)) { (b, input) =>
      val (_, s) = b
      process(input).run(s)
    }
    r
  }

  def process(input: Input): State[Machine, (Int, Int)] = State { s =>
    (input, s) match {
      case (Coin, Machine(true, candies, coins)) if candies > 0 =>
        ((coins + 1, candies), Machine(false, candies, coins + 1))
      case (Turn, Machine(false, candies, coins)) =>
        ((coins, candies - 1), Machine(true, candies - 1, coins))
      case (_, m@Machine(_, candies, coins)) =>
        ((coins, candies), m)
    }
  }
}
