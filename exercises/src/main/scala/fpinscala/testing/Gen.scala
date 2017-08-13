package fpinscala.testing

import fpinscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = this.check && p.check
  }

}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(s => RNG.int(s)).map(x => (x % 2) == 0))

  def double: Gen[Double] = Gen(State(s => RNG.double(s)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val range = stopExclusive - start + 1
    val s: State[RNG, Int] = State(s => RNG.nonNegativeInt(s)).map(x => (x % range) + start)
    Gen(s)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val w1: Double = g1._2
    val w2: Double = g2._2
    val threshold: Double = w1 / (w1 + w2)
    Gen.double.flatMap(d => if (d < threshold) g1._1 else g2._1)
  }
}

/*
trait Gen[A] {
  def sample: (A, RNG)
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}
*/

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = {
    val sampleB: State[RNG, B] = sample.map(f)
    Gen(sampleB)
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val sampleB: State[RNG, B] = sample.flatMap(a => f(a).sample)
    Gen(sampleB)
  }

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

}

trait SGen[+A] {

}

