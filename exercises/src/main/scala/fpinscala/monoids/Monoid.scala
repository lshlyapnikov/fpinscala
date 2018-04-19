package fpinscala.monoids

//import fpinscala.parallelism.Nonblocking._
//import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import fpinscala.monoids.Monoid.dual

import scala.concurrent._
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
//  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

//  import fpinscala.testing._
//  import Prop._
//  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = a => f(g(a))

    override def zero: A => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero              = m.zero
  }

  // TODO: what is semigroup???
  def semigroupMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(o1: Option[A], o2: Option[A]): Option[A] = (o1, o2) match {
      case (Some(a1), Some(a2)) => Some(f(a1, a2))
      case (x, None)            => x
      case (None, x)            => x
    }
    override def zero: Option[A] = None
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // TODO: figure this out!
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // TODO: figure this out!
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def map2Options[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) {
      m.zero
    } else if (as.length == 1) {
      f(as(0))
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val isOrdered = new Monoid[Option[(Boolean, Int)]] {
      override def op(o1: Option[(Boolean, Int)],
                      o2: Option[(Boolean, Int)]): Option[(Boolean, Int)] = {

        if (o1.isEmpty)
          o2
        else
          map2Options(o1, o2) { (a1, a2) =>
            if (!a1._1) a1
            else if (a2._2 > a1._2) a2
            else (false, a1._2)
          }
      }

      override def zero: Option[(Boolean, Int)] = None
    }

    val result: Option[(Boolean, Int)] = foldMap(ints.toList, isOrdered)(x => Option((true, x)))
    result.fold(true)(_._1) // empty sequence is ordered
  }

//  def par[A](m: Monoid[A]): Monoid[Par[A]] =
//    sys.error("todo")
//
//  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
//    sys.error("todo")

  def par[A](m: Monoid[A])(implicit ec: ExecutionContext): Monoid[Future[A]] =
    new Monoid[Future[A]] {
      override def op(f1: Future[A], f2: Future[A]): Future[A] =
        for {
          a1 <- f1
          a2 <- f2
        } yield m.op(a1, a2)

      override def zero: Future[A] = Future.successful(m.zero)
    }

  // TODO implement it using foldMapV
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B)(
      implicit ec: ExecutionContext): Future[B] = {

    val fm: Monoid[Future[B]] = par(m)

    def loop(xs: IndexedSeq[A]): Future[B] = {
      if (xs.isEmpty) {
        fm.zero
      } else if (xs.length == 1) {
        fm.zero.map(_ => f(xs(0)))
      } else if (xs.length <= 4 * 1024) {
        fm.zero.map(_ => foldMapV(xs, m)(f))
      } else {
        val pairF: Future[(IndexedSeq[A], IndexedSeq[A])] =
          fm.zero.map(_ => xs.splitAt(xs.length / 2))
        pairF.flatMap {
          case (l, r) =>
            fm.op(loop(l), loop(r))
        }
      }
    }

    loop(as)
  }

  sealed trait WC
  case class Stub(chars: String)                            extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def wordsAfterConcat(l: String, r: String): Int =
      if ((l + r).isEmpty) 0
      else 1

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, c1, r1), Part(l2, c2, r2)) => Part(l1, c1 + wordsAfterConcat(r1, l2) + c2, r2)
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2)
      case (Part(l1, c1, r1), Stub(s2))         => Part(l1, c1, r1 + s2)
      case (Stub(s1), Part(l2, c2, r2))         => Part(s1 + l2, c2, r2)
    }

    override def zero: WC = Stub("")
    //override def zero: WC = Part("", 0, "") -- zero + Stub must be Stub
  }

  def countWords(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unWc(a: String): Int = if (a.isEmpty) 0 else 1

    val toCount: WC => Int = {
      case Stub(a)       => unWc(a)
      case Part(l, c, r) => unWc(l) + c + unWc(r)
    }

    toCount(foldMapV(s.toIndexedSeq, wcMonoid)(wc))
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(x: (A, B), y: (A, B)): (A, B) = (ma.op(x._1, y._1), mb.op(x._2, y._2))

    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(x: A => B, y: A => B): A => B = a => mb.op(x(a), y(a))

    override def zero: A => B = _ => mb.zero
  }

  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
      (x.keySet ++ y.keySet).foldLeft(zero) { (b, k) =>
        b.updated(k, mv.op(x.getOrElse(k, mv.zero), y.getOrElse(k, mv.zero)))
      }
    }

    override def zero: Map[K, V] = Map.empty[K, V]
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    def g(a: A)(b: B): B = f(b, a)
    foldMap(as)(g)(dual(endoMonoid[B]))(z)
  }

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])((a, b) => a :: b)
}

object ListFoldable extends Foldable[List] {
//  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
//    sys.error("todo")
//  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
//    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
//  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
//  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
//    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

//  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
//  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
//    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    def loop(as: Tree[A], b: B): B = as match {
      case Leaf(a)      => mb.op(b, f(a))
      case Branch(l, r) => mb.op(loop(l, b), loop(r, b))
    }

    loop(as, mb.zero)
  }
//  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
//    sys.error("todo")
//  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(a => f(a)).getOrElse(mb.zero)

//  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
//    sys.error("todo")
//  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
}
