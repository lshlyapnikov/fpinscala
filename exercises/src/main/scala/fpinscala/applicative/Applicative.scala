package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._
import monoids._

import language.{higherKinds, implicitConversions, reflectiveCalls}

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    //    type H = B => C
    //    val g: A => H     = f.curried
    //    val fh: F[B => C] = apply(unit(g))(fa)
    val g: A => B => C = f.curried
    val fh: F[B => C]  = map(fa)(g)
    apply(fh)(fb)
  }

  def map3_a[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val g: (A, B) => C => D = (a, b) => c => f(a, b, c)
    val fcd: F[C => D]      = map2(fa, fb)(g)
    apply(fcd)(fc)
  }

  def map3_b[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val g: (A, B) => C => D = (a, b) => c => f(a, b, c)
    val fcd: F[C => D]      = map2(fa, fb)(g)
    map2(fc, fcd)((c, cd) => cd(c))
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val g: A => B => C => D  = f.curried
    val fbcd: F[B => C => D] = apply(unit(g))(fa)
    val fcd: F[C => D]       = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  def map4_a[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val g: (A, B, C) => D => E = (a, b, c) => d => f(a, b, c, d)
    val fde: F[D => E]         = map3(fa, fb, fc)(g)
    apply(fde)(fd)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val g: A => B => C => D => E   = f.curried
    val fbcde: F[B => C => D => E] = apply(unit(g))(fa)
    val fcde: F[C => D => E]       = apply(fbcde)(fb)
    val fde: F[D => E]             = apply(fcde)(fc)
    apply(fde)(fd)
  }

  def map4_b[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val g: A => B => C => D => E = f.curried
    apply(apply(apply(apply(unit(g))(fa))(fb))(fc))(fd)
  }

  def map4_c[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val g: (A, B, C) => D => E = (a, b, c) => d => f(a, b, c, d)
    val fde: F[D => E]         = map3(fa, fb, fc)(g)
    map2(fd, fde)((d, de) => de(d))
  }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa) { (f: A => B, a: A) =>
      f(a)
    }
  }

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def mapViaMap2[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(())) { (a: A, _: Unit) =>
      f(a)
    }

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a: A, fbs: F[List[B]]) =>
      map2(f(a), fbs) { (b: B, bs: List[B]) =>
        b :: bs
      }
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => (a, b))

  def product1[G[_]](g: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
          f: (A, B) => C): (F[C], G[C]) = {
        (self.map2(fa._1, fb._1)(f), g.map2(fa._2, fb._2)(f))
      }
    }

  def compose[G[_]](g: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] =
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb) { (ga: G[A], gb: G[B]) =>
          g.map2(ga, gb)(f)
        }
    }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    // used foldRight initially, the answer uses foldLeft.
    // map is not ordered, so foldLeft is fine and probably faster, no need to do the reverse
    ofa.foldLeft(self.unit(Map.empty[K, V])) { (b: F[Map[K, V]], a: (K, F[V])) =>
      self.map2(a._2, b) { (v: V, m: Map[K, V]) =>
        m.updated(a._1, v)
      }
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]] = List.empty)

trait Monad[F[_]] extends Applicative[F] {
  self =>
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def compose2[G[_]](G: Monad[G]): Monad[({ type f[x] = F[G[x]] })#f] =
    new Monad[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fga) { ga: G[A] =>
          G.flatMap(ga) { a: A =>
            val fgb: F[G[B]] = f(a) // need G[B] in here!
            ???
          }
          ???
        }
    }
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
        case Left(e)  => Left(e)
        case Right(a) => f(a)
      }
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F],
                           N: Monad[N],
                           T: Traverse[N]): Monad[({ type f[x] = F[N[x]] })#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                           f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val optionAplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))
  }

  def eitherApplicative[E] = new Applicative[({ type f[x] = Either[E, x] })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def map2[A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] =
      fa.flatMap((a: A) => fb.map((b: B) => f(a, b)))
  }

  //  val treeApplicative = new Applicative[Tree] {
  //    override def unit[A](a: => A): Tree[A] = Tree(a)
  //
  //    override def map2[A, B, C](fa: Tree[A], fb: Tree[B])(f: (A, B) => C): Tree[C] = {
  //      map2(fa.tail, fb.tail)(f)
  //
  //      val tail: List[Tree[C]] = ???
  //      Tree(f(fa.head, fb.head), tail)
  //    }
  //  }

  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b))         => Success(f(a, b))
        case (Success(_), e @ Failure(_, _))  => e
        case (e @ Failure(_, _), Success(_))  => e
        case (Failure(a, as), Failure(b, bs)) => Failure(a, (as :+ b) ++ bs)
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  def myMonoidApplicative[T](m: Monoid[T]) = new Applicative[({ type f[x] = Const[T, x] })#f] {
    override def unit[A](a: => A): Const[T, A] = m.zero

    override def map2[A, B, C](fa: Const[T, A], fb: Const[T, B])(f: (A, B) => C): Const[T, C] = {
      val a: T = fa
      val b: T = fb
      m.op(a, b)
    }
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A]                        = a
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }

  val idApplicative = new Applicative[Id] {
    override def unit[A](a: => A): Id[A]                                    = a
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idMonad)
  }

  def map_a[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idApplicative)
  }

  def map_via_stream_applicative[A, B](fa: F[A])(f: A => B): F[B] = {
    val sa: Applicative[Stream] = fpinscala.applicative.Applicative.streamApplicative

    def g(a: A): Stream[B] = {
      val b: B = f(a)
      sa.unit(b)
    }

    traverse[Stream, A, B](fa)(g)(sa).head
  }

  def map_via_option_applicative[A, B](fa: F[A])(f: A => B): F[B] = {
    val oa: Applicative[Option] = fpinscala.applicative.Applicative.optionAplicative
    val g: A => Option[B]       = a => oa.unit(f(a))
    traverse[Option, A, B](fa)(g)(oa).get
  }

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    def stateB(a: A): State[S, B] =
      for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b
    traverseS(fa)((a: A) => stateB(a)).run(s)
  }

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def toReversedList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_: A, s: List[A]) => (s.head, s.tail))._1

  def reverse_a[A](fa: F[A]): F[A] = {
    val result: (F[A], List[A]) =
      mapAccum(fa, toReversedList(fa))((_: A, s: List[A]) => (s.head, s.tail))
    result._1
  }

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
      implicit G: Applicative[G],
      H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = super.map(fa)(f)

    override def sequence[G[_]: Applicative, A](fma: List[G[A]]): G[List[A]] = {
      val G: Applicative[G] = implicitly
      fma.foldRight(G.unit(List.empty[A])) { (ga: G[A], gla: G[List[A]]) =>
        G.map2(ga, gla)((a: A, la: List[A]) => a :: la)
      }
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G: Applicative[G] = implicitly
      fa match {
        case None    => G.unit(None)
        case Some(a) => G.map(f(a))((b: B) => Some(b))
      }
    }

    //
    //    override def sequence[G[_]: Applicative, A](fma: Option[G[A]]): G[Option[A]] = {
    //      val G: Applicative[G] = implicitly
    //      fma match {
    //        case None     => G.unit(None)
    //        case Some(ga) => G.map(ga)((a: A) => Some(a))
    //      }
    //    }
  }

  val treeTraverse = new Traverse[Tree] {
    self =>
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      val head: B             = f(fa.head)
      val tail: List[Tree[B]] = fa.tail.map((t: Tree[A]) => map(t)((a: A) => f(a)))
      Tree(head, tail)
    }

    override def sequence[G[_]: Applicative, A](fma: Tree[G[A]]): G[Tree[A]] = {
      val G: Applicative[G] = implicitly

      val ga: G[A] = fma.head
      val glta: G[List[Tree[A]]] = fma.tail.foldRight(G.unit(List.empty[Tree[A]])) {
        (tga: Tree[G[A]], glta: G[List[Tree[A]]]) =>
          val gta: G[Tree[A]] = sequence(tga)
          G.map2(gta, glta)(_ :: _)
      }

      G.map2(ga, glta)((a: A, lta: List[Tree[A]]) => Tree(a, lta))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
