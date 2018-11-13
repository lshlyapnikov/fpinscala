package fpinscala
package monads

import com.sun.org.apache.xerces.internal.util.ParserConfigurationSettings
import fpinscala.parsing.MyParser.IteratingParser
import parsing.{Parsers, _}
import testing._
import parallelism._
import state._
import parallelism.Par._

import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    val z: M[List[B]] = unit(List.empty[B])
    la.foldRight(z) { (a, acc) =>
      val b: M[B] = f(a)
      map2(acc, b) { (bs, b) =>
        b :: bs
      }
    }
  }

//  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
//    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = (a: A) => {
    flatMap(f(a))(b => g(b))
  }

  def flatMapViaCompose[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose((_: Unit) => ma, f)()
  }

  def _filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val mlba: M[List[(Boolean, A)]] = traverse(ms) { a =>
      map(f(a))((_, a))
    }
    map(mlba) { lba =>
      lba.filter(ba => ba._1).map(ba => ba._2)
    }
  }

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val z: M[List[A]] = unit(List.empty[A])

    def g(a: A, acc: M[List[A]]): M[List[A]] =
      compose(f, (flag: Boolean) => if (flag) map(acc)(a :: _) else acc)(a)

    ms.foldRight(z)((a, b) => g(a, b))
  }

  def filterM_[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) =>
      compose(f, (b: Boolean) => if (b) map(y)(x :: _) else y)(x))

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma) { ma: M[A] =>
    ma
  }

  // Implement in terms of `join` and `map`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = ???
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.point(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type F[X] = State[S, X] })#F] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)
  }

  lazy val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B]         = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader {
      r: R =>
        val a: A = st.run(r)
        f(a).run(r)
    }
  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}
