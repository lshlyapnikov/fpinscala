package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this.foldRight(List.empty[A]) { (a, b) => a :: b }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0  => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def takeWithLoop(n: Int): Stream[A] = {
    def loop(s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => cons(h(), loop(t(), n - 1))
      case _ => Empty
    }

    loop(this, n)
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val a: A = h()
      if (p(a)) cons(a, t().takeWhile(p))
      else Empty
    case _ => Empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A]) { (a, b) =>
      if (p(a)) cons(a, b)
      else empty
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true){ (a, b) => p(a) && b }

  def headOption: Option[A] = this.foldRight(None: Option[A]) { (a, _) => Option(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](op: A => B): Stream[B] = this.foldRight(Stream.empty[B]) { (a, b) => cons(op(a), b) }

  def flatMap[B](op: A => Stream[B]): Stream[B] = this.foldRight(Stream.empty[B]) { (a, b) => op(a).append(b) }

  def append[B >: A](bs: => Stream[B]): Stream[B] = this.foldRight(bs){ (a, b) => cons(a, b) }

  def filter(op: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A]) { (a, b) =>
    if (op(a)) cons(a, b)
    else b
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def go(a0: Int, a1: Int): Stream[Int] = cons(a0, go(a1, a0 + a1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map { case (a, s) =>
      cons(a, unfold(s)(f))
  }.getOrElse(Stream.empty[A])

  def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { t => Option((t._1, (t._2, t._1 + t._2))) }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(()) { _ => Option((a, ())) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n) { s => Option((s, s + 1)) }
}