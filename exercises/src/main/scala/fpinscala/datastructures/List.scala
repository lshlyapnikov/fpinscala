package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail called on an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list, cannot replace head")
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => sys.error("empty list, nothing to drop")
      case Cons(_, t) =>
        if (n == 0) t
        else drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }

    @annotation.tailrec
    def reverse(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) => reverse(t, Cons(h, acc))
    }

    reverse(loop(l, Nil), Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, z) => z + 1 }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z) { (a: A, b: B) => f(b, a) }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (b: B, a: A) => f(a, b) }

  def reverse1[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }

    loop(l, Nil)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) { (z, a) => Cons(a, z) }

  def appendViaReverse[A](l: List[A], a: A): List[A] = reverse(Cons(a, reverse(l)))

  def appendViaFoldRight[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys) { (a, b) => Cons(a, b) }

  def appendViaRecursion[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(h, t) => Cons(h, appendViaRecursion(t, ys))
  }

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A]) { (a, b) => append(a, b) }

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int]) { (a, b) => Cons(a + 1, b) }

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String]) { (a, b) => Cons(a.toString, b) }

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B]) { (a, b) => Cons(f(a), b) }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A]) { (a, b) => if (f(a)) Cons(a, b) else b }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buffer = scala.collection.mutable.ListBuffer[A]()

    @annotation.tailrec
    def loop(as: List[A]): Unit = as match {
      case Nil => ()
      case Cons(h, t) =>
        if (f(h)) buffer += h
        loop(t)
    }

    loop(l)
    List(buffer: _*)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def flatMap1[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B]) { (b, a) => concat(List(b, f(a))) }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) { a => if (f(a)) List(a) else Nil }

  def sumElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumElements(t1, t2))
  }

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(as: List[A], bs: List[A]): Boolean = (as, bs) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) startsWith(t1, t2)
        else false
    }

    @annotation.tailrec
    def iterate(as: List[A]): Boolean =
      if (startsWith(as, sub)) true
      else as match {
        case Nil => false
        case Cons(h, t) => iterate(t)
      }

    iterate(sup)
  }
}
