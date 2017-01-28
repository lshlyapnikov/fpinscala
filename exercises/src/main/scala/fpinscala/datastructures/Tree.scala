package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def size2[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def loop(as: List[Tree[A]], acc: Int): Int = as match {
      case Nil => acc
      case Cons(Leaf(_), tail) => loop(tail, acc + 1)
      case Cons(Branch(left, right), tail) => loop(Cons(left, Cons(right, tail)), acc + 1)
    }

    loop(List(tree), 0)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(left, right) => Math.max(maximum(left), maximum(right))
  }

  def depth2[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l, r) => Math.max(loop(l, acc + 1), loop(r, acc + 1))
    }

    loop(tree, 0)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => Math.max(depth(l), depth(r)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def foldOverNodes[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = tree match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldOverNodes(l, foldOverNodes(r, z)(f))(f)
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree) { _ => 1 } { (b1, b2) => b1 + b2 + 1 }

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree) { a => a } { (b1, b2) => math.max(b1, b2) }

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree) { a => 1 } { (b1, b2) => math.max(b1, b2) + 1 }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree) { a => Leaf(f(a)): Tree[B] } { (b1: Tree[B], b2: Tree[B]) => Branch(b1, b2) }
}