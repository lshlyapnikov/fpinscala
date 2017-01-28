package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FreeSpec, Matchers}

class ListSpec extends FreeSpec with Matchers {
  "exercise 3.1" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x shouldBe 3

    List.apply(Seq(2, 3, 4): _*)
  }

  "exercise 3.2 tail" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
    tail(List(1)) shouldBe Nil
    a[RuntimeException] shouldBe thrownBy(tail(Nil))
  }

  "exercise 3.3 setHead" in {
    setHead(List(1, 2, 3, 4), 100) shouldBe List(100, 2, 3, 4)
    a[RuntimeException] shouldBe thrownBy(setHead(Nil, 1))
  }

  "exercise 3.4 drop" in {
    drop(List(1, 2, 3, 4), 0) shouldBe List(1, 2, 3, 4)
    drop(List(1, 2, 3, 4), 1) shouldBe List(2, 3, 4)
    drop(List(1, 2, 3, 4), 3) shouldBe List(4)
    drop(List(1, 2, 3, 4), 4) shouldBe Nil
    drop(Nil, 0) shouldBe Nil
    a[RuntimeException] shouldBe thrownBy(drop(List(1), 2))
    a[RuntimeException] shouldBe thrownBy(drop(Nil, 1))

    val l2 = List(2, 3, 4)
    val l1 = Cons(1, l2)
    drop(l1, 0) shouldBe theSameInstanceAs(l1)
    drop(l1, 1) shouldBe theSameInstanceAs(l2)
  }

  "exercise 3.5 dropWhile" in {
    def isEven(x: Int): Boolean = x % 2 == 0

    dropWhile(List(2, 4, 6, 7), isEven) shouldBe List(7)
    dropWhile(List(1, 2, 4), isEven) shouldBe List(1, 2, 4)
    dropWhile(Nil, isEven) shouldBe Nil
    dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) shouldBe List(4, 5)
  }

  "exercise 3.6 init" in {
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
    a[RuntimeException] shouldBe thrownBy(init(Nil))
  }

  "exercise 3.8" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
    foldRight(List(10, 2), 5.0) { (a, b) => a / b } shouldBe 10 / (2 / 5.0)
  }

  "exercise 3.9 length" in {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(Nil) shouldBe 0
  }

  "exercise 3.10 foldLeft" in {
    foldLeft(List(1, 2, 3), Nil: List[Int]) { (z, a) => Cons(a, z) } shouldBe List(3, 2, 1)
  }

  "exercise 3.11" in {
    // sum
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    // product
    foldLeft(Nil: List[Int], 1)(_ * _) shouldBe 1
    foldLeft(List(2, 3, 4), 1)(_ * _) shouldBe 24
    // length
    foldLeft(List(1, 2, 3, 4, 5), 0) { (z, _) => z + 1 } shouldBe 5
    foldLeft(Nil, 0) { (z, _) => z + 1 } shouldBe 0
    foldLeft(List(10, 2), 5.0) { (b, a) => b / a } shouldBe (5.0 / 10) / 2
  }

  "exercise 3.12 reverse" in {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    reverse(Nil) shouldBe Nil
  }

  "exercise 3.13" in {
    foldLeftViaFoldRight(List(10, 2), 5.0) { (b, a) => b / a } shouldBe 5.0 / 10 / 2
    foldLeft(List(4, 3), 9) { (b, a) => b % a } shouldBe 9 % 4 % 3
    foldLeftViaFoldRight(List(4, 3), 9) { (b, a) => b % a } shouldBe (9 % 4) % 3
    foldLeftViaFoldRight(List(1, 2, 3), Nil: List[Int]) { (b, a) => Cons(a, b) } shouldBe List(3, 2, 1)

    foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
    foldRightViaFoldLeft(List(2, 3), 8) { (a, b) => b % a } shouldBe 8 % 3 % 2
  }

  "exercise 3.14 append" in {
    appendViaReverse(List(1, 2, 3), 4) shouldBe List(1, 2, 3, 4)
    appendViaFoldRight(List(1, 2, 3), List(4, 5)) shouldBe List(1, 2, 3, 4, 5)
    appendViaRecursion(List(1, 2, 3), List(4, 5)) shouldBe List(1, 2, 3, 4, 5)
  }

  "exercise 3.15 concat" in {
    concat(List(List(1, 2), List(3, 4), List(5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
    concat(List(List(1), Nil, List(2), Nil, List(3))) shouldBe List(1, 2, 3)
  }

  "exercise 3.16 - 3.22 " in {
    add1(List(10, 20, 30)) shouldBe List(11, 21, 31)
    doubleToString(List(1.1, 2.2, 3.3)) shouldBe List("1.1", "2.2", "3.3")
    map(List(1, 2, 3)) { a => a.toString } shouldBe List("1", "2", "3")
    filter(List(1, 2, 3, 4)) { a => a % 2 == 0 } shouldBe List(2, 4)
    filter2(List(1, 2, 3, 4)) { a => a % 2 == 0 } shouldBe List(2, 4)
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    flatMap1(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    filterViaFlatMap(List(1, 2, 3, 4)) { a => a % 2 == 0 } shouldBe List(2, 4)
  }

  "exercise 3.22 .. zip and etc" in {
    sumElements(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldBe List(4, 10, 18)
  }

  "exercise 3.24 hasSequence" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
  }

}
