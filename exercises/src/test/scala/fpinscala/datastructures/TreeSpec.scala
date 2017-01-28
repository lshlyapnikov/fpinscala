package fpinscala.datastructures

import fpinscala.datastructures.Tree._
import org.scalatest.{FreeSpec, Matchers}

class TreeSpec extends FreeSpec with Matchers {
  "exercise 3.25 size" in {
    Tree.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    Tree.size(Leaf(1)) shouldBe 1
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 7

    Tree.size2(Branch(Leaf(1), Leaf(2))) shouldBe 3
    Tree.size2(Leaf(1)) shouldBe 1
    Tree.size2(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 7
  }

  "exercise 3.26 maximum" in {
    maximum(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 4
  }

  "exercise 2.27 depth" in {
    depth2(Leaf(1)) shouldBe 1
    depth2(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 4

    depth(Leaf(1)) shouldBe 1
    depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) shouldBe 4
  }

  "exercise 2.28 map" in {
    map(Leaf(1)) { a => (a * 10).toString } shouldBe Leaf("10")
    map(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) { a => (a * 10).toString } shouldBe
      Branch(Leaf("10"), Branch(Leaf("20"), Branch(Leaf("30"), Leaf("40"))))
  }

  "exercise 2.29 fold" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))

    foldOverNodes(Leaf(1), 100)(_ + _) shouldBe 100 + 1
    foldOverNodes(tree, 10)(_ * _) shouldBe 10 * 1 * 2 * 3 * 4

    fold(Leaf(1))( a => a)(_ + _) shouldBe 1
    fold(tree)(a => a)(_ * _) shouldBe 1 * 2 * 3 * 4

    sizeViaFold(tree) shouldBe 7

    maximumViaFold(tree) shouldBe 4

    depthViaFold(tree) shouldBe 4

    mapViaFold(tree) { a => (a * 10).toString } shouldBe
      Branch(Leaf("10"), Branch(Leaf("20"), Branch(Leaf("30"), Leaf("40"))))

  }
}
