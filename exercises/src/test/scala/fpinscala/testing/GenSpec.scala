package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.testing.Gen._
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable

class GenSpec extends FreeSpec with Matchers {
  "choose exercise 8.4" in {
    val gen1or2: Gen[Int] = choose(1, 2)

    val buffer = mutable.Buffer.empty[Int]
    var state: (Int, RNG) = (-1, RNG.Simple(0))
    for (_ <- 1 to 25) {
      state = gen1or2.sample.run(state._2)
      println(s"state: $state")
      buffer += state._1
    }

    buffer.toSet shouldBe Set(1, 2)
  }

  "unit exercise 8.5" in {
    val constGen: Gen[Int] = unit(123)
    val listOfConstsGen: Gen[List[Int]] = listOfN(25, constGen)
    val (actual: List[Int], _) = listOfConstsGen.sample.run(RNG.Simple(0))
    actual shouldBe List.fill(25)(123)
  }

  "boolean" in {
    val buffer = mutable.Buffer.empty[Boolean]
    var state: (Boolean, RNG) = (false, RNG.Simple(0))
    for (_ <- 1 to 25) {
      state = boolean.sample.run(state._2)
      println(s"state: $state")
      buffer += state._1
    }

    buffer.toSet shouldBe Set(true, false)
  }

  "boolean test 2" in {
    val listOfBooleansGen: Gen[List[Boolean]] = listOfN(25, boolean)
    val (actual: List[Boolean], _) = listOfBooleansGen.sample.run(RNG.Simple(0))
    actual should have length 25
    actual.toSet shouldBe Set(true, false)
  }

  val charGen: Gen[Char] = Gen.choose('A', 'z').map(i => i.toChar)

  val strGen: Gen[String] = for {
    n <- Gen.choose(0, 25)
    cs <- Gen.listOfN(n, charGen)
  } yield cs.mkString

  "flatMap exercise 8.6" in {

    val str25Gen: Gen[String] = Gen.listOfN(25, charGen).map(_.mkString)

    var state: (String, RNG) = ("", RNG.Simple(0))
    state = str25Gen.sample.run(state._2)
    println(s"state: $state")

    val (stringList, _) = Gen.listOfN(25, strGen).sample.run(RNG.Simple(0))
    stringList.size shouldBe 25
    stringList.foreach(println)
  }

  "listOfN dynamic" in {
    val strListGen = strGen.listOfN(Gen.choose(1, 100))
    val (strList, _) = strListGen.sample.run(RNG.Simple(0))
    println("----")
    strList.foreach(println)
  }

  "weighted" in {
    val n = 10000
    val gen = Gen.weighted((Gen.unit(1), 0.5), (Gen.unit(2), 0.5))
    val (list: List[Int], _) = gen.listOfN(n).sample.run(RNG.Simple(0))
    val c1 = list.count(_ == 1)
    val c2 = n - c1
    c1 * 1.0 / n shouldBe 0.5 +- 0.01
    c2 * 1.0 / n shouldBe 0.5 +- 0.01
  }
}
