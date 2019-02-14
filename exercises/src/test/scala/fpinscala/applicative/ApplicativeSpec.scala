package fpinscala.applicative

import org.scalatest.{FreeSpec, Matchers}

import scala.language.reflectiveCalls
import scala.util.control.NonFatal

class ApplicativeSpec extends FreeSpec with Matchers {
  import fpinscala.applicative.Applicative._

  "sequence of a list of streams" in {
    val list: List[Stream[Int]] =
      List(Stream(1, 2, 3, 4, 5), Stream(10, 20, 30, 40, 50), Stream(100, 200, 300, 400, 500))
    list.foreach(println)
    val stream = streamApplicative.sequence(list)
    stream.foreach(println)
  }

  "validationApplicative" in {
    val list: List[Validation[String, Int]] = List(Failure("error1", Vector("error2", "error3")),
                                                   Success(1),
                                                   Failure("error4", Vector("error5")))
    val result: Validation[String, List[Int]] = validationApplicative.sequence(list)
    result shouldBe Failure("error1", Vector("error2", "error3", "error4", "error5"))
  }

  "treeTraverse" in {
    import fpinscala.applicative.Traverse.treeTraverse
    import fpinscala.applicative.Monad.eitherMonad

    type Out[X] = Either[String, X]

    def parse(s: String): Out[Int] =
      try {
        Right(s.toInt)
      } catch {
        case NonFatal(_) => Left(s"Cannot convert to Int: $s")
      }

    val t1: Tree[String] = Tree("1", List(Tree("2", List(Tree("3", List(Tree("4"))), Tree("5")))))
    val ev1: Applicative[({ type f[x] = Either[String, x] })#f] = eitherApplicative[String]
    val et1: Out[Tree[Int]] = treeTraverse.traverse(t1)(parse)(ev1)
    et1 shouldBe Right(Tree(1, List(Tree(2, List(Tree(3, List(Tree(4))), Tree(5))))))

    val t2: Tree[String] =
      Tree("1", List(Tree("2", List(Tree("3abc", List(Tree("4"))), Tree("5")))))
    val et2: Out[Tree[Int]] = treeTraverse.traverse(t2)(parse)(ev1)
    et2 shouldBe Left("Cannot convert to Int: 3abc")
  }

  "Traverse.foldLeft" in {
    import fpinscala.applicative.Traverse._
    // TODO: figure out the cause of java.lang.StackOverflowError
    listTraverse.foldLeft(List(10, 20, 30))(0)((b, a) => b + a) shouldBe 60
    listTraverse.foldLeft(List(10, 20, 30))(0)((b, a) => b - a) shouldBe -40
  }
}
