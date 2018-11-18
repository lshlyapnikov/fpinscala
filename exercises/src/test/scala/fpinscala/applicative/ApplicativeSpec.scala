package fpinscala.applicative

import org.scalatest.{FreeSpec, Matchers}

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
}
