package fpinscala.parsing

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object Generators {
  def nonEmptyStringGen: Gen[String] = arbitrary[String].filter(_.nonEmpty)

  def whitespacesGen: Gen[String] = Gen.sized { n =>
    Gen.listOfN(n, " ").map(_.mkString)
//    Gen.listOfN(n, Gen.oneOf(' ', '\n', '\r', '\t')).map(_.mkString)
  }

  def jsonFieldGen: Gen[(String, String, String)] =
    for {
      id  <- Gen.identifier
      str <- nonEmptyStringGen
      ws1 <- whitespacesGen
      ws2 <- whitespacesGen
      ws3 <- whitespacesGen
      ws4 <- whitespacesGen
    } yield (s"""$ws1"$id"$ws2:$ws3"$str"$ws4""", id, str)

  def jsonObjectGen: Gen[(String, List[(String, String)])] =
    Gen.sized { n =>
      Gen.listOfN(n, jsonFieldGen).map { t3s =>
        val z = (List.empty[String], List.empty[(String, String)])
        val r = t3s.foldLeft(z) { (b, a) =>
          (a._1 :: b._1, (a._2, a._3) :: b._2)
        }
        (r._1.mkString("{", ",", "}"), r._2)
      }
    }

}
