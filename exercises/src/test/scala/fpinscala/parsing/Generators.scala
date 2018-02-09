package fpinscala.parsing

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

object Generators {
  def nonEmptyStringGen: Gen[String] = arbitrary[String].filter(_.nonEmpty)

  def whitespacesGen: Gen[String] = Gen.sized { n =>
    Gen.listOfN(n, Gen.oneOf(' ', '\n', '\r', '\t')).map(_.mkString)
  }

  def jsonObjStrGen: Gen[(String, String, String)] =
    for {
      id <- Gen.identifier
      str <- nonEmptyStringGen
      ws1 <- whitespacesGen
      ws2 <- whitespacesGen
      ws3 <- whitespacesGen
      ws4 <- whitespacesGen
      ws5 <- whitespacesGen
    } yield (s"""{$ws1"$id"$ws2:$ws3"$str"$ws4}$ws5""", id, str)
}
