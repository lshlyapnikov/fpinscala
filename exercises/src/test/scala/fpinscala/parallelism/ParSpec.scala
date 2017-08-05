package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FreeSpec, Matchers}
import Par._

class ParSpec extends FreeSpec with Matchers {
  val es: ExecutorService = Executors.newCachedThreadPool()
  "exercise 7.5 Par.sequence" in {
    Par.run(es)(sequence(List(unit(1), unit(2), unit(3)))).get shouldBe List(1, 2, 3)
  }

}
