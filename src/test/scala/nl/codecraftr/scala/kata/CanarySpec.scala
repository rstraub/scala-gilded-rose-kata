package nl.codecraftr.scala.kata

import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar

class CanarySpec
    extends AnyWordSpec
    with Matchers
    with MockitoSugar
    with BeforeAndAfter {

  before {
    println("fires before every test")
  }

  "canary" should {
    "run tests" in {
      true shouldBe true
    }
  }
}
