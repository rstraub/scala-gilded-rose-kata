package nl.codecraftr.scala.kata

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CanaryIntegrationIT extends AnyWordSpec with Matchers {
  "integration spec" should {
    "be run too" in {
      true shouldBe true
    }
  }
}
