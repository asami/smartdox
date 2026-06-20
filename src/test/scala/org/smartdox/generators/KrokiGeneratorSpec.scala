package org.smartdox.generators

import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

/*
 * @since   Jun. 21, 2026
 * @version Jun. 21, 2026
 * @author  ASAMI, Tomoharu
 */
class KrokiGeneratorSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "KrokiGenerator" should {
    "keep the direct SmartDox Docker image as the compatibility default" in {
      Given("SmartDox is used directly without Cozy toolchain configuration")
      When("the compatibility Docker image default is inspected")
      val image = KrokiGenerator._default_direct_docker_image

      Then("the existing SmartDox PDF image remains the direct-use default")
      image shouldBe "simplemodeling/smartdox-pdf:latest"
    }

    "allow explicit Kroki Docker image override" in {
      Given("a Cozy toolchain image supplied through SmartDox Kroki configuration")
      _with_property("smartdox.kroki.docker.image", "ghcr.io/asami/cozy-toolchain:latest") {
        When("SmartDox resolves the Kroki Docker image")
        val image = KrokiGenerator.defaultDockerImage

        Then("the explicit Cozy toolchain image is used")
        image shouldBe "ghcr.io/asami/cozy-toolchain:latest"
      }
    }
  }

  private def _with_property[A](key: String, value: String)(body: => A): A = {
    val previous = Option(System.getProperty(key))
    System.setProperty(key, value)
    try {
      body
    } finally {
      previous match {
        case Some(v) => System.setProperty(key, v)
        case None => System.clearProperty(key)
      }
    }
  }
}
