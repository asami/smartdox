package org.smartdox.generators

import java.io.File
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.StringData
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator.{Config, Context}
import org.yaml.snakeyaml.Yaml

/*
 * @since   Oct.  7, 2026
 * @version Oct.  7, 2026
 * @author  ASAMI, Tomoharu
 */
class AntoraKrokiEndpointSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Generated Antora Kroki endpoints" should {
    "locale playbook products" which {
      "use literal IPv4 loopback and port 8000 in the actual single-locale root playbook" in {
        Given("the existing single JA locale root fixture and a fresh normal generator context")
        val input = Realm.create(new File("src/test/resources/site-single-locale-root"))
        val env = Environment.createJaJp()
        val config = Config(CliConfig.buildJaJp())
        val context = new Context(env, config, env.contextFoundation)
        val generator = new AntoraGenerator(context, DoxSite.Config.default)

        When("SmartDox generates the single-locale Antora product")
        val result = generator.generate(input)

        Then("the root playbook exists and locale-subdirectory playbooks are absent")
        result.get("antora.d/antora-playbook.yml") should not be empty
        result.get("antora.d/ja/antora-playbook.yml") shouldBe empty
        result.get("antora.d/en/antora-playbook.yml") shouldBe empty

        When("the actual root playbook is read and its YAML attributes are parsed")
        val (playbook, attributes) = _playbook_attributes(result, "antora.d/antora-playbook.yml")

        Then("the generated endpoint uses literal IPv4 loopback and port 8000 and retains SVG and diagram fetching")
        attributes.get("kroki-server-url") shouldBe "http://127.0.0.1:8000"
        attributes.get("kroki-default-format") shouldBe "svg"
        attributes.get("kroki-fetch-diagram") shouldBe "true"
        playbook should not include ("9609")
      }

      "use literal IPv4 loopback and port 8000 in both actual JA and EN multi-locale playbooks" in {
        Given("the existing multi-locale site and publication fixtures and a fresh normal generator context")
        val input = Realm.create(new File("src/test/resources/site-mini"))
        val publication = Some(new File("src/test/resources/publish-fixture"))
        val env = Environment.createJaJp()
        val config = Config(CliConfig.buildJaJp())
        val context = new Context(env, config, env.contextFoundation)
        val generator = new AntoraGenerator(context, DoxSite.Config.default, publication)

        When("SmartDox generates the multi-locale Antora product")
        val result = generator.generate(input)

        Then("both locale playbooks exist and the single-locale root playbook is absent")
        result.get("antora.d/ja/antora-playbook.yml") should not be empty
        result.get("antora.d/en/antora-playbook.yml") should not be empty
        result.get("antora.d/antora-playbook.yml") shouldBe empty

        Vector("ja", "en").foreach { locale =>
          Given(s"the generated $locale playbook in the multi-locale product")
          val path = s"antora.d/$locale/antora-playbook.yml"

          When(s"the actual $locale playbook is read and its YAML attributes are parsed")
          val (playbook, attributes) = _playbook_attributes(result, path)

          Then(s"the $locale endpoint uses literal IPv4 loopback and port 8000 and retains SVG and diagram fetching")
          attributes.get("kroki-server-url") shouldBe "http://127.0.0.1:8000"
          attributes.get("kroki-default-format") shouldBe "svg"
          attributes.get("kroki-fetch-diagram") shouldBe "true"
          playbook should not include ("9609")
        }
      }
    }
  }

  private def _playbook_attributes(realm: Realm, path: String): (String, java.util.Map[String, Any]) = {
    val playbook = realm.get(path) match {
      case Some(data: StringData) => data.string
      case _ => fail(s"Missing or non-string generated playbook: $path")
    }
    val yaml = new Yaml().load[java.util.Map[String, Any]](playbook)
    val asciidoc = yaml.get("asciidoc").asInstanceOf[java.util.Map[String, Any]]
    val attributes = asciidoc.get("attributes").asInstanceOf[java.util.Map[String, Any]]
    playbook -> attributes
  }
}
