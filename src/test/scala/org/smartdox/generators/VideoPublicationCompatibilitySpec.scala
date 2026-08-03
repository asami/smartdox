package org.smartdox.generators

import java.io.File
import org.junit.runner.RunWith
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.StringData
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator.{Config, Context}

/*
 * @since   Aug.  4, 2026
 * @version Aug.  4, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class VideoPublicationCompatibilitySpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "VideoPublication source-page compatibility" should {
    "preserve transcript and RDF publication links for a matched legacy video" in {
      Given("the established video source package and publication fixture")
      val input = Realm.create(new File("src/test/resources/video-package-site"))
      val generator = new AntoraGenerator(_context, DoxSite.Config.default, Some(new File("src/test/resources/video-publication-fixture")))

      When("the Antora article is generated")
      val article = _article(generator.generate(input))

      Then("the existing player, transcript, and RDF links remain rendered")
      article should include ("src=\"/repository/video/tutorial/0.1.0/tutorial-0.1.0.mp4\"")
      article should include ("Transcript")
      article should include ("/repository/video/tutorial/0.1.0/tutorial-0.1.0.transcript.json")
      article should include ("RDF Turtle")
      article should include ("/repository/video/tutorial/0.1.0/tutorial-0.1.0.ttl")
      article should include ("RDF JSON-LD")
      article should include ("/repository/video/tutorial/0.1.0/tutorial-0.1.0.jsonld")
      article should include ("RDF Manifest")
      article should include ("/repository/video/tutorial/0.1.0/tutorial-0.1.0.rdf-manifest.json")
    }

    "render the existing diagnostic when a video source package has no publication metadata" in {
      Given("a video source package without publication metadata")
      val input = Realm.create(new File("src/test/resources/video-package-site"))
      val generator = new AntoraGenerator(_context, DoxSite.Config.default)

      When("the Antora article is generated")
      val article = _article(generator.generate(input))

      Then("the ordinary article remains and reports the existing missing-metadata diagnostic")
      article should include ("Missing video publication metadata")
      article should include ("No publication metadata found for concepts/tutorial.video")
      article should not include ("smartdox-video-publication")
    }

    "keep an invalid compatibility publicPath available to the existing legacy player" in {
      Given("a matched legacy video publication whose ordinary-article adapter path is invalid")
      val input = Realm.create(new File("src/test/resources/video-package-site"))
      val generator = new AntoraGenerator(_context, DoxSite.Config.default, Some(new File("src/test/resources/video-publication-invalid-content-fixture")))

      When("the Antora source-page article is generated")
      val article = _article(generator.generate(input))

      Then("the established source-page renderer receives its original publicPath unchanged")
      article should include ("smartdox-video-publication")
      article should include ("<video")
      article should include ("src=\"not-a-site-path\"")
    }
  }

  private lazy val _context: Context = {
    val environment = Environment.createJaJp()
    val cliconfig = CliConfig.buildJaJp()
    new Context(environment, Config(cliconfig), environment.contextFoundation)
  }

  private def _article(realm: Realm): String =
    realm.get("antora.d/docs/concepts/modules/ROOT/pages/tutorial.adoc").collect {
      case data: StringData => data.string
    }.getOrElse(fail("Generated tutorial article is missing"))
}
