package org.smartdox.metadata

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.smartdox.metadata.PublishMetadata.{VideoPresentation, VideoStatus}

/*
 * @since   Aug.  4, 2026
 * @version Aug.  4, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PublishMetadataSpec extends AnyWordSpec with Matchers with GivenWhenThen with ScalaCheckPropertyChecks {
  "PublishMetadata article media" should {
    "native registry parsing" which {
      "load provider-neutral media from an unrestricted JSON registry path" in {
        Given("an article-media entry outside the legacy publication key prefixes")
        val metadata = PublishMetadata.load(new File("src/test/resources/article-media-publication-fixture")).get

        When("the exact article and locale variants are resolved")
        val en = metadata.resolveArticleMedia("development-process/example", "en").get
        val ja = metadata.resolveArticleMedia("development-process/example", "ja").get

        Then("external and site-hosted published video retain independent infographic data")
        en.infographic.map(_.publicPath.toString) shouldBe Some("/en/development-process/images/example/video-summary-en.png")
        en.video.map(_.presentation) shouldBe Some(VideoPresentation.ExternalLink)
        en.projectableVideo.flatMap(_.watchUrl).map(_.toString) shouldBe Some("https://youtu.be/example-en")
        ja.infographic shouldBe empty
        ja.video.map(_.presentation) shouldBe Some(VideoPresentation.SiteHosted)
        ja.projectableVideo.flatMap(_.contentUrl).map(_.toString) shouldBe Some("/ja/development-process/videos/example.mp4")
      }

      "load canonical BCP-47 extensions from YAML" in {
        Given("a standalone YAML article-media registry with a Unicode locale extension")
        val metadata = PublishMetadata.load(new File("src/test/resources/article-media-publication-yaml-fixture")).get

        When("the locale is resolved with noncanonical casing")
        val variant = metadata.resolveArticleMedia("development-process/localized-example", "EN-us-U-ca-JAPANESE")

        Then("the complete canonical BCP-47 tag resolves without a fallback")
        variant.flatMap(_.projectableVideo).flatMap(_.watchUrl).map(_.toString) shouldBe Some("https://example.com/localized-example")
        metadata.resolveArticleMedia("development-process/localized-example", "en-US") shouldBe empty
      }

      "reject a native record with a leading-slash article identity" in {
        Given("a native article-media record with a non-site-relative identity")
        val records = Vector(_native("/development-process/example", _variants("en", _external_published)))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading identifies the invalid article identity")
        error.getMessage should include ("article identity")
      }

      "reject a noncanonical native locale tag" in {
        Given("a native record whose locale case is not canonical")
        val records = Vector(_native("development-process/example", _variants("ja-jp", _external_published)))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading rejects the locale instead of silently normalizing persisted input")
        error.getMessage should include ("locale must be canonical")
      }

      "reject an invalid BCP-47 locale without lossy normalization" in {
        Given("a native record with an empty BCP-47 locale subtag")
        val records = Vector(_native("development-process/example", _variants("en--US", _external_published)))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading rejects the malformed tag instead of resolving another locale")
        error.getMessage should include ("Invalid article-media locale")
      }

      "reject whitespace around a persisted locale tag" in {
        Given("a native record with whitespace around an otherwise valid locale")
        val records = Vector(_native("development-process/example", _variants(" en-US ", _external_published)))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading rejects the lossy whitespace transformation")
        error.getMessage should include ("Invalid article-media locale")
      }

      "reject an invalid published external watch URL" in {
        Given("a published external video with a relative watch URL")
        val records = Vector(_native("development-process/example", _variants("en", _external_published.replace("https://example.com/watch", "/relative"))))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading requires an absolute watch URL")
        error.getMessage should include ("watch_url")
        error.getMessage should include ("must be an absolute URI")
      }

      "reject an invalid native infographic public path" in {
        Given("an infographic with an external rather than site-visible path")
        val records = Vector(_native("development-process/example", _variants("en", """{"infographic":{"public_path":"https://example.com/image.png"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading requires a site-visible infographic path")
        error.getMessage should include ("infographic public_path")
        error.getMessage should include ("must be a site-visible path")
      }

      "reject an invalid published site-hosted content URL" in {
        Given("a published site-hosted video with an external content URL")
        val records = Vector(_native("development-process/example", _variants("en", """{"video":{"presentation":"site-hosted","status":"published","content_url":"https://example.com/video.mp4"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading requires a site-visible content URL")
        error.getMessage should include ("content_url")
        error.getMessage should include ("must be a site-visible path")
      }

      "reject an unsupported native video presentation" in {
        Given("a native record with an unsupported video presentation")
        val records = Vector(_native("development-process/example", _variants("en", _external_published.replace("external-link", "embedded"))))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("the presentation enum boundary is rejected")
        error.getMessage should include ("video presentation")
      }

      "reject an unsupported native video status" in {
        Given("a native record with an unsupported video status")
        val records = Vector(_native("development-process/example", _variants("en", _external_published.replace("published", "scheduled"))))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("the status enum boundary is rejected")
        error.getMessage should include ("video status")
      }

      "reject a published video without its required URL" in {
        Given("a published site-hosted video without content_url")
        val records = Vector(_native("development-process/example", _variants("en", """{"video":{"presentation":"site-hosted","status":"published"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading rejects the incomplete published video")
        error.getMessage should include ("requires content_url")
      }

      "reject duplicate normalized article and locale variants" in {
        Given("two records with the same normalized article identity and locale")
        val records = Vector(
          _native("development-process/example", _variants("en", _external_published)),
          _native("development-process/example", _variants("en", _external_published))
        )

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("the duplicate is a metadata-load failure")
        error.getMessage should include ("Duplicate article-media publication variant")
      }

      "retain an infographic when a valid video is unavailable" in {
        Given("draft and withdrawn videos with an independent infographic")
        val metadata = _load_bundle(Vector(_native("development-process/example", """{
          |"en":{"infographic":{"public_path":"/images/example.png"},"video":{"presentation":"external-link","status":"draft"}},
          |"ja":{"video":{"presentation":"site-hosted","status":"withdrawn"}}
          |}""".stripMargin)))

        When("the localized variants are resolved")
        val en = metadata.resolveArticleMedia("development-process/example", "en").get
        val ja = metadata.resolveArticleMedia("development-process/example", "ja").get

        Then("the infographic remains available and no unavailable video projects")
        en.infographic.map(_.publicPath.toString) shouldBe Some("/images/example.png")
        en.projectableVideo shouldBe empty
        ja.projectableVideo shouldBe empty
      }
    }

    "normalized resolution" which {
      "normalize lookup paths while requiring an exact locale" in {
        Given("one canonical localized native record")
        val metadata = _load_bundle(Vector(_native("concepts/tutorial", _variants("en", _external_published))))

        When("equivalent separators and locale casing are supplied to the resolver")
        val normalized = metadata.resolveArticleMedia("concepts\\tutorial", "EN")
        val absent = metadata.resolveArticleMedia("concepts/tutorial", "en-US")

        Then("identity is normalized but a different locale does not fall back")
        normalized.flatMap(_.projectableVideo) should not be empty
        absent shouldBe empty
      }

      "keep path normalization stable for arbitrary article terminal segments" in {
        val segmentgenerator = Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString)

        forAll(segmentgenerator) { segment =>
          Given("a registry for one generated nonempty article terminal segment")
          val registry = _load_bundle(Vector(_native(s"concepts/$segment", _variants("en", _external_published))))

          When("the same identity is resolved with a backslash separator")
          val resolved = registry.resolveArticleMedia(s"concepts\\$segment", "en")

          Then("the resolver finds exactly the normalized article identity")
          resolved should not be empty
        }
      }
    }

    "VideoPublication compatibility" which {
      "adapt an index source package as locale-neutral site-hosted media" in {
        Given("the established .video source-package publication fixture")
        val metadata = PublishMetadata.load(new File("src/test/resources/video-publication-fixture")).get

        When("ordinary article media is resolved in two locales")
        val en = metadata.resolveArticleMedia("concepts/tutorial", "en").get
        val ja = metadata.resolveArticleMedia("concepts/tutorial", "ja").get

        Then("the index source package derives one locale-neutral site-hosted candidate")
        en.video.map(_.presentation) shouldBe Some(VideoPresentation.SiteHosted)
        en.video.map(_.status) shouldBe Some(VideoStatus.Published)
        en.projectableVideo.flatMap(_.contentUrl).map(_.toString) shouldBe Some("/repository/video/tutorial/0.1.0/tutorial-0.1.0.mp4")
        ja.projectableVideo.flatMap(_.contentUrl).map(_.toString) shouldBe en.projectableVideo.flatMap(_.contentUrl).map(_.toString)
      }

      "adapt a non-index path by stripping exactly one final dox suffix" in {
        Given("one valid non-index legacy video publication")
        val metadata = _load_bundle(Vector(_video_publication("plain", "concepts/plain.video", "concepts/plain.dox", "/repository/videos/plain.mp4")))

        When("the ordinary article identity is resolved")
        val variant = metadata.resolveArticleMedia("concepts/plain", "en")

        Then("the compatibility candidate uses publicPath as contentUrl")
        variant.flatMap(_.projectableVideo).flatMap(_.contentUrl).map(_.toString) shouldBe Some("/repository/videos/plain.mp4")
      }

      "prefer an exact native variant and suppress defective compatibility candidates" in {
        Given("one native record, two conflicting legacy records, and one invalid legacy path")
        val native = _native("concepts/tutorial", _variants("en", """{"infographic":{"public_path":"/images/native.png"},"video":{"presentation":"external-link","status":"published","watch_url":"https://example.com/native"}}"""))
        val nonindex = _video_publication("tutorial", "concepts/tutorial.video", "concepts/tutorial.dox", "/repository/videos/tutorial.mp4")
        val duplicate = _video_publication("tutorial-duplicate", "concepts/tutorial.video", "concepts/tutorial.dox", "/repository/videos/tutorial-duplicate.mp4")
        val invalid = _video_publication("broken", "concepts/broken.video", "index.dox", "not-a-site-path")
        val metadata = _load_bundle(Vector(native, nonindex, duplicate, invalid))

        When("article media and compatibility diagnostics are resolved")
        val resolved = metadata.resolveArticleMedia("concepts/tutorial", "en").get
        val diagnostics = metadata.articleMedia.diagnostics.map(_.code)

        Then("the complete native variant wins without merging and only adapters are suppressed")
        resolved.infographic.map(_.publicPath.toString) shouldBe Some("/images/native.png")
        resolved.video.map(_.presentation) shouldBe Some(VideoPresentation.ExternalLink)
        metadata.resolveArticleMedia("concepts/tutorial", "ja") shouldBe empty
        diagnostics should contain ("article-media.video-publication-conflict")
        diagnostics should contain ("article-media.video-publication-invalid-content-url")
        metadata.resolveArticleMedia("concepts/broken", "en") shouldBe empty
        metadata.videoPublications.map(_.publicPath) should contain ("not-a-site-path")
      }
    }
  }

  private def _load_bundle(metadata: Vector[String]): PublishMetadata = {
    val directory = Files.createTempDirectory("smartdox-article-media-publication")
    try {
      Files.write(directory.resolve("publication.json"), _bundle(metadata).getBytes(StandardCharsets.UTF_8))
      PublishMetadata.load(directory.toFile).getOrElse(fail("Article-media test registry was not loaded"))
    } finally {
      _delete_tree(directory.toFile)
    }
  }

  private def _bundle(metadata: Vector[String]): String = {
    val entries = metadata.zipWithIndex.map {
      case (json, index) =>
        s"""{"path":"metadata/article-media/$index.json","metadata":$json}"""
    }.mkString(",")
    s"""{"type":"publication-bundle","entries":[$entries]}"""
  }

  private def _native(identity: String, variants: String): String =
    s"""{"type":"article-media-publication","article":{"identity":"$identity"},"variants":$variants}"""

  private def _variants(locale: String, value: String): String =
    s"""{"$locale":$value}"""

  private def _video_publication(name: String, sourcepackage: String, articlepath: String, publicpath: String): String =
    s"""{"type":"video-publication","video":{"name":"$name","sourcePackage":"$sourcepackage","articlePath":"$articlepath","artifact":{"repositoryPublicPath":"$publicpath"}}}"""

  private val _external_published =
    """{"video":{"presentation":"external-link","status":"published","watch_url":"https://example.com/watch"}}"""

  private def _delete_tree(file: File): Unit = {
    Option(file.listFiles).toVector.flatten.foreach(_delete_tree)
    file.delete()
  }
}
