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
import org.goldenport.i18n.I18NContext
import org.smartdox.metadata.PublishMetadata.{VideoPresentation, VideoStatus}
import org.smartdox.semanticweb.Rdf

/*
 * @since   Aug.  4, 2026
 *  version Aug. 29, 2026
 * @version Sep.  8, 2026
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

        Then("external and site-hosted published video retain independent infographic and PDF role data")
        en.infographic.map(_.publicPath.toString) shouldBe Some("/en/development-process/images/example/video-summary-en.png")
        en.articlePdf.map(_.publicPath.toString) shouldBe Some("/en/development-process/pdf/example-article-en.pdf")
        en.articlePdf.map(_.mediaType) shouldBe Some("application/pdf")
        en.articlePdf.flatMap(_.label) shouldBe Some("Article PDF")
        en.summarySlidesPdf shouldBe empty
        en.video.map(_.presentation) shouldBe Some(VideoPresentation.ExternalLink)
        en.projectableVideo.flatMap(_.watchUrl).map(_.toString) shouldBe Some("https://youtu.be/example-en")
        ja.infographic shouldBe empty
        ja.articlePdf shouldBe empty
        ja.summarySlidesPdf.map(_.publicPath.toString) shouldBe Some("/ja/development-process/pdf/example-summary-ja.pdf")
        ja.summarySlidesPdf.map(_.mediaType) shouldBe Some("application/pdf")
        ja.summarySlidesPdf.flatMap(_.label) shouldBe Some("要約スライド PDF")
        ja.video.map(_.presentation) shouldBe Some(VideoPresentation.SiteHosted)
        ja.projectableVideo.flatMap(_.contentUrl).map(_.toString) shouldBe Some("/ja/development-process/videos/example.mp4")
      }

      "keep PDF roles independent across exact locale variants" in {
        Given("one article whose English variant has only an article PDF and whose Japanese variant has only summary slides")
        val metadata = _load_bundle(Vector(_native("development-process/pdf-example", """{
          |"en":{"article_pdf":{"public_path":"/pdf/article-en.pdf","media_type":"application/pdf"}},
          |"ja":{"summary_slides_pdf":{"public_path":"/pdf/summary-ja.pdf","media_type":"application/pdf"}}
          |}""".stripMargin)))

        When("the exact English, Japanese, and absent Japanese-region variants are resolved")
        val en = metadata.resolveArticleMedia("development-process/pdf-example", "en").get
        val ja = metadata.resolveArticleMedia("development-process/pdf-example", "ja").get
        val absent = metadata.resolveArticleMedia("development-process/pdf-example", "ja-JP")

        Then("each role remains attached only to its direct locale field without role or locale fallback")
        en.articlePdf.map(_.publicPath.toString) shouldBe Some("/pdf/article-en.pdf")
        en.summarySlidesPdf shouldBe empty
        ja.articlePdf shouldBe empty
        ja.summarySlidesPdf.map(_.publicPath.toString) shouldBe Some("/pdf/summary-ja.pdf")
        absent shouldBe empty
      }

      "reject an article PDF with an invalid site-visible path" in {
        Given("an article PDF whose public path is an external URI")
        val records = Vector(_native("development-process/pdf-example", _variants("en", """{"article_pdf":{"public_path":"https://example.com/article.pdf","media_type":"application/pdf"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading requires a site-visible PDF path")
        error.getMessage should include ("article_pdf public_path")
        error.getMessage should include ("must be a site-visible path")
      }

      "reject a summary-slides PDF with a non-PDF media type" in {
        Given("a summary-slides PDF whose declared media type is not application/pdf")
        val records = Vector(_native("development-process/pdf-example", _variants("en", """{"summary_slides_pdf":{"public_path":"/pdf/summary.pdf","media_type":"application/octet-stream"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading requires the exact PDF media type")
        error.getMessage should include ("summary_slides_pdf media_type")
        error.getMessage should include ("application/pdf")
      }

      "reject an article PDF without its required media type" in {
        Given("an article PDF that omits the required media_type field")
        val records = Vector(_native("development-process/pdf-example", _variants("en", """{"article_pdf":{"public_path":"/pdf/article.pdf"}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading identifies the missing PDF media type")
        error.getMessage should include ("article_pdf media_type")
      }

      "reject a PDF label that is present but blank" in {
        Given("an article PDF with a whitespace-only optional label")
        val records = Vector(_native("development-process/pdf-example", _variants("en", """{"article_pdf":{"public_path":"/pdf/article.pdf","media_type":"application/pdf","label":"  "}}""")))

        When("the registry is loaded")
        val error = intercept[IllegalArgumentException](_load_bundle(records))

        Then("metadata loading rejects a supplied label without visible text")
        error.getMessage should include ("article_pdf label")
        error.getMessage should include ("nonblank")
      }

      "preserve a nonblank PDF label verbatim" in {
        Given("an article PDF with a nonblank label that has leading and trailing whitespace")
        val metadata = _load_bundle(Vector(_native("development-process/pdf-example", _variants("en", """{"article_pdf":{"public_path":"/pdf/article.pdf","media_type":"application/pdf","label":"  Read the full article  "}}"""))))

        When("the registry is loaded and its English article PDF is resolved")
        val articlepdf = metadata.resolveArticleMedia("development-process/pdf-example", "en").flatMap(_.articlePdf)

        Then("the supplied label is retained exactly after nonblank validation")
        articlepdf.flatMap(_.label) shouldBe Some("  Read the full article  ")
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
      "derive identities for Document Project entrypoints" in {
        Given("root and nested source Dox entrypoints, their generated HTML entrypoints, and ordinary article paths")
        When("the source paths are converted to registry identities")
        val sourceentrypoint = PublishMetadata.sourcePathToArticleIdentity("development-process/domain-modeling.dox/index.dox")
        val generatedentrypoint = PublishMetadata.sourcePathToArticleIdentity("development-process/domain-modeling.dox/index.html")
        val rootsourceentrypoint = PublishMetadata.sourcePathToArticleIdentity("guide.dox/index.dox")
        val rootgeneratedentrypoint = PublishMetadata.sourcePathToArticleIdentity("guide.dox/index.html")
        val ordinaryarticle = PublishMetadata.sourcePathToArticleIdentity("development-process/literate-modeling.dox")
        val ordinarydirectoryindex = PublishMetadata.sourcePathToArticleIdentity("development-process/domain-modeling/index.dox")

        Then("only root or nested Document Project entrypoints remove their project directory and ordinary terminal paths remain stable")
        sourceentrypoint shouldBe Some("development-process/domain-modeling")
        generatedentrypoint shouldBe Some("development-process/domain-modeling")
        rootsourceentrypoint shouldBe Some("guide")
        rootgeneratedentrypoint shouldBe Some("guide")
        ordinaryarticle shouldBe Some("development-process/literate-modeling")
        ordinarydirectoryindex shouldBe Some("development-process/domain-modeling/index")
      }

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

  "PublishMetadata loading and projection" should {
    "select metadata sources" which {
      "load recognized standalone metadata when no bundle is present" in {
        val directory = Files.createTempDirectory("smartdox-publish-metadata-standalone")
        try {
          Given("a directory containing one recognized standalone catalog metadata file")
          Files.createDirectories(directory.resolve("catalog"))
          Files.write(
            directory.resolve("catalog/standalone.json"),
            """{"type":"publication","publication":{"name":"standalone","title":"Standalone"}}""".getBytes(StandardCharsets.UTF_8)
          )

          When("publication metadata is loaded without a publication bundle")
          val metadata = PublishMetadata.load(directory.toFile).get

          Then("the standalone entry is selected with its source path and identity")
          metadata.entries.map(_.path) shouldBe Vector("catalog/standalone.json")
          metadata.entries.map(_.identity) shouldBe Vector("standalone")
        } finally {
          _delete_tree(directory.toFile)
        }
      }

      "prefer a publication bundle when standalone metadata is also present" in {
        val directory = Files.createTempDirectory("smartdox-publish-metadata-bundle")
        try {
          Given("a directory containing standalone metadata and a publication bundle")
          Files.createDirectories(directory.resolve("catalog"))
          Files.write(
            directory.resolve("catalog/standalone.json"),
            """{"type":"publication","publication":{"name":"standalone"}}""".getBytes(StandardCharsets.UTF_8)
          )
          _write_bundle(directory, Vector(
            "metadata/catalog/bundled.json" -> """{"type":"publication","publication":{"name":"bundled"}}"""
          ))

          When("publication metadata is loaded")
          val metadata = PublishMetadata.load(directory.toFile).get

          Then("only the bundle entry participates in the loaded registry")
          metadata.entries.map(_.path) shouldBe Vector("metadata/catalog/bundled.json")
          metadata.entries.map(_.identity) shouldBe Vector("bundled")
        } finally {
          _delete_tree(directory.toFile)
        }
      }
    }

    "project the publication catalog" which {
      "retain the established catalog and group page paths" in {
        Given("a publication bundle containing one named publication")
        val metadata = _load_bundle_entries(Vector(
          "metadata/catalog/tutorial.json" -> """{"type":"publication","publication":{"name":"tutorial","title":"Tutorial","summary":"A tutorial"},"version":"1.0.0"}"""
        ))

        When("the loaded metadata generates publication pages")
        val pagepaths = metadata.generatedPages.map(_._1)

        Then("the global catalog and publication group pages remain observable")
        pagepaths should contain allOf (
          "catalog/index.dox",
          "repository/tutorial/index.dox",
          "catalog/tutorial/index.dox",
          "catalog/tutorial/metadata.dox"
        )
      }

      "expose bundle paths and payloads through the public realm" in {
        val directory = Files.createTempDirectory("smartdox-publish-metadata-realm")
        try {
          Given("a publication bundle containing one catalog metadata entry")
          _write_bundle(directory, Vector(
            "metadata/catalog/tutorial.json" -> """{"type":"publication","publication":{"name":"tutorial"}}"""
          ))

          When("the bundle is projected to the public realm")
          val realm = PublishMetadata.publicRealm(Some(directory.toFile)).get
          implicit val i18ncontext: I18NContext = I18NContext.default
          val payload = realm.getString("metadata/catalog/tutorial.json")

          Then("the bundle path is present with its serialized metadata payload")
          payload should not be empty
          payload.get should include ("\"type\" : \"publication\"")
          payload.get should include ("\"name\" : \"tutorial\"")
        } finally {
          _delete_tree(directory.toFile)
        }
      }
    }

    "merge configured RDF artifacts" which {
      "parse a matching Turtle artifact only when merging is enabled" in {
        val publication = Files.createTempDirectory("smartdox-publish-metadata-rdf")
        val repository = Files.createTempDirectory("smartdox-publish-metadata-repository")
        try {
          Given("a publication bundle and repository containing its registered Turtle artifact")
          _write_bundle(publication, _rdf_publication_entries)
          val turtle = repository.resolve("repository/video/tutorial/0.1.0/tutorial.ttl")
          Files.createDirectories(turtle.getParent)
          Files.write(turtle, """<https://example.com/video/tutorial> <https://schema.org/name> "Tutorial".""".getBytes(StandardCharsets.UTF_8))
          val metadata = PublishMetadata.load(publication.toFile).get

          When("RDF artifact triples are requested with merge enabled and disabled")
          val merged = metadata.videoRdfArtifactTriples(PublishMetadata.RdfMergeConfig(Some(repository.toFile), "fail"))
          val disabled = metadata.videoRdfArtifactTriples(PublishMetadata.RdfMergeConfig(Some(repository.toFile), "fail", mergePublicationArtifacts = false))

          Then("the configured artifact is parsed and disabling the merge returns no triples")
          merged should contain (Rdf.Triple(
            Rdf.Node.Uri("https://example.com/video/tutorial"),
            Rdf.Node.Uri("https://schema.org/name"),
            Rdf.Node.Literal("Tutorial")
          ))
          disabled shouldBe empty
        } finally {
          _delete_tree(publication.toFile)
          _delete_tree(repository.toFile)
        }
      }

      "apply the configured missing-artifact policy" in {
        val publication = Files.createTempDirectory("smartdox-publish-metadata-rdf-missing")
        val repository = Files.createTempDirectory("smartdox-publish-metadata-rdf-empty")
        try {
          Given("a publication bundle whose registered Turtle artifact is absent from the repository")
          _write_bundle(publication, _rdf_publication_entries)
          val metadata = PublishMetadata.load(publication.toFile).get

          When("warn and fail missing-artifact policies are applied")
          val warned = metadata.videoRdfArtifactTriples(PublishMetadata.RdfMergeConfig(Some(repository.toFile), "warn"))
          val failed = intercept[IllegalArgumentException] {
            metadata.videoRdfArtifactTriples(PublishMetadata.RdfMergeConfig(Some(repository.toFile), "fail"))
          }

          Then("warn remains reference-only and fail reports the missing artifact")
          warned shouldBe empty
          failed.getMessage should include ("Missing RDF artifact")
        } finally {
          _delete_tree(publication.toFile)
          _delete_tree(repository.toFile)
        }
      }
    }
  }

  private def _load_bundle(metadata: Vector[String]): PublishMetadata = {
    val directory = Files.createTempDirectory("smartdox-article-media-publication")
    try {
      _write_bundle(directory, metadata.zipWithIndex.map {
        case (json, index) => s"metadata/article-media/$index.json" -> json
      })
      PublishMetadata.load(directory.toFile).getOrElse(fail("Article-media test registry was not loaded"))
    } finally {
      _delete_tree(directory.toFile)
    }
  }

  private def _load_bundle_entries(entries: Vector[(String, String)]): PublishMetadata = {
    val directory = Files.createTempDirectory("smartdox-publication-bundle")
    try {
      _write_bundle(directory, entries)
      PublishMetadata.load(directory.toFile).getOrElse(fail("Publication metadata test registry was not loaded"))
    } finally {
      _delete_tree(directory.toFile)
    }
  }

  private def _bundle(metadata: Vector[String]): String = {
    _bundle_entries(metadata.zipWithIndex.map {
      case (json, index) => s"metadata/article-media/$index.json" -> json
    })
  }

  private def _bundle_entries(entries: Vector[(String, String)]): String = {
    val values = entries.map {
      case (path, json) => s"""{"path":"$path","metadata":$json}"""
    }.mkString(",")
    s"""{"type":"publication-bundle","entries":[$values]}"""
  }

  private def _write_bundle(directory: java.nio.file.Path, entries: Vector[(String, String)]): Unit = {
    Files.write(directory.resolve("publication.json"), _bundle_entries(entries).getBytes(StandardCharsets.UTF_8))
  }

  private def _native(identity: String, variants: String): String =
    s"""{"type":"article-media-publication","article":{"identity":"$identity"},"variants":$variants}"""

  private def _variants(locale: String, value: String): String =
    s"""{"$locale":$value}"""

  private def _video_publication(name: String, sourcepackage: String, articlepath: String, publicpath: String): String =
    s"""{"type":"video-publication","video":{"name":"$name","sourcePackage":"$sourcepackage","articlePath":"$articlepath","artifact":{"repositoryPublicPath":"$publicpath"}}}"""

  private val _rdf_publication_entries = Vector(
    "metadata/videos/tutorial.json" -> """{"type":"video-publication","video":{"name":"tutorial","version":"0.1.0","artifact":{"repositoryPublicPath":"repository/video/tutorial/0.1.0/tutorial.mp4"}}}""",
    "metadata/video/tutorial/0.1.0/rdf.json" -> """{"type":"video-rdf","video":{"name":"tutorial","version":"0.1.0"},"files":{"turtle":{"warehousePath":"repository/video/tutorial/0.1.0/tutorial.ttl","publicPath":"repository/video/tutorial/0.1.0/tutorial.ttl"}}}"""
  )

  private val _external_published =
    """{"video":{"presentation":"external-link","status":"published","watch_url":"https://example.com/watch"}}"""

  private def _delete_tree(file: File): Unit = {
    Option(file.listFiles).toVector.flatten.foreach(_delete_tree)
    file.delete()
  }
}
