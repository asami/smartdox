package org.smartdox.metadata

import java.net.URI
import org.joda.time.LocalDate
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NString
import org.smartdox.metadata.History.{ContentKind, EventKind}
import org.smartdox.metadata.Notices.Notice
import org.smartdox.semanticweb.Site.SiteModel
import org.smartdox.semanticweb.Rdf

/*
 * @since   Jun.  4, 2026
 * @version Jun. 22, 2026
 * @author  ASAMI, Tomoharu
 */
class DoxSiteDashboardSpec extends AnyWordSpec with Matchers {
  "DoxSiteDashboard" should {
    "count articles and created increments deterministically" in {
      val architecture = _category("architecture", "Architecture")
      val implementation = _category("implementation", "Implementation")
      val historyCategory = _category("history", "History")
      val glossaryCategory = _category("glossary", "Glossary")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01")),
        _notice("Architecture Blog", "architecture/blog.html", architecture, DocumentMetaData.Kind.Blog, Some("2026-01-02")),
        _notice("Implementation One", "implementation/one.html", implementation, DocumentMetaData.Kind.Article, Some("2026-02-15")),
        _notice("Architecture Term", "glossary/architecture/term.html", glossaryCategory, DocumentMetaData.Kind.Article, Some("2026-01-02")),
        _notice("History Page", "history/index.html", historyCategory, DocumentMetaData.Kind.Article, Some("2026-02-14")),
        _notice("Glossary Page", "glossary/index.html", glossaryCategory, DocumentMetaData.Kind.Article, Some("2026-02-14")),
        _notice("News", "news.html", architecture, DocumentMetaData.Kind.News, Some("2026-02-16"))
      ))
      val history = History(Vector(
        _slot(EventKind.Created, "2026-01-01", ContentKind.Article, notices.notices(0)),
        _slot(EventKind.Updated, "2026-01-03", ContentKind.Article, notices.notices(0)),
        _slot(EventKind.Created, "2026-01-02", ContentKind.Article, notices.notices(1)),
        _slot(EventKind.Created, "2026-01-02", ContentKind.Glossary, notices.notices(3)),
        _slot(EventKind.Created, "2026-02-15", ContentKind.Article, notices.notices(2)),
        _slot(EventKind.Created, "2026-02-14", ContentKind.Article, notices.notices(4)),
        _slot(EventKind.Created, "2026-02-14", ContentKind.Article, notices.notices(5)),
        _slot(EventKind.Created, "2026-02-16", ContentKind.Article, notices.notices(6))
      ))
      val meta = MetaData(
        categories = CategoryCollection(VectorMap(
          "architecture" -> architecture,
          "implementation" -> implementation,
          "history" -> historyCategory,
          "glossary" -> glossaryCategory
        )),
        notices = notices,
        history = history
      )

      val dashboard = DoxSiteDashboard.create(meta)

      dashboard.counts.categoryCount shouldBe 2
      dashboard.categories.map(_.name) should not contain "history"
      dashboard.categories.map(_.name) should not contain "glossary"
      dashboard.counts.articleCount shouldBe 3
      dashboard.counts.totalItemCount shouldBe 3
      dashboard.rdf.tripleCount shouldBe 0
      dashboard.increments.scale shouldBe "week"
      dashboard.increments.buckets.map(_.count).sum shouldBe 4
      dashboard.increments.buckets.map(_.articleCount).sum shouldBe 3
      dashboard.increments.buckets.map(_.glossaryTermCount).sum shouldBe 1
      dashboard.categories.find(_.name == "architecture").get.counts.articleCount shouldBe 2
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.count).sum shouldBe 3
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.articleCount).sum shouldBe 2
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.glossaryTermCount).sum shouldBe 1
    }


    "include RDF summary after site model construction" in {
      val architecture = _category("architecture", "Architecture")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      ))
      val meta = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = notices
      )
      val site = SiteModel.create(meta, notices.notices.map(_.toSiteResource))
      val dashboard = DoxSiteDashboard.create(meta.copy(site = site))

      dashboard.rdf.resourceCount shouldBe 1
      dashboard.rdf.tripleCount should be > 0
      dashboard.rdf.subjectCount should be > 0
      dashboard.rdf.predicateCount should be > 0
      val category = dashboard.categories.find(_.name == "architecture").get
      category.rdf.tripleCount should be > 0
      category.rdf.subjectCount should be > 0
      category.rdf.predicateCount should be > 0
    }

    "emit deterministic graph metadata for the RDF viewer" in {
      val architecture = _category("architecture", "Architecture")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      ))
      val meta = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = notices
      )
      val site = SiteModel.create(meta, notices.notices.map(_.toSiteResource))
      val json = DoxSiteDashboard.toRdfGraphJsonString(meta.copy(site = site))

      json should include ("\"nodes\"")
      json should include ("\"edges\"")
      json should include ("\"category\" : \"architecture\"")
      json should include ("https://www.simplemodeling.org/architecture/one")
    }

    "emit glossary term metadata for term hubs" in {
      val architecture = _category("architecture", "Architecture")
      val termNotice = _notice("Runtime", "glossary/architecture/runtime.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      val articleNotice = _notice("Runtime Article", "architecture/runtime-article.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-02"))
      val glossaryDefinition = Glossary.Definition.InGlossary(
        Glossary.Definition.Ingredients(
          Glossary.Term.make("Runtime"),
          new URI("glossary/architecture/runtime.html"),
          org.smartdox.Paragraph(List(org.smartdox.Text("Runtime definition.")))
        ),
        null,
        termNotice.metadata
      )
      val meta0 = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = Notices(Vector(articleNotice)),
        glossary = Glossary(Vector(glossaryDefinition))
      )
      val termResource = glossaryDefinition.toSiteResource.id
      val articleResource = articleNotice.toSiteResource.id
      val videoResource = "https://www.simplemodeling.org/repository/video/tutorial/0.1.0/runtime.mp4"
      val publicationTriples = Vector(
        Rdf.Triple(Rdf.Node.Uri(termResource), Rdf.Node.Uri("https://schema.org/about"), Rdf.Node.Uri(articleResource)),
        Rdf.Triple(Rdf.Node.Uri(termResource), Rdf.Node.Uri("https://schema.org/video"), Rdf.Node.Uri(videoResource))
      )
      val site = SiteModel.create(meta0, Vector(glossaryDefinition.toSiteResource, articleNotice.toSiteResource), publicationTriples = publicationTriples)
      val meta = meta0.copy(site = site)
      val terms = DoxSiteDashboard.toGlossaryTermsJsonString(meta)
      val graph = DoxSiteDashboard.toRdfGraphJsonString(meta)

      terms should include (""""terms"""")
      terms should include (""""id" : "architecture:runtime"""")
      terms should include (""""public_path" : "glossary/architecture/runtime.html"""")
      terms should include ("Runtime definition")
      terms should include (""""article_refs"""")
      terms should include (""""path" : "architecture/runtime-article.html"""")
      terms should include (""""video_refs"""")
      terms should include ("runtime.mp4")
      terms should include (""""rdf_refs"""")
      terms should include ("\"unreferenced\" : false")
      graph should include (""""terms"""")
      graph should include ("architecture:runtime")
    }

    "choose month buckets for long spans" in {
      val category = _category("architecture", "Architecture")
      val notices = Notices(Vector(
        _notice("Old", "architecture/old.html", category, DocumentMetaData.Kind.Article, Some("2025-01-01")),
        _notice("New", "architecture/new.html", category, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      ))
      val history = History(Vector(
        _slot(EventKind.Created, "2025-01-01", ContentKind.Article, notices.notices(0)),
        _slot(EventKind.Created, "2026-01-01", ContentKind.Article, notices.notices(1))
      ))
      val meta = MetaData(categories = CategoryCollection(VectorMap("architecture" -> category)), notices = notices, history = history)

      DoxSiteDashboard.create(meta).increments.scale shouldBe "month"
    }
  }

  private def _category(name: String, title: String): Category =
    Category(
      Category.CategoryName(name),
      Some(Category.CategoryTitle(I18NString(title))),
      new URI(name)
    )

  private def _notice(
    title: String,
    uri: String,
    category: Category,
    kind: DocumentMetaData.Kind,
    published: Option[String]
  ): Notice =
    Notice(
      I18NString(title),
      None,
      Some(category),
      new URI(uri),
      Some(I18NString(title)),
      I18NString(title),
      I18NString(title),
      Nil,
      published.map(new LocalDate(_)),
      DocumentMetaData.UpdateHistory.empty,
      Some(kind),
      Some(DocumentMetaData.Status.Published),
      None,
      DocumentMetaData.empty
    )

  private def _slot(kind: EventKind, date: String, contentKind: ContentKind, notice: Notice): History.Slot =
    History.Slot(kind, new LocalDate(date), contentKind, notice, None)
}
