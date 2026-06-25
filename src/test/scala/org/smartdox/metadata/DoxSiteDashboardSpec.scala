package org.smartdox.metadata

import java.net.URI
import org.joda.time.LocalDate
import com.typesafe.config.ConfigFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.GivenWhenThen
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NString
import org.smartdox.SmartDoxSpecVocabulary
import org.smartdox.metadata.History.{ContentKind, EventKind}
import org.smartdox.metadata.Notices.Notice
import org.smartdox.semanticweb.Site.SiteModel
import org.smartdox.semanticweb.Rdf

/*
 * @since   Jun.  4, 2026
 * @version Jun. 25, 2026
 * @author  ASAMI, Tomoharu
 */
class DoxSiteDashboardSpec extends AnyWordSpec with Matchers with GivenWhenThen with SmartDoxSpecVocabulary {
  "DoxSiteDashboard" should {
    "count articles and created increments deterministically" in {
      Given("metadata contains article, blog, glossary, history, and news notices with created dates")
      val architecture = _category("architecture", "Architecture")
      val implementation = _category("implementation", "Implementation")
      val historycategory = _category("history", "History")
      val glossarycategory = _category("glossary", "Glossary")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01")),
        _notice("Architecture Blog", "architecture/blog.html", architecture, DocumentMetaData.Kind.Blog, Some("2026-01-02")),
        _notice("Implementation One", "implementation/one.html", implementation, DocumentMetaData.Kind.Article, Some("2026-02-15")),
        _notice("Architecture Term", "glossary/architecture/term.html", glossarycategory, DocumentMetaData.Kind.Article, Some("2026-01-02")),
        _notice("History Page", "history/index.html", historycategory, DocumentMetaData.Kind.Article, Some("2026-02-14")),
        _notice("Glossary Page", "glossary/index.html", glossarycategory, DocumentMetaData.Kind.Article, Some("2026-02-14")),
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
          "history" -> historycategory,
          "glossary" -> glossarycategory
        )),
        notices = notices,
        history = history
      )

      When("SmartDox creates the dashboard summary")
      val dashboard = DoxSiteDashboard.create(meta)

      Then("special categories are excluded from public dashboard category counts")
      dashboard.counts.categoryCount shouldBe 2
      dashboard.categories.map(_.name) should not contain "history"
      dashboard.categories.map(_.name) should not contain "glossary"
      And("article, total item, and RDF counters remain deterministic")
      dashboard.counts.articleCount shouldBe 3
      dashboard.counts.totalItemCount shouldBe 3
      dashboard.rdf.tripleCount shouldBe 0
      And("created increments count article and glossary additions without update events")
      dashboard.increments.scale shouldBe "week"
      dashboard.increments.buckets.map(_.count).sum shouldBe 4
      dashboard.increments.buckets.map(_.articleCount).sum shouldBe 3
      dashboard.increments.buckets.map(_.glossaryTermCount).sum shouldBe 1
      And("category-local counts preserve article and glossary term increments")
      dashboard.categories.find(_.name == "architecture").get.counts.articleCount shouldBe 2
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.count).sum shouldBe 3
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.articleCount).sum shouldBe 2
      dashboard.categories.find(_.name == "architecture").get.increments.buckets.map(_.glossaryTermCount).sum shouldBe 1
    }

    "include RDF summary after site model construction" in {
      Given("metadata has a category notice that can be projected into the site model")
      val architecture = _category("architecture", "Architecture")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      ))
      val meta = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = notices
      )
      val site = SiteModel.create(meta, notices.notices.map(_.toSiteResource))

      When("the dashboard is created after RDF site model construction")
      val dashboard = DoxSiteDashboard.create(meta.copy(site = site))

      Then("the site-level RDF counters reflect generated RDF resources")
      dashboard.rdf.resourceCount shouldBe 1
      dashboard.rdf.tripleCount should be > 0
      dashboard.rdf.subjectCount should be > 0
      dashboard.rdf.predicateCount should be > 0
      And("the category-local RDF counters are also populated")
      val category = dashboard.categories.find(_.name == "architecture").get
      category.rdf.tripleCount should be > 0
      category.rdf.subjectCount should be > 0
      category.rdf.predicateCount should be > 0
    }

    "emit deterministic graph metadata for the RDF viewer" in {
      Given("metadata and site RDF contain one architecture article resource")
      val architecture = _category("architecture", "Architecture")
      val notices = Notices(Vector(
        _notice("Architecture One", "architecture/one.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      ))
      val meta = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = notices
      )
      val site = SiteModel.create(meta, notices.notices.map(_.toSiteResource))

      When("SmartDox serializes the RDF graph handoff metadata")
      val json = DoxSiteDashboard.toRdfGraphJsonString(meta.copy(site = site))

      Then("the graph metadata contains node and edge collections")
      json should include_metadata(""""nodes"""")
      json should include_metadata(""""edges"""")
      And("category and resource identity are kept for Cozy graph navigation")
      json should include_metadata(""""category" : "architecture"""")
      json should include_metadata("https://www.simplemodeling.org/architecture/one")
    }

    "emit glossary term metadata for term hubs" in {
      Given("a glossary term with event metadata, adjacent article references, and video RDF references")
      val architecture = _category("architecture", "Architecture")
      val termnotice = _notice("Runtime", "glossary/architecture/runtime.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-01"))
      val articlenotice = _notice("Runtime Article", "architecture/runtime-article.html", architecture, DocumentMetaData.Kind.Article, Some("2026-01-02"))
      val termmetadata = termnotice.metadata.copy(properties = Some(ConfigFactory.parseString(
        """term_type = event
          |event.occurred_at = "2026-06-25"
          |event.location = "KnowledgeHub"
          |event.actors = ["architecture:knowledge-owner"]
          |event.roles = ["architecture:reviewer"]
          |event.participants = ["architecture:knowledge-owner", "architecture:reviewer"]
          |event.scenarios = ["scenario:review"]
          |event.evidence = ["bib:design-patterns"]
          |event.cml.event = "knowledge.published"
          |event.cml.component = "knowledgehub"
          |event.cml.statemachine = "KnowledgeItemLifecycle"
          |""".stripMargin)))
      val glossarydefinition = Glossary.Definition.InGlossary(
        Glossary.Definition.Ingredients(
          Glossary.Term.make("Runtime"),
          new URI("glossary/architecture/runtime.html"),
          org.smartdox.Paragraph(List(org.smartdox.Text("Runtime definition.")))
        ),
        null,
        termmetadata
      )
      val meta0 = MetaData(
        categories = CategoryCollection(VectorMap("architecture" -> architecture)),
        notices = Notices(Vector(articlenotice)),
        glossary = Glossary(Vector(glossarydefinition))
      )
      val termresource = glossarydefinition.toSiteResource.id
      val articleresource = articlenotice.toSiteResource.id
      val videoresource = "https://www.simplemodeling.org/repository/video/tutorial/0.1.0/runtime.mp4"
      val publicationtriples = Vector(
        Rdf.Triple(Rdf.Node.Uri(termresource), Rdf.Node.Uri("https://schema.org/about"), Rdf.Node.Uri(articleresource)),
        Rdf.Triple(Rdf.Node.Uri(termresource), Rdf.Node.Uri("https://schema.org/video"), Rdf.Node.Uri(videoresource))
      )
      val site = SiteModel.create(meta0, Vector(glossarydefinition.toSiteResource, articlenotice.toSiteResource), publicationTriples = publicationtriples)
      val meta = meta0.copy(site = site)

      When("SmartDox renders the glossary term index and RDF graph metadata")
      val terms = DoxSiteDashboard.toGlossaryTermsJsonString(meta)
      val graph = DoxSiteDashboard.toRdfGraphJsonString(meta)

      Then("the term index preserves term identity, definition, and event-specific metadata")
      terms should include_metadata(""""terms"""")
      terms should include_metadata(""""id" : "architecture:runtime"""")
      terms should include_metadata(""""public_path" : "glossary/architecture/runtime.html"""")
      terms should include_metadata("Runtime definition")
      terms should include_metadata(""""term_type" : "event"""")
      terms should include_metadata(""""occurred_at" : "2026-06-25"""")
      terms should include_metadata(""""actors"""")
      terms should include_metadata("architecture:knowledge-owner")
      terms should include_metadata("knowledge.published")
      And("adjacent article and video references remain connected to the term")
      terms should include_metadata(""""article_refs"""")
      terms should include_metadata(""""path" : "architecture/runtime-article.html"""")
      terms should include_metadata(""""video_refs"""")
      terms should include_metadata("runtime.mp4")
      terms should include_metadata(""""rdf_refs"""")
      terms should include_metadata(""""unreferenced" : false""")
      And("the graph metadata keeps the linked term, article, and video resources")
      graph should include_metadata(""""terms"""")
      graph should include_metadata("architecture:runtime")
    }

    "choose month buckets for long spans" in {
      Given("created events span more than the weekly dashboard window")
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

      When("SmartDox creates the dashboard increments")
      val dashboard = DoxSiteDashboard.create(meta)

      Then("the increment scale switches to month buckets")
      dashboard.increments.scale shouldBe "month"
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

  private def _slot(kind: EventKind, date: String, contentkind: ContentKind, notice: Notice): History.Slot =
    History.Slot(kind, new LocalDate(date), contentkind, notice, None)
}
