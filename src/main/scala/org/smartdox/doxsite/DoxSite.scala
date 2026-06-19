package org.smartdox.doxsite

import scalaz.{Tree => ZTree, Category => _, _}, Scalaz._
import scala.util.control.NonFatal
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import java.io.File
import java.net.URI
import java.net.URL
import java.time.Instant
import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.i18n.I18NContext
import org.goldenport.config.ConfigLoader
import org.goldenport.tree.Tree
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeCursor
import org.goldenport.tree.TreeTransformer
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.tree.TreeVisitor
import org.goldenport.tree.ControlTreeNode
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.FileData
import org.goldenport.realm.RealmTransformer
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.collection.NonEmptyVector
import org.goldenport.collection.VectorMap
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.LocaleUtils
import org.goldenport.io.InputSource
import org.goldenport.util.StringUtils
import org.goldenport.util.OptionUtils
import org.goldenport.util.OptionUtils.lastOption
import org.goldenport.util.LocalDateUtils
import org.goldenport.util.InstantUtils.instantOrderingAsc
import org.goldenport.util.RegexUtils
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.PublishMetadata
import org.smartdox.metadata.DoxSiteDashboard
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.Explanation
import org.smartdox.metadata.Glossary
import org.smartdox.semanticweb.Rdf
import org.smartdox.metadata.Bibliography
import org.smartdox.metadata.CategoryCollection
import org.smartdox.metadata.Category
import org.smartdox.metadata.Notices
import org.smartdox.metadata.Notices.Notice
import org.smartdox.metadata.History
import org.smartdox.metadata.KeywordCollection
import org.smartdox.metadata.TagCollection
import org.smartdox.metadata.{AtomFeed, AtomFeedBag}
import org.smartdox.generator.Context
import org.smartdox.service.operations.SiteParameters
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.LanguageFilterTransformer
import org.smartdox.semanticweb._
import org.smartdox.semanticweb.Site.SiteModel
import org.smartdox.semanticweb.Site.SiteMetadata
import GlossaryCollector.PROP_GLOSSARY_DIRECTORY

/*
 * @since   Feb. 23, 2025
 *  version Feb. 25, 2025
 *  version Mar.  9, 2025
 *  version Apr. 29, 2025
 *  version May. 31, 2025
 *  version Jun. 28, 2025
 *  version Jul. 26, 2025
 *  version Aug. 27, 2025
 *  version Sep. 28, 2025
 *  version Oct. 30, 2025
 *  version Nov. 29, 2025
 *  version Dec.  8, 2025
 *  version May. 14, 2026
 * @version Jun. 19, 2026
 * @author  ASAMI, Tomoharu
 */
class DoxSite(
  val config: DoxSite.Config,
  space: Tree[Node],
  val metadata: MetaData
) {
  import DoxSite._

  def toRealm(context: Context): Realm = {
    val targets = List(LocaleUtils.en, LocaleUtils.ja) // TODO
    val realm = targets match {
      case Nil => _build_plain(context)
      case xs => _build_multi(context)
    }
    _build_machine_metadata(realm)
  }

  private def _build_plain(
    context: Context
  ): Realm = {
    val rule = RealmBuilder.Rule(config.outputTreeTransformerConfig)
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
  }

  private def _build_multi(
    context: Context
  ): Realm = {
    val en = _build_locale(context, RealmBuilder.Rule.en.withConfig(config.outputTreeTransformerConfig))
    val ja = _build_locale(context, RealmBuilder.Rule.ja.withConfig(config.outputTreeTransformerConfig))
    val realm = Realm.create()
    val a = realm.merge("en", en)
    val r = a.merge("ja", ja)
    _build_notices(r)
    _build_categories(r)
    _build_atomfeed(r)
    _build_rdf(r)
    r
  }

  private def _build_locale(
    context: Context,
    rule: RealmBuilder.Rule
  ) = {
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
  }

  // private def _build_en(
  //   context: Context
  // ) = {
  //   val rule = RealmBuilder.Rule.en.withConfig(config.outputTreeTransformerConfig)
  //   val a = space.transform(new RealmBuilder(context, rule))
  //   Realm(a)
  // }

  // private def _build_ja(
  //   context: Context
  // ) = {
  //   val rule = RealmBuilder.Rule.ja.withConfig(config.outputTreeTransformerConfig)
  //   val a = space.transform(new RealmBuilder(context, rule))
  //   Realm(a)
  // }

  def traverse(p: TreeVisitor[Node]): Unit = space.traverse(p)

  def traverse(pathname: String, p: TreeVisitor[Node]): Unit =
    space.getNode(pathname) match {
      case Some(s) => s.traverse(p)
      case None => Unit
    }

  private def _build_notices(p: Realm): Realm = {
    import DocumentMetaData.Status

    def _filter_production_ = metadata.notices.take(Status.Published)
    def _filter_full_ = metadata.notices.notices
    def _filter_draft_ = metadata.notices.take(
      Status.Published,
      Status.WorkInProgress,
      Status.Draft
    )
    def _filter_test_ = metadata.notices.take(
      Status.Published,
      Status.WorkInProgress,
      Status.Draft,
      Status.Test
    )

    val xs0 = config.strategy match {
      case Strategy.Production => _filter_production_
      case Strategy.ProductionUpdate => _filter_production_
      case Strategy.ProductionPreview => _filter_production_
      case Strategy.Full => _filter_full_
      case Strategy.WorkInProgress => _filter_draft_
      case Strategy.Draft => _filter_draft_
      case Strategy.Preparation => _filter_draft_
      case Strategy.Overview => _filter_draft_
      case Strategy.Test => _filter_test_
    }
    val xs1 = xs0.sortWith(_compare)
//    val xs = xs1 ++ _make_stub(xs1.length)
    val xs = xs1
    for ((x, i) <- xs.zipWithIndex) {
      val ja = x.yamlString(config.localeSetting.context(LocaleUtils.ja))
      val path = f"WEB-INF/data/notice${i + 1}%02d.yaml"
      p.setContent(path, ja)
      val pathja = f"WEB-INF/data/ja/notice${i + 1}%02d.yaml"
      p.setContent(pathja, ja)
      val en = x.yamlString(config.localeSetting.context(LocaleUtils.en))
      val pathen = f"WEB-INF/data/en/notice${i + 1}%02d.yaml"
      p.setContent(pathen, en)
    }
    val byc: Map[Category, Vector[Notice]] = xs.groupBy(_.category).flatMap {
      case (Some(c), v) => Some(c -> v)
      case (None, _) => None
    }
    for ((c, ns) <- byc) {
      for ((x , i) <- ns.zipWithIndex) {
        val path = c.containerString
        val ja = x.yamlString(config.localeSetting.context(LocaleUtils.ja))
        val pathja = f"WEB-INF/data/ja/${path}/notice${i + 1}%02d.yaml"
        p.setContent(pathja, ja)
        val en = x.yamlString(config.localeSetting.context(LocaleUtils.en))
        val pathen = f"WEB-INF/data/en/${path}/notice${i + 1}%02d.yaml"
        p.setContent(pathen, en)
      }
    }
    p
  }

  private def _build_categories(p: Realm): Realm = {
    for (c <- metadata.categories.categoryVector) {
      val path = c.containerString
      val ja = c.yamlString(config.localeSetting.context(LocaleUtils.ja))
      val pathja = f"WEB-INF/data/ja/${path}/category.yaml"
      p.setContent(pathja, ja)
      val en = c.yamlString(config.localeSetting.context(LocaleUtils.en))
      val pathen = f"WEB-INF/data/en/${path}/category.yaml"
      p.setContent(pathen, en)
    }
    p
  }

  private def _build_atomfeed(p: Realm): Realm = {
    for (x <- metadata.atomFeed) {
      _build_atomfeed_ja(p, x.ja)
      _build_atomfeed_en(p, x.en)
    }
    p
  }

  private def _build_atomfeed_ja(realm: Realm, af: AtomFeed): Realm = {
    _build_atomfeed(realm, "atom.xml", af)
    _build_atomfeed(realm, "ja/atom.xml", af)
  }

  private def _build_atomfeed_en(realm: Realm, af: AtomFeed): Realm = {
    _build_atomfeed(realm, "en/atom.xml", af)
  }

  private def _build_atomfeed(realm: Realm, path: String, af: AtomFeed): Realm = {
    val json = af.toAtomString
    realm.setContent(path, json)
  }

  /*
   * Unused:
   * BokSiteOntology, BokSiteSchema
   * CategorySiteOntology, CategorySiteSchema
   * ProjectSiteOntology, ProjectSiteSchema
   */
  private val _ontology_models: Vector[OntologyModel] = Vector(
    SimpleModelingOrgOntology,
    BokOntology,
    CategoryOntology,
    SimpleModelingOntology,
    SimpleModelOntology,
    DocumentModelOntology,
    GlossaryOntology,
    BibliographyOntology,
    SmartDoxOntology,
    ProjectOntology,
    ComponentRepositoryOntology
  )

  private val _schema_models: Vector[SchemaModel] = Vector(
    SimpleModelingOrgSchema,
    BokSchema,
    CategorySchema,
//    SimpleModelingSchema,
    SimpleModelSchema,
    DocumentModelSchema,
    GlossarySchema,
    BibliographySchema,
//    SmartDoxSchema,
    ProjectSchema,
    ComponentRepositorySchema
  )

  private val _public_ontology_models: Vector[OntologyModel] = Vector(
    SimpleModelingOrgPublicOntology
  )

  private val _public_schema_models: Vector[SchemaModel] = Vector(
    SimpleModelingOrgPublicSchema
  )

  private def _build_rdf(realm: Realm): Realm = {
    val a = _build_rdf_definitions(realm)
    val b = _build_rdf_public(a)
    _build_rdf_site(b)
  }

  private def _build_rdf_definitions(realm: Realm): Realm = {
    val a = _ontology_models.foldLeft(realm)(_build_rdf)
    _schema_models.foldLeft(realm)(_build_rdf)
  }

  private def _build_rdf_public(realm: Realm): Realm = {
    val a = _public_ontology_models.foldLeft(realm)(_build_rdf_public)
    _public_schema_models.foldLeft(realm)(_build_rdf_public)
  }

  private def _build_rdf(
    realm: Realm,
    knowledge: OntologyModel
  ): Realm =
    _build_rdf(realm, knowledge.namespace, Some(knowledge.asJsonLD), Some(knowledge.asTurtle))

  private def _build_rdf(
    realm: Realm,
    knowledge: SchemaModel
  ): Realm =
    _build_rdf(realm, knowledge.namespace, Some(knowledge.asJsonLD), Some(knowledge.asTurtle))

  private def _build_rdf(
    realm: Realm,
    namespace: String,
    jsonld: Option[String],
    turtle: Option[String]
  ): Realm = {
    val path = _path(namespace)
    jsonld.foreach(x => realm.setContent(path("index.jsonld"), x))
    turtle.foreach(x => realm.setContent(path("index.ttl"), x))
    realm
  }

  private def _build_rdf_public(
    realm: Realm,
    knowledge: OntologyModel
  ): Realm = {
    val namespace = knowledge.namespace
    val jsonld = Some(knowledge.asJsonLD)
    val turtle = Some(knowledge.asTurtle)
    _build_rdf_public(realm, namespace, jsonld, turtle)
  }

  private def _build_rdf_public(
    realm: Realm,
    knowledge: SchemaModel
  ): Realm = {
    val namespace = knowledge.namespace
    val jsonld = Some(knowledge.asJsonLD)
    val turtle = Some(knowledge.asTurtle)
    _build_rdf_public(realm, namespace, jsonld, turtle)
  }

  private def _build_rdf_public(
    realm: Realm,
    namespace: String,
    jsonld: Option[String],
    turtle: Option[String]
  ): Realm = {
    val path = _path(namespace)
    jsonld.foreach(x => realm.setContent(path.changeSuffix("jsonld"), x))
    turtle.foreach(x => realm.setContent(path.changeSuffix("ttl"), x))
    realm
  }

  private def _build_rdf_site(realm: Realm): Realm = {
    val jsonld = metadata.site.toJsonLD
    val turtle = metadata.site.toTurtle
    realm.setContent("site.jsonld", jsonld)
    realm.setContent("site.ttl", turtle)
  }

  private def _build_machine_metadata(realm: Realm): Realm = {
    realm.setContent("metadata/dashboard/site.json", DoxSiteDashboard.toJsonString(metadata.dashboard))
    realm
  }

  private def _path(namespace: String): PathName = {
    val a = namespace.takeWhile(_ != '#')
    val uri = URI.create(a)
    PathName(uri.getPath)
  }

  private def _compare(lhs: Notice, rhs: Notice): Boolean =
    config.strategy match {
      case Strategy.WorkInProgress => _compare_draft(lhs, rhs)
      case Strategy.Draft => _compare_draft(lhs, rhs)
      case _ => _compare_default(lhs, rhs)
    }

  private def _compare_default(lhs: Notice, rhs: Notice): Boolean = (
    _compare_status_option(lhs, rhs) orElse
    _compare_published_option(lhs, rhs) orElse
    _compare_updated_option(lhs, rhs) orElse
    _compare_lastmodified_option(lhs, rhs) getOrElse false
  )

  private def _compare_draft(lhs: Notice, rhs: Notice): Boolean = (
    _compare_status_option(lhs, rhs) orElse
    _compare_published_option(lhs, rhs) orElse
    _compare_updated_option(lhs, rhs) orElse
    _compare_lastmodified_option(lhs, rhs) getOrElse false
  )

  private def _compare_status_option(lhs: Notice, rhs: Notice): Option[Boolean] =
      DocumentMetaData.Status.compareDraftOption(lhs.status, rhs.status)

  private def _compare_updated_option(lhs: Notice, rhs: Notice): Option[Boolean] =
    if (lhs.updateds == rhs.updateds)
      None
    else
      LocalDateUtils.compareDescOption(lhs.lastUpdated, rhs.lastUpdated)

  private def _compare_published_option(lhs: Notice, rhs: Notice): Option[Boolean] =
    if (lhs.published == rhs.published)
      None
    else
      LocalDateUtils.compareDescOption(lhs.published, rhs.published)

  private def _compare_lastmodified_option(lhs: Notice, rhs: Notice): Option[Boolean] =
    OptionUtils.compareDescOption(lhs.lastModified, rhs.lastModified)

  private def _make_stub(n: Int): Vector[Notice] =
    if (n >= 10)
      Vector.empty
    else
      Vector.fill(10 - n)(Notice.notitle)
}

object DoxSite {
  import io.circe._
  import io.circe.syntax._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._
  import org.goldenport.util.CirceUtils.Codec._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  case class Config(
    inputTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    transformTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    outputTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    strategy: Strategy = Strategy.Overview,
    localeSetting: Config.LocaleSetting = Config.LocaleSetting.jaen,
    siteMetadata: SiteMetadata = SiteMetadata.empty,
    siteNavigation: Config.SiteNavigation = Config.SiteNavigation.default,
    siteOutput: Config.SiteOutput = Config.SiteOutput.default,
    siteHeader: Config.SiteHeader = Config.SiteHeader.default,
    simplemodelingOrg: Boolean = false,
    origin: Option[File] = None,
    includeFilePatterns: Vector[Regex] = Vector(""".*\.(dox|org|md|markdown|ya?ml|png|jpg|jpeg|svg)$""").map(_.r),
    excludeFilePatterns: Vector[Regex] = Vector(
      """^_.*""",      // filenames starting with "_" (include-only partials)
      """.*[~]$""",    // editor temporary files
      """.*\.bak$""",  // backup files
      """.*\.tmp$""",  // temporary files
      """.*\.swp$"""   // swap files (e.g., Vim)
    ).map(_.r),
    includePathPatterns: Vector[Regex] = Vector(".*").map(_.r),
    excludePathPatterns: Vector[Regex] = Vector(
      """(?x)
  (?:^|[\\/])_                   # directories starting with "_"
  | \.d(?:[\\/]|\Z)              # directories ending with ".d"
  | (?:^|[\\/])(assets|styles|includes)[\\/]  # static resource directories
"""
    ).map(_.r)
  ) {
    def isAutoWire(p: Page): Boolean = strategy.isAutoWire(p)
    def isAutoI18n(p: Page): Boolean = strategy.isAutoI18n(p)
    def isNotice: Boolean = strategy.isNotice
    def isGlossary: Boolean = strategy.isGlossary
    def isGlossaryInDocument: Boolean = strategy.isGlossaryInDocument
    def isLinkEnable: Boolean = strategy.isLinkEnable
    def isLinkEnable(p: Page): Boolean = strategy.isLinkEnable(p)

    def siteTitle: String = siteMetadata.name getOrElse {
      if (simplemodelingOrg)
        "SimpleModeling"
      else
        "SmartDox Site"
    }
    def siteUrl: Option[URL] =
      siteMetadata.url.map(x => new URI(x).toURL).orElse {
        if (simplemodelingOrg)
          Some(new URI("https://www.simplemodeling.org").toURL)
        else
          None
      }
    def siteDefaultAuthor: Option[I18NString] =
      siteMetadata.author.flatMap(_.name).map(I18NString(_)).orElse {
        if (simplemodelingOrg)
          Some(I18NString.enja("ASAMI, Tomoharu", "浅海 智晴"))
        else
          None
      }

    def textMark = Config.WorkAround.textMark

    def +(rhs: Config): Config = copy(
      inputTreeTransformerConfig = lastOption(inputTreeTransformerConfig, rhs.inputTreeTransformerConfig),
      transformTreeTransformerConfig = lastOption(transformTreeTransformerConfig, rhs.transformTreeTransformerConfig),
      outputTreeTransformerConfig = lastOption(outputTreeTransformerConfig, rhs.outputTreeTransformerConfig),
      strategy = rhs.strategy,
      siteMetadata = lastOption(
        Option(siteMetadata).filterNot(_.isEmpty),
        Option(rhs.siteMetadata).filterNot(_.isEmpty)
      ).getOrElse(SiteMetadata.empty),
      siteNavigation = if (rhs.siteNavigation == Config.SiteNavigation.default) siteNavigation else rhs.siteNavigation,
      siteOutput = if (rhs.siteOutput == Config.SiteOutput.default) siteOutput else rhs.siteOutput,
      siteHeader = if (rhs.siteHeader == Config.SiteHeader.default) siteHeader else rhs.siteHeader,
      simplemodelingOrg = simplemodelingOrg || rhs.simplemodelingOrg
    )

    def withSimpleModelingOrgCompatibility: Config = copy(
      siteNavigation = Config.SiteNavigation.simplemodelingOrgCompatibility,
      siteOutput = Config.SiteOutput.simplemodelingOrgCompatibility,
      siteHeader = Config.SiteHeader.simplemodelingOrgCompatibility,
      simplemodelingOrg = true
    )
  }
  object Config {
    val default = Config()

    case class SiteNavigation(
      mode: SiteNavigation.Mode = SiteNavigation.Mode.Category
    )
    object SiteNavigation {
      val default = SiteNavigation()
      val simplemodelingOrgCompatibility = SiteNavigation(SiteNavigation.Mode.SimpleModeling)

      sealed trait Mode extends NamedValueInstance
      object Mode extends EnumerationClass[Mode] {
        val elements = Vector(SimpleModeling, Category)

        case object SimpleModeling extends Mode {
          val name = "simplemodeling"
        }
        case object Category extends Mode {
          val name = "category"
        }

        implicit val modeDecoder: Decoder[Mode] = Decoder.decodeString.emap(_create)
        implicit val modeEncoder: Encoder[Mode] = Encoder.encodeString.contramap(_.name)

        private def _create(p: String): Either[String, Mode] =
          get(p).toRight(s"Unknown site.navigation.mode: $p")
      }

      implicit val siteNavigationDecoder: Decoder[SiteNavigation] = deriveConfiguredDecoder
      implicit val siteNavigationEncoder: Encoder[SiteNavigation] = deriveConfiguredEncoder
    }

    case class SiteOutput(
      localeMode: SiteOutput.LocaleMode = SiteOutput.LocaleMode.SingleLocaleRoot,
      defaultLocale: String = "ja"
    )
    object SiteOutput {
      val default = SiteOutput()
      val simplemodelingOrgCompatibility = SiteOutput(SiteOutput.LocaleMode.MultiLocaleSubdirs, "ja")

      sealed trait LocaleMode extends NamedValueInstance
      object LocaleMode extends EnumerationClass[LocaleMode] {
        val elements = Vector(MultiLocaleSubdirs, SingleLocaleRoot)

        case object MultiLocaleSubdirs extends LocaleMode {
          val name = "multi_locale_subdirs"
        }
        case object SingleLocaleRoot extends LocaleMode {
          val name = "single_locale_root"
        }

        implicit val localeModeDecoder: Decoder[LocaleMode] = Decoder.decodeString.emap(_create)
        implicit val localeModeEncoder: Encoder[LocaleMode] = Encoder.encodeString.contramap(_.name)

        private def _create(p: String): Either[String, LocaleMode] =
          get(p).toRight(s"Unknown site.output.locale_mode: $p")
      }

      implicit val siteOutputDecoder: Decoder[SiteOutput] = deriveConfiguredDecoder
      implicit val siteOutputEncoder: Encoder[SiteOutput] = deriveConfiguredEncoder
    }

    case class SiteHeader(
      languageToggle: Boolean = false
    )
    object SiteHeader {
      val default = SiteHeader()
      val simplemodelingOrgCompatibility = SiteHeader(languageToggle = true)

      implicit val siteHeaderDecoder: Decoder[SiteHeader] = deriveConfiguredDecoder
      implicit val siteHeaderEncoder: Encoder[SiteHeader] = deriveConfiguredEncoder
    }

    case class LocaleSetting(
      slots: Vector[LocaleSetting.Slot] = Vector.empty
    ) {
      def context(locale: Locale): I18NContext = slots.find(_.locale == locale).map(_.context) getOrElse RAISE.notImplementedYetDefect

      def autoI18nDelimiter = "｜"
      def autoI18nLanguages = List(LocaleUtils.en, LocaleUtils.ja)
    }
    object LocaleSetting {
      case class Slot(locale: Locale, context: I18NContext)
      object Slot {
        implicit val slotDecoder: Decoder[Slot] = Decoder.instance { cursor =>
          for {
            locale <- cursor.downField("locale").as[Locale]
            contextname <- cursor.downField("context").as[String]
          } yield {
            val context = contextname match {
              case "ja" => I18NContext.ja
              case "en" => I18NContext.en
            }
            Slot(locale, context)
          }
        }

        implicit val slotEncoder: Encoder[Slot] = Encoder.instance { s =>
          Json.obj(
            "locale" -> s.locale.asJson,
            "context" -> s.context.locale.asJson
          )
        }
      }

      val jaen = LocaleSetting(
        Vector(
          Slot(LocaleUtils.ja, I18NContext.ja),
          Slot(LocaleUtils.en, I18NContext.en)
        )
      )

      implicit val localesettingDecoder: Decoder[LocaleSetting] = deriveConfiguredDecoder
      implicit val localesettingEncoder: Encoder[LocaleSetting] = deriveConfiguredEncoder
    }

    // def create(p: Option[Strategy]): Config = default.copy(strategy = p)
    def create(p: SiteParameters.Holder): Config = {
      val inconfig = p.target match {
        case Some(s) if s.nonEmpty =>
          val g = s"/${PROP_GLOSSARY_DIRECTORY}/.*".r
          Some(_tree_transformer_config(s :+ g))
        case _ => None
      }
      val outconfig = p.outputScopePolicy match {
        case Some(policy) =>
          val scope = TreeTransformer.Config.Scope(policy)
          val c = TreeTransformer.Config(scope)
          Some(c)
        case _ => None
      }
      default.copy(
        inputTreeTransformerConfig = inconfig,
        outputTreeTransformerConfig = outconfig,
        strategy = p.strategy getOrElse default.strategy
      )
    }

    private def _tree_transformer_config(ps: List[Regex]) = {
      val scope = TreeTransformer.Config.Scope(
        TreeTransformer.Config.Scope.Policy.Target,
        ps
      )
      TreeTransformer.Config(scope)
    }

    implicit val configDecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configEncoder: Encoder[Config] = deriveConfiguredEncoder

    object WorkAround {
      object textMark {
        val article = "📄 " // Markdown/記事リンク: [📄ドメイン・モデル構成要素]
      }
    }
  }

  sealed trait Strategy extends NamedValueInstance {
    def isAutoWire(p: Page): Boolean = documentStrategy(p).isAutoWire
    def isAutoI18n(p: Page): Boolean = documentStrategy(p).isAutoI18n
    def isNotice: Boolean = true
    def isGlossary: Boolean = true
    def isGlossaryInDocument: Boolean = false
    def isLinkEnable: Boolean = true
    def isLinkEnable(p: Page): Boolean = documentStrategy(p).isLinkEnable
    def isDiagramGeneration(p: Page): Boolean = documentStrategy(p).isDiagramGeneration
    def isActive(p: Dox): Boolean =
      documentStrategy(p).isActive
    def documentStrategy(p: Page): DocumentStrategy =
      documentStrategy(p.dox)
    def documentStrategy(p: Dox): DocumentStrategy =
      documentStrategy(_metadata(p))
    def documentStrategy(p: DocumentMetaData): DocumentStrategy

    private def _metadata(p: Dox) = Dox.getMetadata(p) getOrElse DocumentMetaData.empty
  }
  object Strategy extends EnumerationClass[Strategy] {
    import DocumentMetaData.Status

    val elements = Vector(Production, ProductionPreview, Full, WorkInProgress, Draft, Preparation, Overview, Test)

    case object Production extends Strategy {
      val name = "production"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Full
          case _ => DocumentStrategy.Skip
        }
    }
    case object ProductionUpdate extends Strategy {
      val name = "production-update"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match { // TODO 3days
          case Status.Published => DocumentStrategy.Full
          case _ => DocumentStrategy.Skip
        }
    }
    case object ProductionPreview extends Strategy {
      val name = "production-preview"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Draft
          case _ => DocumentStrategy.Skip
        }
    }
    case object Full extends Strategy {
      val name = "full"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Inactive => DocumentStrategy.Skip
          case _ => DocumentStrategy.Full
        }
    }
    case object WorkInProgress extends Strategy {
      val name ="work-in-progress"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Draft
          case Status.WorkInProgress => DocumentStrategy.Full
          case Status.Draft => DocumentStrategy.Skip
          case Status.InPreparation => DocumentStrategy.Skip
          case Status.Inactive => DocumentStrategy.Skip
          case Status.Test => DocumentStrategy.Skip
          case Status.Error => DocumentStrategy.Draft
        }
    }
    case object Draft extends Strategy {
      val name ="draft"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Draft
          case Status.WorkInProgress => DocumentStrategy.Draft
          case Status.Draft => DocumentStrategy.Draft
          case Status.InPreparation => DocumentStrategy.Skip
          case Status.Inactive => DocumentStrategy.Skip
          case Status.Test => DocumentStrategy.Skip
          case Status.Error => DocumentStrategy.Draft
        }
    }
    case object Preparation extends Strategy {
      val name ="preparation"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.InPreparation => DocumentStrategy.Draft
          case Status.Error => DocumentStrategy.Draft
          case _ => DocumentStrategy.Skip
        }
    }
    case object Overview extends Strategy {
      val name ="overview"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Draft
          case Status.WorkInProgress => DocumentStrategy.Draft
          case Status.Draft => DocumentStrategy.Draft
          case Status.InPreparation => DocumentStrategy.Draft
          case Status.Inactive => DocumentStrategy.Skip
          case Status.Test => DocumentStrategy.Draft
          case Status.Error => DocumentStrategy.Draft
        }
    }
    case object Test extends Strategy {
      val name ="test"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Test => DocumentStrategy.Test
          case Status.Error => DocumentStrategy.Draft
          case _ => DocumentStrategy.Skip
        }
    }

    implicit val strategyDecoder: Decoder[Strategy] = Decoder.decodeString.emap(_create)

    implicit val strategyEncoder: Encoder[Strategy] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, Strategy] =
      get(p).toRight(s"Unknown strategy: $p")
  }

  sealed trait DocumentStrategy extends NamedValueInstance {
    def isActive: Boolean = true
    def isAutoWire: Boolean = true
    def isAutoI18n: Boolean = true // See Dox2Parser
    def isNotice: Boolean = true
    def isGlossary: Boolean = true
    def isLinkEnable: Boolean = true
    def isDiagramGeneration: Boolean = true
//    def targetStatus: List[DocumentMetaData.Status]
  }
  object DocumentStrategy extends EnumerationClass[DocumentStrategy] {
    import DocumentMetaData.Status

    val elements = Vector(Production, Full, WorkInProgress, Draft, Skip, Test)

    case object Production extends DocumentStrategy {
      val name = "production"
//      def targetStatus = List(Status.Published)
    }
    case object Full extends DocumentStrategy {
      val name = "full"
      // def targetStatus = List(
      //   Status.Published,
      //   Status.WorkInProgress,
      //   Status.Draft,
      //   Status.InPreparation
      // )
    }
    case object WorkInProgress extends DocumentStrategy {
      val name ="work-in-progress"
//      def targetStatus = List(Status.WorkInProgress)
    }
    case object Draft extends DocumentStrategy {
      val name ="draft"
      override def isAutoWire: Boolean = false
      override def isGlossary: Boolean = false
      override def isLinkEnable: Boolean = false
      override def isDiagramGeneration: Boolean = false
//      def targetStatus = List(Status.Draft, Status.WorkInProgress)
    }

    case object Skip extends DocumentStrategy {
      val name ="skip"
      override def isActive: Boolean = false
      override def isAutoWire: Boolean = false
      override def isAutoI18n: Boolean = false
      override def isNotice: Boolean = false
      override def isGlossary: Boolean = false
      override def isLinkEnable: Boolean = false
      override def isDiagramGeneration: Boolean = false
    }

    case object Test extends DocumentStrategy {
      val name = "test"
    }

    implicit val strategyDecoder: Decoder[DocumentStrategy] = Decoder.decodeString.emap(_create)

    implicit val strategyEncoder: Encoder[DocumentStrategy] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, DocumentStrategy] =
      get(p).toRight(s"Unknown document strategy: $p")
  }

  class DoxSiteBuilder(
    override val rule: DoxSiteBuilder.Rule,
    context: DoxSiteTransformer.Context
  ) extends TreeTransformer[Realm.Data, Node] {
    override def isCleanEmptyChildren = true
    def treeTransformerContext = context.nodeContext

    override protected def make_Node(
      oldname: String, // unused
      newname: String, // unused
      node: TreeNode[Realm.Data],
      content: Realm.Data
    ): TreeTransformer.Directive[Node] = {
      // println("S: " + content)
      content match {
        case Realm.EmptyData => TreeTransformer.Directive.Default[Node]
        case m: Realm.StringData => _build_file(node, m)
        case m: Realm.UrlData => TreeTransformer.Directive.Default[Node]
        case m: Realm.FileData => directive_leaf(ImageNode(Node.Name(newname), m.file))
        case m: Realm.BagData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ObjectData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ApplicationData => TreeTransformer.Directive.Default[Node]
      }
    }

    private def _build_file(node: TreeNode[Realm.Data], p: Realm.StringData): TreeTransformer.Directive[Node] =
      _build_file(node, p.string, p.lastModifiedOption)

    private def _build_file(node: TreeNode[Realm.Data], p: String, lastmodified: Option[Instant]): TreeTransformer.Directive[Node] =
      node.getNameSuffix match {
        case None => TreeTransformer.Directive.Default[Node]
        case Some(s) =>
          node.getContent match {
            case None => TreeTransformer.Directive.Default[Node]
            case Some(c) => 
              c match {
                case m: Realm.StringData =>
                  val name = node.name
                  val r: List[Node] = s match {
                    case "dox" => _dox_page(node, m.string, m.lastModifiedOption)
                    case "org" => _org_page(name, m.string)
                    case "md" => _markdown_page(name, m.string)
                    case "markdown" => _markdown_page(name, m.string)
                    case "yaml" => _yaml_metadata(node.pathname, name, m.string)
                    case _ => Nil
                  }
                  r match {
                    case Nil => TreeTransformer.Directive.Default[Node]
                    case m :: Nil => TreeTransformer.Directive.Node(TreeNode.create(m.name.name, m))
                    case ms => TreeTransformer.Directive.Nodes(ms.map(x => TreeNode.create(x.name.name, x)))
                  }
                case _ => TreeTransformer.Directive.Default[Node]
              }
          }
      }

    private def _dox_page(
      node: TreeNode[Realm.Data],
      c: String,
      lastmodified: Option[Instant]
    ): List[Node] = try {
      _dox_page_create(node, c, lastmodified)
    } catch {
      case NonFatal(e) => _error_page(node, c, lastmodified, e)
    }

    private def _dox_page_create(
      node: TreeNode[Realm.Data],
      c: String,
      lastmodified: Option[Instant]
    ) = {
      // println(s"_dox_page: $c")
      val dox = context.cache.get(node.pathname, lastmodified) getOrElse {
        val pathname = rule.doxSiteConfig.origin match {
          case Some(s) => new File(s, node.pathname).toString
          case None => node.pathname
        }
        Dox2Parser.parseWithFilename(pathname, c)
      }
      _create_dox(node.name, dox, lastmodified)
    }

    private def _error_page(
      node: TreeNode[Realm.Data],
      c: String,
      lastmodified: Option[Instant],
      e: Throwable
    ) = {
      val dox = Dox2Parser.errorDocument(node.pathname, e)
      _create_dox(node.name, dox, lastmodified)
    }

    private def _org_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      _create_dox(name, dox)
    }

    private def _markdown_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      _create_dox(name, dox)
    }

    // private def _create_dox(name: String, dox: Dox, lastmodified: Option[Instant] = None) =
    //   if (rule.strategy.isActive(dox))
    //     _create_page(name, dox, lastmodified)
    //   else
    //     Nil

    private def _create_dox(name: String, dox: Dox, lastmodified: Option[Instant] = None) =
      _create_page(name, dox, lastmodified)

    private def _create_page(name: String, dox: Dox, lastmodified: Option[Instant]) =
      List(Page(name, Dox.toDocument(dox), lastmodified))

    private def _yaml_metadata(pathname: String, name: String, s: String) =
      name match {
        case "category.yaml" => _yaml_category(pathname, name, s)
        case m => _yaml_hocon(name, s)
      }

    private def _yaml_category(pathname: String, name: String, s: String): List[Node] = try {
      val index = StringUtils.changeLeafRelative(pathname, "index.html")
      implicit def decoder = Category.categoryDecoder(new URI(index))

      val in = InputSource(s)
      val r = for {
        c <- ConfigLoader.loadConfigFromYaml[Category](in)
        r <- Consequence(CategoryMetaData(name, c))
      } yield List(r)
      r getOrElse Nil
    } catch {
      case NonFatal(e) => List(CategoryMetaData.error(name, e))
    }

    private def _yaml_hocon(name: String, s: String): List[Node] = {
      val in = InputSource(s)
      val r = for {
        hocon <- ConfigLoader.loadConfigHocon(in)
        r <- Consequence(HoconMetaData(Node.Name(name), hocon))
      } yield List(r)
      r getOrElse Nil
    }
  }
  object DoxSiteBuilder {
    case class Rule(
      doxSiteConfig: Config = Config.default
    ) extends TreeTransformer.Rule[Realm.Data, Node] {
      def strategy = doxSiteConfig.strategy

      override def config = doxSiteConfig.inputTreeTransformerConfig
      override def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
        val filename = p.name
        val pathname = p.pathname
        if (_is_available(filename, pathname)) {
          p.getNameSuffix.collect {
            case "dox" => s"${p.nameBody}.dox"
            case "org" => s"${p.nameBody}.dox"
            case "md" => s"${p.nameBody}.dox"
            case "markdown" => s"${p.nameBody}.dox"
            case "yaml" => s"${p.nameBody}.yaml"
            case "png" => s"${p.nameBody}.png"
            case "jpg" => s"${p.nameBody}.jpg"
            case "jpeg" => s"${p.nameBody}.jpeg"
            case "svg" => s"${p.nameBody}.svg"
          }
        } else {
          None
        }
      }

      private def _is_available(filename: String, pathname: String) =
        _is_available_filename(filename) && _is_available_pathname(pathname)

      private def _is_available_filename(filename: String) = (
        RegexUtils.isWholeMatch(doxSiteConfig.includeFilePatterns, filename) &&
          !RegexUtils.isWholeMatch(doxSiteConfig.excludeFilePatterns, filename)
      )

      private def _is_available_pathname(pathname: String) = (
        RegexUtils.isWholeMatch(doxSiteConfig.includePathPatterns, pathname) &&
          !RegexUtils.isWholeMatch(doxSiteConfig.excludePathPatterns, pathname)
      )

      override def isIgnore(p: TreeNode[Realm.Data]): Boolean =
        p.name.endsWith(".d")
    }
    object Rule {
      def apply(p: TreeTransformer.Config): Rule = Rule(Config(inputTreeTransformerConfig = Some(p)))
    }
  }

  class DoxSiteEnabler(
    val context: DoxSiteTransformer.Context,
    rule: DoxSiteBuilder.Rule
  ) extends DoxSiteTransformer {
    override protected def make_Node(
      node: TreeNode[Node],
      content: Node
    ): TreeTransformer.Directive[Node] = content match {
      case m: Page =>
        if (rule.strategy.isActive(m.dox))
          directive_leaf(m)
        else
          directive_empty
      case m => TreeTransformer.Directive.Default[Node]
    }
  }
  object DoxSiteEnabler {
  }

  class RealmBuilder(
    gcontext: Context,
    rule: RealmBuilder.Rule,
    context: Option[TreeTransformer.Context[Realm.Data]] = None
  ) extends TreeTransformer[Node, Realm.Data] {
    def treeTransformerContext = {
      val c = context getOrElse gcontext.realmContext
      rule.config.fold(c)(c.withConfig)
    }

    private def _i18n_context = context.flatMap(_.i18NContextOption) getOrElse gcontext.i18NContext

    override protected def make_Node(
      node: TreeNode[Node],
      content: Node
    ): TreeTransformer.Directive[Realm.Data] = {
      content match {
        case m: Page => _to_html(m) // TreeTransformer.Directive.Content(m.toRealmData)
        case m: MetaDataNode => directive_empty
        case m: ImageNode => directive_leaf(FileData(m.file))
      }
    }

    private def _to_html(p: Page): TreeTransformer.Directive.LeafNode[Realm.Data] = {
      val dox = _filter(p.dox)
      val rule = Dox2HtmlTransformer.Rule.noCss
      val s = Consequence.from(Dox2HtmlTransformer(gcontext, rule).transform(dox)).
        foldConclusion(_.message)
      val data = Realm.StringData(s)
      val name = StringUtils.changeSuffix(p.name.name, "html")
      TreeTransformer.Directive.LeafNode(name, data)
    }

    private def _filter(p: Dox) =
      rule.targetLocale.fold(p) { x =>
        val c = _i18n_context.withLocale(x)
        val ctx = gcontext.doxContext.withI18NContext(c)
        Dox.transform(p, new LanguageFilterTransformer(ctx))
      }
  }
  object RealmBuilder {
    case class Rule(
      override val config: Option[TreeTransformer.Config] = None,
      targetLocale: Option[Locale] = None
    ) extends TreeTransformer.Rule[Node, Realm.Data] {
      def withConfig(p: Option[TreeTransformer.Config]) = copy(config = p)
    }
    object Rule {
      val default = Rule()
      val en = Rule(targetLocale = Some(LocaleUtils.en))
      val ja = Rule(targetLocale = Some(LocaleUtils.ja))
    }
  }

  val realmConfig = Realm.Builder.Config.default.addTextSuffixes("yaml")

  def create(
    context: Context,
    file: File
  ): DoxSite = create(context, file, None, DoxSite.Config.default)

  def create(
    context: Context,
    file: File,
    configname: Option[String],
    inconfig: DoxSite.Config
  ): DoxSite = {
    val a = Realm.create(realmConfig, file)
    create(context, a, configname, inconfig)
  }

  def create(
    context: Context,
    realm: Realm,
    configname: String,
    inconfig: DoxSite.Config
  ): DoxSite =
    create(context, realm, Some(configname), inconfig)

  def create(
    context: Context,
    realm: Realm,
    configname: String,
    inconfig: DoxSite.Config,
    extraPages: Seq[(String, Page)]
  ): DoxSite =
    create(context, realm, Some(configname), inconfig, extraPages)

  def create(
    context: Context,
    realm: Realm,
    configname: Option[String],
    inconfig: DoxSite.Config
  ): DoxSite =
    create(context, realm, configname, inconfig, Nil)

  def create(
    context: Context,
    realm: Realm,
    configname: Option[String],
    inconfig: DoxSite.Config,
    extraPages: Seq[(String, Page)],
    videopublications: Seq[PublishMetadata.VideoPublication] = Nil,
    publicationtriples: Seq[Rdf.Triple] = Nil
  ): DoxSite = {
    val config = _config(inconfig, realm, configname)(context.i18NContext)
    val nodectx = TreeTransformer.Context.default[Node]
    val doxctx = context.doxContext
    val ctx = DoxSiteTransformer.Context(
      DoxSiteTransformer.Config(Some(config)),
      context,
      nodectx,
      doxctx
    )
    val rule = DoxSiteBuilder.Rule(config)
    val a00: Tree[Node] = realm.transformTree(new DoxSiteBuilder(rule, ctx))
    val a0: Tree[Node] = _deploy_video_source_pages(a00, videopublications)
    val a1: Tree[Node] = a0.transform(new DoxSiteEnabler(ctx, rule))
    val a0x = _deploy_extra_pages(a0, extraPages)
    val a = _deploy_extra_pages(a1.transform(new DoxSitePreTransformer(ctx)), extraPages)
    val categories = _collect_category(config, context, a)
    val (notices, history0) = _collect_notice_history(config, context, categories, a)
    val atoms = _build_atom_feed(notices)
    val (b, glossary) = _build_glossary(ctx, a)
    val bibliography = _collect_bibliography(ctx, a)
    val keywords = _collect_keywords()
    val tags = _collect_tags()
    val history = _build_history(history0, glossary, bibliography, keywords, tags)
    val metadata0x = MetaData(
      glossary = glossary,
      categories = categories,
      keywords = keywords,
      tags = tags,
      notices = notices,
      atomFeed = atoms,
      history = history
    )
    val metadata0 = metadata0x.copy(dashboard = DoxSiteDashboard.create(metadata0x))
    val ctx1 = ctx.withMetaData(metadata0)
    val (c, links) = _enable_link(ctx1, b, a0x)
    val metadata1 = metadata0.copy(linkCollection = links)
    val metadata = _build_site_model(metadata1, config.siteMetadata, videopublications, publicationtriples)
    val d: Tree[Node] = _deploy_metadata(c, metadata)
    val z = d.transform(new DoxSitePostTransformer(ctx1))
    _flush_cache(ctx1, z)
    new DoxSite(config, z, metadata)
  }

  private def _deploy_video_source_pages(base: Tree[Node], videopublications: Seq[PublishMetadata.VideoPublication]): Tree[Node] = {
    val rewrites = ArrayBuffer.empty[(String, String, Page)]
    base.traverse(new DoxSiteVisitor {
      override protected def enter_Content(node: TreeNode[Node], content: Node): Unit =
        content match {
          case page: Page =>
            _video_source_page(node.pathname, page, videopublications).foreach(rewrites += _)
          case _ =>
        }
    })
    rewrites.foreach {
      case (source, target, page) =>
        base.setContent(target, page)
        base.remove(source)
    }
    base
  }

  private def _video_source_page(path: String, page: Page, videopublications: Seq[PublishMetadata.VideoPublication]): Option[(String, String, Page)] = {
    val normalized = path.stripPrefix("/")
    val suffix = "/index.dox"
    if (!normalized.endsWith(suffix)) {
      None
    } else {
      val directory = normalized.stripSuffix(suffix)
      if (!directory.endsWith(".video")) {
        None
      } else {
        val segments = directory.split('/').toVector.filter(_.nonEmpty)
        val slug = segments.last.stripSuffix(".video")
        val parent = segments.dropRight(1).mkString("/")
        val target = if (parent.isEmpty) s"$slug.dox" else s"$parent/$slug.dox"
        val embedded = _embed_video_publication(page, slug, directory, videopublications)
        Some((directory, target, embedded.copy(name = Node.Name(s"$slug.dox"))))
      }
    }
  }


  private def _embed_video_publication(
    page: Page,
    slug: String,
    directory: String,
    videopublications: Seq[PublishMetadata.VideoPublication]
  ): Page = {
    val extra = videopublications.find(_.matchesPackage(directory)) match {
      case Some(video) =>
        val track = video.caption.map { caption =>
          Html5(
            "track",
            VectorMap("kind" -> "captions", "src" -> caption.publicPath, "label" -> "Captions"),
            Nil
          )
        }
        val player = Html5(
          "video",
          VectorMap("controls" -> "controls", "src" -> video.publicPath, "class" -> "smartdox-video-player"),
          track.toList :+ Text(s"Video: ${video.name}")
        )
        Html5(
          "div",
          VectorMap("class" -> "smartdox-video-publication"),
          player :: _video_publication_links(video).toList
        )
      case None =>
        DiagnosticBlock.error(
          "Missing video publication metadata",
          s"No publication metadata found for $directory. Rendered the article without running video generation."
        )
    }
    val dox = page.dox.copy(body = page.dox.body.copy(contents = page.dox.body.contents :+ extra))
    page.copy(dox = dox)
  }


  private def _video_publication_links(video: PublishMetadata.VideoPublication): Option[Html5] = {
    val links = Vector(
      video.transcript.map(x => _video_publication_link("Transcript", x.publicPath)),
      video.rdf.flatMap(_.turtle).map(x => _video_publication_link("RDF Turtle", x.publicPath)),
      video.rdf.flatMap(_.jsonLd).map(x => _video_publication_link("RDF JSON-LD", x.publicPath)),
      video.rdf.flatMap(_.manifest).map(x => _video_publication_link("RDF Manifest", x.publicPath))
    ).flatten
    if (links.isEmpty)
      None
    else
      Some(Html5(
        "ul",
        VectorMap("class" -> "smartdox-video-publication-links"),
        links.toList
      ))
  }

  private def _video_publication_link(label: String, href: String): Html5 =
    Html5(
      "li",
      VectorMap.empty,
      List(Html5("a", VectorMap("href" -> href), List(Text(label))))
    )

  private def _deploy_extra_pages(
    base: Tree[Node],
    extraPages: Seq[(String, Page)]
  ): Tree[Node] = {
    extraPages.foreach {
      case (path, page) => base.setContent(path, page)
    }
    base
  }

  private def _collect_category(
    config: DoxSite.Config,
    gcontext: Context,
    p: Tree[Node]
  ): CategoryCollection = {
    val c = new CategoryCollector(gcontext)
    p.traverse(c)
    c.categories
  }

  private def _collect_notice_history(
    config: DoxSite.Config,
    gcontext: Context,
    categories: CategoryCollection,
    p: Tree[Node]
  ): (Notices, History) =
    if (config.isNotice) {
      val noticecollector = new NoticeCollector(gcontext, categories)
      p.traverse(noticecollector)
      (noticecollector.notices, noticecollector.history)
    } else {
      (Notices.empty, History.empty)
    }

  private def _collect_keywords(): KeywordCollection = KeywordCollection.empty

  private def _collect_tags(): TagCollection = TagCollection.create()

  private def _build_history(p: History, g: Glossary, bib: Bibliography, k: KeywordCollection, t: TagCollection): History = {
    val a = g.toHistory
    val b = k.toHistory
    val c = t.toHistory
    val d = bib.toHistory
    p.add(a, b, c, d)
  }

  private def _build_atom_feed(p: Notices): Option[AtomFeedBag] = {
    val ja = p.toAtomFeed(LocaleUtils.ja)
    val en = p.toAtomFeed(LocaleUtils.en)
    if (ja.nonEmpty || en.nonEmpty)
      Some(AtomFeedBag(ja, en))
    else
      None
  }

  private def _build_glossary(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): (Tree[Node], Glossary) = {
    val (a, g0) = _collect_glossary(ctx, p)
    val (b, g1) = _build_glossary_in_documents(ctx, a)
    (b, g0 + g1)
  }

  private def _collect_glossary(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): (Tree[Node], Glossary) = {
    if (ctx.config.isGlossary) {
      val g = GlossaryCollector.collect(ctx.generatorContext, p)
      p.remove(GlossaryCollector.PROP_GLOSSARY_DIRECTORY)
      (p, g)
    } else {
      (p, Glossary.empty)
    }
  }

  private def _build_glossary_in_documents(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): (Tree[Node], Glossary) =
    if (ctx.config.isGlossaryInDocument) {
      val gb = new GlossaryBuilder(ctx)
      val b: Tree[Node] = p.transform(gb)
      (b, gb.glossary)
    } else {
      (p, Glossary.empty)
    }

  private def _collect_bibliography(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): Bibliography = {
    Bibliography.empty // TODO
  }

  private def _build_site_model(
    p: MetaData,
    siteMetadata: SiteMetadata,
    videopublications: Seq[PublishMetadata.VideoPublication],
    publicationtriples: Seq[Rdf.Triple]
  ): MetaData = {
    val articles = _article_site_resources(p)
    val glossaries = _glossary_site_resources(p)
    val resourcs = articles ++ glossaries
    val site = SiteModel.create(p, resourcs, siteMetadata, videopublications, publicationtriples)
    val metadata = p.copy(site = site)
    metadata.copy(dashboard = DoxSiteDashboard.create(metadata))
  }

  private def _article_site_resources(p: MetaData) =
    p.notices.notices.filter { _.effectiveKind match {
      case DocumentMetaData.Kind.Article => true
      case DocumentMetaData.Kind.Blog => true
      case _ => false
    }}.map(_.toSiteResource)

  private def _glossary_site_resources(p: MetaData) = {
    p.glossary.definitions.flatMap {
      case m: Glossary.Definition.InDocument => None
      case m: Glossary.Definition.InGlossary => Some(m.toSiteResource)
    }
  }

  private def _enable_link(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node],
    full: Tree[Node]
  ): (Tree[Node], Option[LinkCollection]) =
    if (ctx.config.isLinkEnable) {
      val doxsitec = ctx.doxSiteConfig
      val c = new LinkCollector(doxsitec)(p)
      val r = p.transform(new LinkEnabler(ctx, c, p, full))
      val cr = new LinkCollector(doxsitec)(r)
      (r, Some(cr))
    } else {
      (p, None)
    }

  private def _deploy_metadata(
    base: Tree[Node],
    meta: MetaData
  ): Tree[Node] = {
    val a = _deploy_glossary(base, meta.glossary)
    _deploy_history(a, meta.history)
  }

  private def _deploy_glossary(base: Tree[Node], glossary: Glossary): Tree[Node] = {
    // val g = base.setNode("glossary")
    for (d <- glossary.definitions) {
      val c = d.createPage
      val path = d.page.toString // TODO
      // g.setContent(path, c)
      base.setContent(path, c)
    }
    base
  }

  private def _deploy_history(base: Tree[Node], history: History): Tree[Node] = {
    val years = history.yearList
    val home = base.setNode("history")
    for ((y, h) <- years) {
      _deploy_year(home, y, h)
    }
    base
  }

  private def _deploy_year(base: TreeNode[Node], year: Int, h: History.HistoryCollection): Unit = {
    val tb = new Table.Builder()
    tb.withCaption(year.toString)
    tb.withHeaderString(List("Date", "Kind", "Event", "Category", "Title", "Summary"))
    for (x <- h.desc) {
      val date = Dox.text(x.date.toString)
      val ckind = Dox.toDox(x.contentKind.title)
      val evt = Dox.toDox(x.eventKind.title)
      val corner = {
        x.category match {
          case Some(s) =>
            val title = s.title.map(_.title) match {
              case Some(s) => Dox.toDox(s)
              case None => Dox.text(s.name.name)
            }
            Hyperlink.createCategory(title, new URI(s"../${s.uri}"))
          case None => Dox.text("-")
        }
      }
      val title = Hyperlink.createArticle(x.title, new URI(s"../${x.uri}"), base.pathnameValue)
      val summary = x.effectiveBrief
      tb.append(date, ckind, evt, corner, title, summary)
    }
    val t = tb.apply()
    val title = year.toString
    val explanation = Explanation.empty
    val meta = DocumentMetaData.create(title, explanation)
    val head = Head(metadata = meta)
    val body = Body(List(t))
    val dox = Document(head, body)
    val name = year.toString
    val lastmodefied = None
    val page = Page(name, dox, None)
    base.setContent(name, page)
  }

  private def _flush_cache(
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): Unit = {
    val flusher = new CacheFlusher(ctx)
    p.traverse(flusher)
  }

  private def _config(
    inconfig: Config,
    realm: Realm,
    configname: Option[String]
  )(implicit ctx: I18NContext): DoxSite.Config = {
    _config(realm, configname) + inconfig
  }

  private def _config(
    realm: Realm,
    configname: Option[String]
  )(implicit ctx: I18NContext): DoxSite.Config =
    configname match {
      case Some("site") => _named_config(realm, "site")
      case Some(n) => _named_config(realm, "site") + _named_config(realm, n)
      case None => DoxSite.Config.default
    }

  private def _named_config(
    realm: Realm,
    name: String
  )(implicit ctx: I18NContext): DoxSite.Config = {
      val c = for {
        json <- ConfigLoader.loadConfigJson(realm, name)
        simplemodelingOrg <- _simplemodeling_org(json)
        output <- _tree_transformer_config(json.hcursor.downField("output").focus)
        siteMetadata <- _site_metadata(json.hcursor.downField("site").downField("metadata").focus)
        siteNavigation <- _site_navigation(json.hcursor.downField("site").downField("navigation").focus)
        siteOutput <- _site_output(json.hcursor.downField("site").downField("output").focus)
        siteHeader <- _site_header(json.hcursor.downField("site").downField("header").focus)
      } yield {
        val config = Config(
          None,
          None,
          output,
          siteMetadata = siteMetadata,
          siteNavigation = siteNavigation,
          siteOutput = siteOutput,
          siteHeader = siteHeader,
          simplemodelingOrg = simplemodelingOrg,
          origin = realm.origin
        )
        if (simplemodelingOrg)
          config.withSimpleModelingOrgCompatibility
        else
          config
      }
      c.take
    }

  private def _simplemodeling_org(json: Json): Consequence[Boolean] =
    json.hcursor.downField("simplemodelingorg").focus match {
      case Some(s) => Consequence run {
        s.as[Boolean] match {
          case Right(r) => Consequence.success(r)
          case Left(l) => Consequence.syntaxErrorFault(l.toString)
        }
      }
      case None => Consequence.success(false)
    }

  private def _site_metadata(json: Option[Json]): Consequence[SiteMetadata] =
    json match {
      case Some(s) => _site_metadata(s)
      case None => Consequence.success(SiteMetadata.empty)
    }

  private def _site_metadata(json: Json): Consequence[SiteMetadata] =
    Consequence run {
      json.as[SiteMetadata] match {
        case Right(r) => Consequence.success(r)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_navigation(json: Option[Json]): Consequence[Config.SiteNavigation] =
    json match {
      case Some(s) => _site_navigation(s)
      case None => Consequence.success(Config.SiteNavigation.default)
    }

  private def _site_navigation(json: Json): Consequence[Config.SiteNavigation] =
    Consequence run {
      val cursor = json.hcursor
      cursor.downField("mode").as[Option[Config.SiteNavigation.Mode]] match {
        case Right(mode) => Consequence.success(Config.SiteNavigation(mode.getOrElse(Config.SiteNavigation.default.mode)))
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_output(json: Option[Json]): Consequence[Config.SiteOutput] =
    json match {
      case Some(s) => _site_output(s)
      case None => Consequence.success(Config.SiteOutput.default)
    }

  private def _site_output(json: Json): Consequence[Config.SiteOutput] =
    Consequence run {
      val cursor = json.hcursor
      val r = for {
        localeMode <- cursor.downField("locale_mode").as[Option[Config.SiteOutput.LocaleMode]]
        defaultLocale <- cursor.downField("default_locale").as[Option[String]]
      } yield Config.SiteOutput(
        localeMode.getOrElse(Config.SiteOutput.default.localeMode),
        defaultLocale.getOrElse(Config.SiteOutput.default.defaultLocale)
      )
      r match {
        case Right(s) => Consequence.success(s)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _site_header(json: Option[Json]): Consequence[Config.SiteHeader] =
    json match {
      case Some(s) => _site_header(s)
      case None => Consequence.success(Config.SiteHeader.default)
    }

  private def _site_header(json: Json): Consequence[Config.SiteHeader] =
    Consequence run {
      json.hcursor.downField("language_toggle").as[Option[Boolean]] match {
        case Right(s) => Consequence.success(Config.SiteHeader(s.getOrElse(Config.SiteHeader.default.languageToggle)))
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }

  private def _tree_transformer_config(json: Option[Json]): Consequence[Option[TreeTransformer.Config]] =
    json match {
      case Some(s) => _tree_transformer_config(s)
      case None => Consequence.success(None)
    }

  private def _tree_transformer_config(json: Json): Consequence[Option[TreeTransformer.Config]] = {
    json.hcursor.downField("scope").downField("policy").focus match {
      case Some(s) => s.as[TreeTransformer.Config.Scope.Policy] match {
        case Right(r) => Consequence.success(Some(TreeTransformer.Config(TreeTransformer.Config.Scope(r))))
        case Left(l) => Consequence.success(None)
      }
      case None => Consequence.success(None)
    }
  }

  private def _doxsite_transformer_config(json: Option[Json]): Consequence[DoxSiteTransformer.Config] =
    json match {
      case Some(s) => _doxsite_transformer_config(s)
      case None => Consequence.success(DoxSiteTransformer.Config.default)
    }

  private def _doxsite_transformer_config(json: Json): Consequence[DoxSiteTransformer.Config] =
    Consequence run {
      json.as[DoxSiteTransformer.Config] match {
        case Right(r) => Consequence.success(r)
        case Left(l) => Consequence.syntaxErrorFault(l.toString)
      }
    }
    // configname.fold(DoxSiteTransformer.Config.default) { n =>
    //   val c = for {
    //     json <- ConfigLoader.loadConfigJson(realm, n)
    //     r <- Consequence run {
    //       json.as[DoxSiteTransformer.Config] match {
    //         case Right(r) => Consequence.success(r)
    //         case Left(l) => Consequence.syntaxErrorFault(l.toString)
    //       }
    //     }
    //   } yield r
    //   c.take
    // }
}
