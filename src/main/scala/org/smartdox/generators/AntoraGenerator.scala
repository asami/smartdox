package org.smartdox.generators

import scala.util.{Try, Success, Failure}
import java.io.File
import java.util.Locale
import java.net.{URL, URI}
import java.nio.file.{Paths, Path}
import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.Realm.FileData
import org.goldenport.realm.RealmMaker
import org.goldenport.tree.Tree
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeVisitor
import org.goldenport.tree.TreeTransformer
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.tree.StringBuildVisitor
import org.goldenport.datatype.{Name, Title}
import org.goldenport.datatype.I18NTitle
import org.goldenport.values.Version
import org.goldenport.values.PathName
import org.goldenport.collection.NonEmptyVector
import org.goldenport.i18n.I18NContext
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.LocaleUtils
import org.goldenport.io.UrlUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils
import org.goldenport.util.ListUtils
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => GeneratorContext, _}
import org.smartdox.doxsite.DoxSite
import org.smartdox.doxsite.{Node, Page, MetaDataNode}
import org.smartdox.doxsite.ImageNode
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.PublishMetadata
import org.smartdox.converters.Dox2AsciidocConverter
import org.smartdox.transformers.LanguageFilterTransformer
import org.smartdox.transformers.DoxTreeNormalizationTransformer
import org.smartdox.service.operations.AntoraOperationClass.AntoraCommand

/*
 * @since   Apr. 18, 2025
 *  version Apr. 28, 2025
 *  version May. 23, 2025
 *  version Jun. 29, 2025
 *  version Jul. 27, 2025
 *  version Aug. 17, 2025
 *  version Oct. 15, 2025
 *  version Nov. 17, 2025
 *  version May. 14, 2026
 *  version Jun. 21, 2026
 *  version Jul. 13, 2026
 * @version Aug.  4, 2026
 * @author  ASAMI, Tomoharu
 */
class AntoraGenerator(
  val context: GeneratorContext,
  val config: DoxSite.Config,
  val publication: Option[File] = None
) extends GeneratorBase {
  import AntoraGenerator._

  def generate(realm: Realm): Realm = {
    val publishmetadata = PublishMetadata.load(publication)
    val extrapages = publishmetadata.map(_.generatedPages).getOrElse(Vector.empty)
    val videopublications = publishmetadata.map(_.videoPublications).getOrElse(Vector.empty)
    val articlemediaprojection = publishmetadata.map(_.articleMediaProjection)
    val site = DoxSite.create(context, realm, Some("antora"), config, extrapages, videopublications, Nil, articlemediaprojection)
    // record_message("XXX")
    val builder = new Builder(Builder.Config(site.config, site.metadata))
    // record_info("INFO")
    site.traverse(builder)
    val antora = builder.build()
    // record_message("YYY")
    val actx = Context(context, site.config)
    val out = antora.toRealm(articlemediaprojection)(actx)
    val r = Realm.create() // .withGitInitAndCommit("antora.d/docs")
    r.merge("antora.d", out)
  }
}

object AntoraGenerator {
  case class Context(
    context: GeneratorContext,
    config: DoxSite.Config
  ) {
    def targetI18NContext = context.targetI18NContext
    def targetI18NContextOption = context.targetI18NContextOption
    def realmContext = context.realmContext
    def doxContext = context.doxContext
    def isDiagramGeneration(p: Page): Boolean = config.strategy.isDiagramGeneration(p)

    def locale = targetI18NContext.locale

    def getDefaultAuthor: Option[I18NString] = config.siteDefaultAuthor

    def configuredLocales: List[Locale] =
      config.siteMetadata.inLanguage.toList.flatMap(_to_locale) match {
        case Nil => List(LocaleUtils.en, LocaleUtils.ja)
        case xs => xs
      }

    def defaultLocale: Locale =
      _to_locale(config.siteOutput.defaultLocale).orElse(configuredLocales.headOption).getOrElse(LocaleUtils.ja)

    private def _to_locale(p: String): Option[Locale] =
      p.toLowerCase match {
        case "en" => Some(LocaleUtils.en)
        case "ja" => Some(LocaleUtils.ja)
        case "en-us" => Some(Locale.US)
        case "ja-jp" => Some(Locale.JAPAN)
        case _ => None
      }

    def withTargetI18NContext(locale: Locale) =
      copy(context = context.withTargetI18NContext(locale))
  }

  case class Antora(
    playbook: Antora.Playbook,
    components: List[Antora.Component]
  ) {
    def toRealm(implicit context: Context): Realm = toRealm(None)

    def toRealm(
      articleMediaProjection: Option[PublishMetadata.ArticleMediaProjection]
    )(implicit context: Context): Realm = {
      context.config.siteOutput.localeMode match {
        case DoxSite.Config.SiteOutput.LocaleMode.SingleLocaleRoot =>
          _build_single_locale_root(articleMediaProjection)(context.withTargetI18NContext(context.defaultLocale))
        case DoxSite.Config.SiteOutput.LocaleMode.MultiLocaleSubdirs => context.configuredLocales match {
          case Nil => _build_plain(articleMediaProjection)
          case xs => _build_multi(context, xs, articleMediaProjection)
        }
      }
    }

    private def _build_plain(
      articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
    )(implicit context: Context): Realm = {
      val realm = Realm.create()
      realm.setContent("antora-playbook.yml", playbook.serialize())
      realm.setNode("docs")
      val cursor = realm.takeCursor("docs")
      for (c <- components) {
        c.export(cursor, articlemediaprojection)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _build_multi(
      context: Context,
      locales: List[Locale],
      articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
    ) = {
      val realm = Realm.create()
      locales.foldLeft(realm) { (z, locale) =>
        z.merge(locale.toString, _build_locale(articlemediaprojection)(context.withTargetI18NContext(locale)))
      }
    }

    private def _build_locale(
      articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
    )(
      implicit context: Context
    ) = {
      val locale = context.targetI18NContext.locale
      val pb0 = playbook.
        withLang(context.locale).
        withAntoraCacheDir(s"../../antora-cache.d/${locale}").
        withKrokiCacheDir(s"../../kroki-cache.d")
      val realm = Realm.create()
      val pb = _setup_supplemental_ui(realm, pb0)
      realm.setContent("antora-playbook.yml", pb.serialize())
      realm.setNode("docs")
      val xs = components.map(_.canonize(context))
      val cursor = realm.takeCursor("docs")
      for (c <- xs) {
        c.export(cursor, articlemediaprojection)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _build_single_locale_root(
      articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
    )(
      implicit context: Context
    ) = {
      val locale = context.targetI18NContext.locale
      val pb0 = playbook.
        withAntoraCacheDir(s"../antora-cache.d/${locale}").
        withKrokiCacheDir(s"../kroki-cache.d")
      val realm = Realm.create()
      val pb = _setup_supplemental_ui(realm, pb0)
      realm.setContent("antora-playbook.yml", pb.serialize())
      realm.setNode("docs")
      val xs = components.map(_.canonize(context))
      val cursor = realm.takeCursor("docs")
      for (c <- xs) {
        c.export(cursor, articlemediaprojection)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _setup_supplemental_ui(
      realm: Realm,
      playbook: Antora.Playbook
    )(implicit context: Context): Antora.Playbook =
      context.config.siteNavigation.mode match {
        case DoxSite.Config.SiteNavigation.Mode.Category =>
          realm.setContent("supplemental-ui/partials/header-content.hbs", _category_header_content(context.config.siteHeader.languageToggle))
          playbook.copy(ui = playbook.ui.withSupplementalFiles(new URI("./supplemental-ui")))
        case DoxSite.Config.SiteNavigation.Mode.SimpleModeling =>
          playbook
      }

    private def _category_header_content(languagetoggle: Boolean): String = {
      val toggle =
        if (languagetoggle)
          """        <div class="navbar-item">
            |          <div class="lang-toggle-wrapper">
            |            <div class="lang-toggle">
            |              <a class="lang-btn" data-lang="ja" href="{{siteRootPath}}/ja/">JA</a>
            |              <a class="lang-btn" data-lang="en" href="{{siteRootPath}}/en/">EN</a>
            |            </div>
            |          </div>
            |        </div>
            |""".stripMargin
        else
          ""
      s"""<header class="header">
         |  <nav class="navbar">
         |    <div class="navbar-brand">
         |      <a class="navbar-item" href="{{{or site.url siteRootPath}}}/">{{site.title}}</a>
         |      {{#if env.SITE_SEARCH_PROVIDER}}
         |      <div class="navbar-item search hide-for-print">
         |        <div id="search-field" class="field">
         |          <input id="search-input" type="text" placeholder="Search the docs"{{#if page.home}} autofocus{{/if}}>
         |        </div>
         |      </div>
         |      {{/if}}
         |      <button class="navbar-burger" aria-controls="topbar-nav" aria-expanded="false" aria-label="Toggle main menu">
         |        <span></span>
         |        <span></span>
         |        <span></span>
         |      </button>
         |    </div>
         |    <div id="topbar-nav" class="navbar-menu">
         |      <div class="navbar-end">
         |        <a class="navbar-item" href="{{siteRootPath}}/index.html">Home</a>
         |        {{#each site.components}}
         |        <a class="navbar-item" href="{{{relativize ./url}}}">{{{./title}}}</a>
         |        {{/each}}
         |$toggle      </div>
         |    </div>
         |  </nav>
         |</header>
         |""".stripMargin
    }

    // private def _build_en(
    //   implicit context: Context
    // ) = {
    //   val realm = Realm.create()
    //   realm.setContent("antora-playbook.yml", playbook.serialize())
    //   realm.setNode("docs")
    //   val cursor = realm.takeCursor("docs")
    //   for (c <- components) {
    //     c.export(cursor)
    //   }
    //   realm.withGitInitAndCommit("docs")
    // }

    // private def _build_ja(
    //   implicit context: Context
    // ) = {
    //   val realm = Realm.create()
    //   realm.setContent("antora-playbook.yml", playbook.serialize())
    //   realm.setNode("docs")
    //   val cursor = realm.takeCursor("docs")
    //   for (c <- components) {
    //     c.export(cursor)
    //   }
    //   realm.withGitInitAndCommit("docs")
    // }
  }
  object Antora {
    import io.circe.Decoder
    import io.circe.HCursor
    import io.circe.Encoder
    import io.circe.Json
    import io.circe.syntax._
    import io.circe.generic.semiauto._
    import cats.syntax.either._
    import CirceUtils.Codec._

    implicit val titleDecoder: Decoder[Title] = Decoder.decodeString.emap { s =>
      Try(Title(s)) match {
        case Success(s) => Right(s)
        case Failure(e) => Left(e.toString)
      }
    }

    implicit val referenceDecoder: Decoder[Reference] =
      Decoder.decodeString.emap(x => Right(Reference(x)))

    implicit val siteDecoder: Decoder[Playbook.Site] = new Decoder[Playbook.Site] {
      def apply(c: HCursor): Decoder.Result[Playbook.Site] =
        for {
          title <- c.downField("title").as[I18NString]
          startpage <- c.downField("start_page").as[Option[Reference]]
          url <- c.downField("url").as[Option[URL]]
        } yield Playbook.Site(I18NTitle(title), startpage, url)
    }

    implicit val contentSourceDecoder: Decoder[Playbook.Content.Source] = new Decoder[Playbook.Content.Source] {
      def apply(c: HCursor): Decoder.Result[Playbook.Content.Source] =
        for {
          uri <- c.downField("url").as[String]
          startpath <- c.downField("start_path").as[String]
        } yield Playbook.Content.Source(new URI(uri), Paths.get(startpath))
    }

    implicit val contentDecoder: Decoder[Playbook.Content] = new Decoder[Playbook.Content] {
      def apply(c: HCursor): Decoder.Result[Playbook.Content] =
        for {
          ss <- c.downField("sources").as[List[Playbook.Content.Source]]
        } yield Playbook.Content(ss)
    }

    implicit val redirectsDecoder: Decoder[Playbook.Redirects] = new Decoder[Playbook.Redirects] {
      def apply(c: HCursor): Decoder.Result[Playbook.Redirects] =
        for {
          enable <- c.downField("enable").as[Boolean]
        } yield Playbook.Redirects(enable)
    }

    implicit val uiBundleDecoder: Decoder[Playbook.Ui.Bundle] = new Decoder[Playbook.Ui.Bundle] {
      def apply(c: HCursor): Decoder.Result[Playbook.Ui.Bundle] =
        for {
          url <- c.downField("url").as[String]
        } yield Playbook.Ui.Bundle(new URI(url))
    }

    implicit val uiDecoder: Decoder[Playbook.Ui] = new Decoder[Playbook.Ui] {
      def apply(c: HCursor): Decoder.Result[Playbook.Ui] =
        for {
          b <- c.downField("bundle").as[Playbook.Ui.Bundle]
          s <- c.downField("supplemental_files").as[Option[URI]]
        } yield Playbook.Ui(b, s)
    }

    implicit val outputDecoder: Decoder[Playbook.Output] = new Decoder[Playbook.Output] {
      def apply(c: HCursor): Decoder.Result[Playbook.Output] =
        for {
          dir <- c.downField("dir").as[String]
        } yield Playbook.Output(Paths.get(dir))
    }

    implicit val asciidocDecoder: Decoder[Playbook.Asciidoc] = deriveDecoder[Playbook.Asciidoc]

    implicit val runtimeDecoder: Decoder[Playbook.Runtime] = deriveDecoder[Playbook.Runtime]

    implicit val playbookDecoder: Decoder[Playbook] = new Decoder[Playbook] {
      def apply(c: HCursor): Decoder.Result[Playbook] =
        for {
          site <- c.downField("site").as[Playbook.Site]
          content <- c.downField("content").as[Playbook.Content]
//          redirects <- c.downField("redirects").as[Playbook.Redirects]
          ui <- c.downField("ui").as[Playbook.Ui]
          output <- c.downField("output").as[Playbook.Output]
          asciidoc <- c.downField("asciidoc").as[Playbook.Asciidoc]
          runtime <- c.downField("runtime").as[Option[Playbook.Runtime]]
        } yield Playbook(site, content, ui, output, asciidoc, runtime)
    }

    implicit val siteEncoder: Encoder[Playbook.Site] = new Encoder[Playbook.Site] {
      def apply(p: Playbook.Site): Json =
        CirceUtils.toJson(
          "title" -> p.title.distillDefault,
          "start_page" -> p.start_page.map(_.path),
          "url" -> p.url.map(_.toString)
        )
    }

    implicit val sourceEncoder: Encoder[Playbook.Content.Source] = new Encoder[Playbook.Content.Source] {
      def apply(p: Playbook.Content.Source): Json =
        CirceUtils.toJson(
          "url" -> p.url,
          "start_path" -> p.start_path
        )
    }

    implicit val contentEncoder: Encoder[Playbook.Content] = new Encoder[Playbook.Content] {
      def apply(p: Playbook.Content): Json =
        CirceUtils.toJson(
          "sources" -> p.sources.map(_.asJson)
        )
    }

    // implicit val redirectsEncoder: Encoder[Playbook.Redirects] = new Encoder[Playbook.Redirects] {
    //   def apply(p: Playbook.Redirects): Json =
    //     CirceUtils.toJson(
    //       "enable" -> p.enable.asJson
    //     )
    // }

    implicit val bundleEncoder: Encoder[Playbook.Ui.Bundle] = new Encoder[Playbook.Ui.Bundle] {
      def apply(p: Playbook.Ui.Bundle): Json =
        CirceUtils.toJson(
          "url" -> p.url
        )
    }

    implicit val uiEncoder: Encoder[Playbook.Ui] = new Encoder[Playbook.Ui] {
      def apply(p: Playbook.Ui): Json =
        CirceUtils.toJson(
          "bundle" -> p.bundle.asJson,
          "supplemental_files" -> p.supplemental_files
        )
    }

    implicit val outputEncoder: Encoder[Playbook.Output] = new Encoder[Playbook.Output] {
      def apply(p: Playbook.Output): Json =
        CirceUtils.toJson(
          "dir" -> p.dir
        )
    }

    implicit val asciidocEncoder: Encoder[Playbook.Asciidoc] = deriveEncoder

    implicit val runtimeEncoder: Encoder[Playbook.Runtime] = deriveEncoder

    implicit val playbookEncoder: Encoder[Playbook] = new Encoder[Playbook] {
      def apply(p: Playbook): Json = Json.obj(
        "site" -> p.site.asJson,
        "content" -> p.content.asJson,
//        "redirects" -> p.redirects.asJson,
        "ui" -> p.ui.asJson,
        "output" -> p.output.asJson,
        "asciidoc" -> p.asciidoc.asJson,
        "runtime" -> p.runtime.asJson
      )
    }

    case class Reference(path: String)

    case class Playbook(
      site: Playbook.Site,
      content: Playbook.Content,
//      redirects: Playbook.Redirects,
      ui: Playbook.Ui,
      output: Playbook.Output,
      asciidoc: Playbook.Asciidoc,
      runtime: Option[Playbook.Runtime]
    ) {
      def withLang(p: Locale): Playbook = copy(site = site.withLang(p))

      def withAntoraCacheDir(p: String): Playbook = {
        val a = runtime match {
          case Some(s) => s.withCacheDir(p)
          case None => Playbook.Runtime(Some(p))
        }
        copy(runtime = Some(a))
      }

      def withKrokiCacheDir(p: String): Playbook =
        copy(asciidoc = asciidoc.withKrokiCacheDir(p))

      def serialize(): String = CirceUtils.toYamlString(this.asJson)
    }
    object Playbook {
      case class Site(
        title: I18NTitle,
        start_page: Option[Reference],
        url: Option[URL] = None
      ) {
        def withLang(p: Locale) = url match {
          case Some(s) => copy(url = Some(UrlUtils.addPathAuto(s, p.toString)))
          case None => this
        }
      }
      object Site {
      }

      case class Content(
        sources: List[Content.Source]
      )
      case class Redirects(
        enable: Boolean
      )
      object Content {
        case class Source(
          url: URI,
          start_path: Path
        )

        def apply(p: Source, ps: Source*): Content = Content(
          p :: ps.toList
        )
      }

      case class Ui(
        bundle: Ui.Bundle,
        supplemental_files: Option[URI] = None
      ) {
        def withSupplementalFiles(p: URI): Ui = copy(supplemental_files = Some(p))
      }
      object Ui {
//        val default = Ui(Bundle(new URI("https://gitlab.com/antora/antora-ui-default/-/jobs/artifacts/master/raw/build/ui-bundle.zip?job=bundle-stable").toURL))
        val default = Ui(Bundle(new URI("./ui-bundle.zip")))

        case class Bundle(
          url: URI
        )
      }

      case class Output(
        dir: Path
      )
      object Output {
        val default = Output(Paths.get("./build/site"))
      }

      case class Asciidoc(
        extensions: List[String] = List("asciidoctor-kroki"),
        attributes: Map[String, String] = Asciidoc.defaultAttributes
      ) {
        def withKrokiCacheDir(p: String): Asciidoc =
          copy(attributes = attributes + ("kroki-fetch-diagram-dir" -> p))
      }
      object Asciidoc {
        val defaultAttributes: Map[String, String] = Map(
          "kroki-server-url" -> "http://localhost:9609", // "https://kroki.io",
          "kroki-default-format" -> "svg",
          "kroki-fetch-diagram" -> "true"
        )
      }

      case class Runtime(
        cache_dir: Option[String]
      ) {
        def withCacheDir(p: String) = copy(cache_dir = Some(p))
      }
    }

    case class Component(
      name: Name,
      title: Option[I18NTitle],
      version: Option[Version],
      modules: NonEmptyVector[Module]
    ) {
      def homePage: Name = Name("index.adoc")

      def hasHomePage: Boolean =
        modules.vector.find(_.isRoot).exists(_.containsPage("index.dox"))

      def canonize(ctx: Context): Component =
        copy(modules = modules.map(_.canonize(ctx)))

      def isNoPages: Boolean = modules.vector.forall(_.isNoPages)

      def export(
        c: Realm.Cursor
      )(implicit context: Context): Unit = export(c, None)

      def export(
        c: Realm.Cursor,
        articleMediaProjection: Option[PublishMetadata.ArticleMediaProjection]
      )(implicit context: Context): Unit =
        ExportFunction(context).apply(c, articleMediaProjection)

      case class ExportFunction(
        context: Context
      ) extends Function1[Realm.Cursor, Unit] {

        private def _newline = "\n"
        private def _locale = context.targetI18NContextOption.map(_.locale) getOrElse LocaleUtils.C

        def apply(c: Realm.Cursor): Unit = apply(c, None)

        def apply(
          c: Realm.Cursor,
          articleMediaProjection: Option[PublishMetadata.ArticleMediaProjection]
        ): Unit = {
          val cc = c.enter(name.name)
          cc.set("antora.yml", _make_meta_yaml)
          val ccc = cc.enter("modules")
          modules.vector.foreach(_export(ccc, _, articleMediaProjection))
        }

        private def _export(
          c: Realm.Cursor,
          p: Module,
          articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
        ) = {
          val cc = c.enter(p.name.name)
          cc.set("nav.adoc", _make_nav_adoc(p))
          if (p.isRoot) {
            _export_files(cc, p, articlemediaprojection)
          } else {
            _export_files(cc, p, articlemediaprojection)
          }
        }

        private def _export_files(
          c: Realm.Cursor,
          p: Module,
          articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
        ) = {
          p.ingredients.vector foreach {
            case m: Module.Ingredient.Pages => _export_pages(c, p, m, articlemediaprojection)
            case m: Module.Ingredient.Images => _export_images(c, m)
            case m: Module.Ingredient.Container => _export_container(c, m)
          }
        }

        private def _export_pages(
          c: Realm.Cursor,
          module: Module,
          p: Module.Ingredient.Pages,
          articlemediaprojection: Option[PublishMetadata.ArticleMediaProjection]
        ) = {
          val tf = new RealmMaker.Transformer[Page] {
            def treeTransformerContext = context.realmContext

            override protected def make_Node(
              node: TreeNode[Page],
              content: Page
            ): TreeTransformer.Directive[Realm.Data] = {
              val isdiagram = context.isDiagramGeneration(content)
              val ctx = Dox2AsciidocConverter.Context(context, isdiagram)
              val da = new Dox2AsciidocConverter(ctx)
              val dox = DoxSite.projectArticleMedia(content.dox, _article_media_path(module, node.pathname), context.locale, articlemediaprojection)
              val r = da.convert(dox)
              val s = r.fold(_.message, identity)
              val name = StringUtils.changeSuffix(node.name, "adoc")
              directive_leaf(name, StringData(s))
            }
          }
          val realm = RealmMaker.make(p.pages, tf)
          c.merge(p.name.name, realm)
        }

        private def _article_media_path(module: Module, pathname: String): String = {
          val segments = Vector(name.name) ++
            (if (module.isRoot) Vector.empty else Vector(module.name.name)) ++
            pathname.split('/').toVector.filter(_.nonEmpty)
          segments.mkString("/")
        }

        private def _export_images(c: Realm.Cursor, p: Module.Ingredient.Images) = {
          val tf = new RealmMaker.Transformer[ImageNode] {
            def treeTransformerContext = context.realmContext

            override protected def make_Node(
              node: TreeNode[ImageNode],
              content: ImageNode
            ): TreeTransformer.Directive[Realm.Data] = {
              directive_leaf(node.name, FileData(content.file))
            }
          }
          val realm = RealmMaker.make(p.images, tf)
          c.merge(p.name.name, realm)
        }

        private def _export_container(c: Realm.Cursor, p: Module.Ingredient.Container) =
          c.merge(p.name.name, p.realm)

        private def _make_meta_yaml: String =
          CirceUtils.toYamlString(
            "name" -> name.name,
            "title" -> title.map(_.distill(_locale)),
            "version" -> version.map(_.v).getOrElse(null),
            "nav" -> _nav
          )

        private def _nav = modules.map(x => s"modules/${x.name}/nav.adoc")

        private def _make_nav_adoc(p: Module): String =
          if (p.isRoot)
            _make_nav_adoc_root(p)
          else
            _make_nav_adoc_module(p)

        class NavMaker(name: Option[String]) extends StringBuildVisitor[Module.Navigation.Reference] {
          override def sb_indent_mark = "*"
          override def sb_indent_post_mark = " "
          override def sb_indent_size = 1

          name foreach  { x =>
            sb_enter()
            sb_println(x)
          }

          def make(): String = sb_to_string()

          override def enter_Container(
            node: TreeNode[Module.Navigation.Reference]
          ) {
            val title = _container_title(node)
            sb_println(title)
          }

          private def _container_title(
            node: TreeNode[Module.Navigation.Reference]
          ): String =
            node.children.find(_.pathname.endsWith("/index.dox")).flatMap(_.getContent).
              map(_.title.distill(_locale)).
              getOrElse(StringUtils.makeTitleFromPathname(node.pathname))

          override def enter_Content(
            node: TreeNode[Module.Navigation.Reference],
            content: Module.Navigation.Reference
          ) {
            val filepath = StringUtils.changeSuffix(content.pathname.v.dropWhile(_ == '/'), "adoc")
            val title = content.title.distill(_locale)
            val s = s"xref:${filepath}[${title}]"
            sb_println(s)
          }
        }

        private def _make_nav_adoc_root(p: Module): String = {
          val maker = new NavMaker(None)
          p.navigation.references.traverse(maker)
          maker.make()
        }

        // private def _make_nav_adoc_root_old(p: Module): String = {
        //   p.navigation.references.map { x =>
        //     val filepath = StringUtils.changeSuffix(x.path.dropWhile(_ == '/'), "adoc")
        //     val title = x.title
        //     s"* xref:${filepath}[${title}]"
        //   }.mkString("", _newline, _newline)
        // }

        private def _make_nav_adoc_module(p: Module): String = {
          val maker = new NavMaker(Some(p.name.name))
          p.navigation.references.traverse(maker)
          maker.make()
        }
      }
    }
    object Component {
      class Builder(
        name: Name,
        title: Option[I18NTitle]
      ) {
        private val _root = new Module.Builder("ROOT")
        private var _modules: Vector[Module] = Vector.empty

        def build(): Component = Component(
          name,
          title,
          None,
          NonEmptyVector(_root.build(), _modules)
        )

        def addNode(node: Node) = {
          _root.addNode(node)
          this
        }

        def addModule(module: Module) = {
          _modules = _modules :+ module
          this
        }
      }
      object Builder {
        def apply(
          name: String
        ): Builder = new Builder(Name(name), None)

        def apply(
          name: String,
          title: I18NString
        ): Builder =
          new Builder(Name(name), Some(I18NTitle(title)))
      }
    }

    case class Module(
      name: Name,
      ingredients: NonEmptyVector[Module.Ingredient],
      navigation: Module.Navigation
    ) {
      def isRoot = name.name == "ROOT"

      def isNoPages: Boolean = ingredients.vector.forall(_.isNoPages)

      def containsPage(path: String): Boolean =
        navigation.references.collectContent {
          case x => x
        }.exists(_.pathname.v.split('/').lastOption.contains(path))

      def canonize(ctx: Context) = copy(ingredients = ingredients.map(_.canonize(ctx)))
    }
    object Module {
      sealed trait Ingredient {
        def name: Name
        def isNoPages: Boolean
        def canonize(ctx: Context): Ingredient
      }
      object Ingredient {
        case class Pages(pages: Tree[Page] = Tree.create()) extends Ingredient {
          val name = Name("pages")

          lazy val isNoPages = {
            val a = pages.collectContent {
              case m => m
            }
            a.isEmpty
          }

          def add(page: Page) = {
            val path = page.name.name
            pages.setContent(path, page)
            this
          }

          case class PagesCanonizeTransformer(
            ctx: Context
          ) extends HomoTreeTransformer[Page] {
            def treeTransformerContext = ctx.doxContext.toContext[Page]
            private val _context = ctx.doxContext

            override def make_Node(node: TreeNode[Page], content: Page): TreeTransformer.Directive[Page] = {
              val t = new LanguageFilterTransformer(_context)
//              val nt = new DoxTreeNormalizationTransformer(_context)
              val a = Dox.toTree(content.dox)
              val b = a.transform(t)
//              val b0 = b.transform(nt)
              val c = Dox.toDox(b)
              directive_leaf(content.copy(dox = Dox.toDocument(c)))
            }
          }

          def canonize(ctx: Context) = {
            val a = pages.transform(PagesCanonizeTransformer(ctx))
            copy(pages = a)
          }
        }
        case class Images(images: Tree[ImageNode] = Tree.create()) extends Ingredient {
          val name = Name("images")

          def isNoPages = true

          def canonize(ctx: Context) = this

          def add(image: ImageNode) = {
            val path = image.name.name
            images.setContent(path, image)
            this
          }
        }
        case class Container(name: Name, realm: Realm = Realm.create()) extends Ingredient {
          def isNoPages = true

          def canonize(ctx: Context) = this
        }
      }

      case class Navigation(
        references: Tree[Navigation.Reference] = Tree.create()
      )
      object Navigation {
        case class Reference(pathname: PathName, title: I18NString)

        val empty = Navigation()
      }

      class Builder(name: String) {
        private var _name: Name = Name(name)
        private var _nodes: Vector[Node] = Vector.empty

        def build(): Module = {
          val xs = _ingredients()
          val nav = _navigation(xs)
          Module(_name, xs, nav)
        }

        private def _ingredients(): NonEmptyVector[Ingredient] = {
          case class Z(
            pages: Ingredient.Pages = Ingredient.Pages(),
            images: Ingredient.Images = Ingredient.Images()
          ) {
            def r: NonEmptyVector[Ingredient] = NonEmptyVector.create(pages, images)

            def +(rhs: Node) = rhs match {
              case m: Page => copy(pages = pages add m)
              case m: MetaDataNode => this
              case m: ImageNode => copy(images = images add m)
            }
          }
          _nodes.foldLeft(Z())(_+_).r
        }

        private def _navigation(ps: NonEmptyVector[Ingredient]): Navigation =
          ps.vector.collect {
            case m: Ingredient.Pages => _navigation(m)
          }.headOption.getOrElse(Navigation.empty)

        private def _navigation(p: Ingredient.Pages): Navigation = {
          case class Slot(pathname: String, title: I18NString)

          class Collector() extends TreeVisitor[Page] {
            private var _pages: Vector[Slot] = Vector.empty

            def toNavigation: Navigation = {
              val tree = Tree.create[Navigation.Reference]()
              _pages.foreach { x =>
                tree.setContent(x.pathname, Navigation.Reference(PathName(x.pathname), x.title))
              }
              Navigation(tree)
            }

            override def enter(node: TreeNode[Page]) {
              for (c <- node.getContent) {
                val title = Dox.getTitleI18NString(c.dox) getOrElse {
                  I18NString(StringUtils.makeTitleFromPathname(c.name.name))
                }
                _pages = _pages :+ Slot(node.pathname, title)
              }
            }
          }

          val collector = new Collector()
          p.pages.traverse(collector)
          collector.toNavigation
        }

        def addNode(node: Node) = {
          _nodes = _nodes :+ node
          this
        }
      }
    }

    class Builder(config: Builder.Config) {
      private var _playbook: Option[Playbook] = None
      private var _components: Vector[Component] = Vector.empty
      private var _current_component: Option[Component.Builder] = None
      private var _current_module: Option[Module.Builder] = None

      def build(): Antora = {
        pushModule()
        pushComponent()
        val comps = _components.filter(_is_available).toList
        val pb = _build_playbook(comps)
        Antora(
          pb,
          comps
        )
      }

      private def _build_playbook(comps: List[Component]): Playbook = _playbook getOrElse {
        val title = config.title
        val startpage = comps.find(_.hasHomePage).map { x =>
          val file = x.homePage
          s"${x.name.name}::${file.name}"
        }
        val url = config.url
        val site = Playbook.Site(
          I18NTitle(title),
          startpage.map(Reference.apply),
          url
        )
        val content = Playbook.Content(_sources(comps))
//        val redirects = Playbook.Redirects(false)
        val ui = Playbook.Ui.default
        val output = Playbook.Output.default
        Playbook(site, content, ui, output, Playbook.Asciidoc(attributes = Playbook.Asciidoc.defaultAttributes ++ config.siteAttributes), None)
      }

      private def _sources(comps: List[Component]): List[Playbook.Content.Source] = {
        comps.map { x =>
          val startpath = x.name.name
          Playbook.Content.Source(
            new URI("./docs"),
            Paths.get(startpath)
          )
        }
      }

      private def _is_available(p: Component) = {
        if (true)
          !p.isNoPages
        else
          true
      }

      def setComponent(name: String, title: I18NString) = {
        _current_component.foreach { x =>
          _components = _components :+ x.build()
        }
        _current_component = Some(Component.Builder(name, title))
        this
      }

      def setModule(name: String) = {
        _current_module = Some(new Module.Builder(name))
        this
      }

      def addNode(node: Node) = {
        _current_module match {
          case Some(s) => s.addNode(node)
          case None => _current_component match {
            case Some(ss) => ss.addNode(node)
            case None => RAISE.notImplementedYetDefect
          }
        }
        this
      }

      def pushComponent() = {
        _current_component foreach { x =>
          _components = _components :+ x.build()
          _current_component = None
        }
        this
      }

      def pushModule() = {
        _current_module foreach { x =>
          _current_component match {
            case Some(s) => s.addModule(x.build())
            case None => RAISE.notImplementedYetDefect
          }
          _current_module = None
        }
        this
      }
    }
    object Builder {
      case class Config(
        doxSiteConfig: DoxSite.Config
        // title: String = "SimpleModeling",
        // url: Option[URL] = Some(new URI("https://www.simplemodeling.org").toURL),
        // defaultAuthor: Option[I18NString] = Some(I18NString.enja("ASAMI, Tomoharu", "浅海 智晴"))
      ) {
        def title: String = doxSiteConfig.siteTitle
        def url: Option[URL] = doxSiteConfig.siteUrl
        def defaultAuthor: Option[I18NString] = doxSiteConfig.siteDefaultAuthor
        def siteAttributes: Map[String, String] = Map(
          "smartdox-site-navigation-mode" -> doxSiteConfig.siteNavigation.mode.name,
          "smartdox-site-language-toggle" -> doxSiteConfig.siteHeader.languageToggle.toString
        )
      }
      object Config {
        // val default = Config()
      }
    }
  }

  // case class Config(
  //   component: Component = Component.empty
  // )
  // object Config {
  //   val empty = Config()

  //   case class Component(
  //     includes: List[String] = Nil
  //   )
  //   object Component {
  //     val empty = Component()
  //   }
  // }

  class Builder(
    config: Builder.Config
  ) extends TreeVisitor[Node] {
    private var _depth: Int = 0
    private var _in_images: Boolean = false
    private var _in_work_area: Int = 0
    private var _module_path: Vector[String] = Vector.empty
    private val _antora = new Antora.Builder(Antora.Builder.Config(config.doxSiteConfig))

    private def _effective_depth: Int =
      if (_in_images)
        _depth - 1
      else
        _depth

    def build(): Antora = _antora.build()

    override def enter(node: TreeNode[Node]) = {
      if (node.name.endsWith(".d"))
        _in_work_area = _in_work_area + 1
      if (_in_work_area > 0)
        Unit
      else
        _enter(node)
    }

    private def _enter(node: TreeNode[Node]) = {
      node.getContent match {
        case Some(s) => _effective_depth match {
          case 0 => _at_home(node, s)
          case 1 => _at_component(node, s)
          case 2 => _at_module(node, s)
          case _ => _at_ingredient(node, s)
        }
        case None =>
          _effective_depth match {
            case 0 => _at_home(node)
            case 1 => _at_component(node)
            case 2 => _at_module(node)
            case _ => _at_ingredient(node)
          }
      }
      _depth = _depth + 1
      if (_is_images(node))
        _in_images = true
    }

    private def _at_home(node: TreeNode[Node]): Unit = _antora.setComponent(node.name, _category_title(node.name))

    private def _at_home(node: TreeNode[Node], c: Node): Unit = {}

    private def _at_component(node: TreeNode[Node]): Unit =
      if (!_is_images(node))
        _antora.setModule(node.name)

    private def _at_component(node: TreeNode[Node], c: Node): Unit = _antora.addNode(c)

    private def _at_module(node: TreeNode[Node]): Unit =
      _module_path = _module_path :+ node.name

    private def _at_module(node: TreeNode[Node], c: Node): Unit = _antora.addNode(c)

    private def _at_ingredient(node: TreeNode[Node]): Unit =
      _module_path = _module_path :+ node.name

    private def _at_ingredient(node: TreeNode[Node], c: Node): Unit =
      _antora.addNode(_with_module_path(c))

    override def leave(node: TreeNode[Node]) = {
      if (_in_work_area == 0)
        _leave(node)
      if (node.name.endsWith(".d"))
        _in_work_area = _in_work_area - 1
    }

    private def _leave(node: TreeNode[Node]) = {
      if (_depth == 0)
        RAISE.noReachDefect(s"AntoraGenerator#_leave: $node")
      _depth = _depth - 1
      if (_is_images(node))
        _in_images = false
      node.getContent match {
        case Some(s) => {}
        case None =>
          _effective_depth match {
            case 0 => _return_to_home(node)
            case 1 => _return_to_component(node)
            case 2 => _return_to_module(node)
          case _ => _return_to_ingredient(node)
          }
      }
    }

    private def _return_to_home(node: TreeNode[Node]): Unit = _antora.pushComponent()
    private def _return_to_component(node: TreeNode[Node]): Unit = _antora.pushModule()
    private def _return_to_module(node: TreeNode[Node]): Unit =
      _module_path = _module_path.dropRight(1)
    private def _return_to_ingredient(node: TreeNode[Node]): Unit =
      _module_path = _module_path.dropRight(1)

    private def _category_title(name: String): I18NString =
      I18NString(config.categoryTitle(name))

    private def _is_images(node: TreeNode[Node]) = node.name == "images"

    private def _with_module_path(p: Node): Node =
      if (_module_path.isEmpty)
        p
      else {
        val name = (_module_path :+ p.name.name).mkString("/")
        p match {
          case m: Page => m.copy(name = Node.Name(name))
          case m: ImageNode => m.withName(name)
          case m => m
        }
      }
  }
  object Builder {
    case class Config(
      doxSiteConfig: DoxSite.Config, // XXX migrate to MetaData
      metadata: MetaData
    ) {
      def categoryTitle(name: String): String = metadata.categories.makeTitle(name)
      def title: String = doxSiteConfig.siteTitle
      def url: Option[URL] = doxSiteConfig.siteUrl
      def defaultAuthor: Option[I18NString] = doxSiteConfig.siteDefaultAuthor
    }
  }
}
