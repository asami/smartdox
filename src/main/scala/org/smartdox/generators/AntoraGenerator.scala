package org.smartdox.generators

import scala.util.{Try, Success, Failure}
import java.util.Locale
import java.net.{URL, URI}
import java.nio.file.{Paths, Path}
import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.{Data, ObjectData, StringData}
import org.goldenport.realm.RealmMaker
import org.goldenport.tree.Tree
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeVisitor
import org.goldenport.tree.TreeTransformer
import org.goldenport.tree.HomoTreeTransformer
import org.goldenport.tree.StringBuildVisitor
import org.goldenport.datatype.{Name, Title}
import org.goldenport.values.Version
import org.goldenport.values.PathName
import org.goldenport.collection.NonEmptyVector
import org.goldenport.i18n.LocaleUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils
import org.goldenport.util.ListUtils
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator._
import org.smartdox.doxsite.DoxSite
import org.smartdox.doxsite.{Node, Page, MetaDataNode}
import org.smartdox.metadata.MetaData
import org.smartdox.converters.Dox2AsciidocConverter
import org.smartdox.transformers.LanguageFilterTransformer
import org.smartdox.transformers.DoxTreeNormalizationTransformer
import org.smartdox.service.operations.AntoraOperationClass.AntoraCommand

/*
 * @since   Apr. 18, 2025
 *  version Apr. 28, 2025
 *  version May. 23, 2025
 * @version Jun. 29, 2025
 * @author  ASAMI, Tomoharu
 */
class AntoraGenerator(
  val context: Context,
  val config: DoxSite.Config
) extends GeneratorBase {
  import AntoraGenerator._

  def generate(realm: Realm): Realm = {
    val site = DoxSite.create(context, realm, "antora", config)
    // record_message("XXX")
    val builder = new Builder(Builder.Config(site.metadata))
    // record_info("INFO")
    site.traverse(builder)
    val antora = builder.build()
    // record_message("YYY")
    val out = antora.toRealm(context)
    val r = Realm.create() // .withGitInitAndCommit("antora.d/docs")
    r.merge("antora.d", out)
  }
}

object AntoraGenerator {
  case class Antora(
    playbook: Antora.Playbook,
    components: List[Antora.Component]
  ) {
    def toRealm(implicit context: Context): Realm = {
      val targets = List(LocaleUtils.en, LocaleUtils.ja) // TODO
      targets match {
        case Nil => _build_plain(context)
        case xs => _build_multi(context)
      }
    }

    private def _build_plain(implicit context: Context): Realm = {
      val realm = Realm.create()
      realm.setContent("antora-playbook.yml", playbook.serialize())
      realm.setNode("docs")
      val cursor = realm.takeCursor("docs")
      for (c <- components) {
        c.export(cursor)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _build_multi(
      context: Context
    ) = {
      val en = _build_locale(context.withTargetI18NContext(LocaleUtils.en))
      val ja = _build_locale(context.withTargetI18NContext(LocaleUtils.ja))
      val realm = Realm.create()
      val a = realm.merge("en", en)
      a.merge("ja", ja)
    }

    private def _build_locale(
      implicit context: Context
    ) = {
      val realm = Realm.create()
      realm.setContent("antora-playbook.yml", playbook.serialize())
      realm.setNode("docs")
      val xs = components.map(_.canonize(context))
      val cursor = realm.takeCursor("docs")
      for (c <- xs) {
        c.export(cursor)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _build_en(
      implicit context: Context
    ) = {
      val realm = Realm.create()
      realm.setContent("antora-playbook.yml", playbook.serialize())
      realm.setNode("docs")
      val cursor = realm.takeCursor("docs")
      for (c <- components) {
        c.export(cursor)
      }
      realm.withGitInitAndCommit("docs")
    }

    private def _build_ja(
      implicit context: Context
    ) = {
      val realm = Realm.create()
      realm.setContent("antora-playbook.yml", playbook.serialize())
      realm.setNode("docs")
      val cursor = realm.takeCursor("docs")
      for (c <- components) {
        c.export(cursor)
      }
      realm.withGitInitAndCommit("docs")
    }
  }
  object Antora {
    import io.circe.Decoder
    import io.circe.HCursor
    import io.circe.Encoder
    import io.circe.Json
    import io.circe.syntax._
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
          title <- c.downField("title").as[String]
          startpage <- c.downField("start_page").as[Reference]
          url <- c.downField("url").as[Option[URL]]
        } yield Playbook.Site(Title(title), startpage, url)
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
        } yield Playbook.Ui(b)
    }

    implicit val outputDecoder: Decoder[Playbook.Output] = new Decoder[Playbook.Output] {
      def apply(c: HCursor): Decoder.Result[Playbook.Output] =
        for {
          dir <- c.downField("dir").as[String]
        } yield Playbook.Output(Paths.get(dir))
    }

    implicit val playbookDecoder: Decoder[Playbook] = new Decoder[Playbook] {
      def apply(c: HCursor): Decoder.Result[Playbook] =
        for {
          site <- c.downField("site").as[Playbook.Site]
          content <- c.downField("content").as[Playbook.Content]
//          redirects <- c.downField("redirects").as[Playbook.Redirects]
          ui <- c.downField("ui").as[Playbook.Ui]
          output <- c.downField("output").as[Playbook.Output]
        } yield Playbook(site, content, ui, output)
    }

    implicit val siteEncoder: Encoder[Playbook.Site] = new Encoder[Playbook.Site] {
      def apply(p: Playbook.Site): Json =
        CirceUtils.toJson(
          "title" -> p.title.title,
          "start_page" -> p.start_page.path,
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
          "bundle" -> p.bundle.asJson
        )
    }

    implicit val outputEncoder: Encoder[Playbook.Output] = new Encoder[Playbook.Output] {
      def apply(p: Playbook.Output): Json =
        CirceUtils.toJson(
          "dir" -> p.dir
        )
    }

    implicit val playbookEncoder: Encoder[Playbook] = new Encoder[Playbook] {
      def apply(p: Playbook): Json = Json.obj(
        "site" -> p.site.asJson,
        "content" -> p.content.asJson,
//        "redirects" -> p.redirects.asJson,
        "ui" -> p.ui.asJson,
        "output" -> p.output.asJson
      )
    }

    case class Reference(path: String)

    case class Playbook(
      site: Playbook.Site,
      content: Playbook.Content,
//      redirects: Playbook.Redirects,
      ui: Playbook.Ui,
      output: Playbook.Output
    ) {
      def serialize(): String = CirceUtils.toYamlString(this.asJson)
    }
    object Playbook {
      case class Site(
        title: Title,
        start_page: Reference,
        url: Option[URL] = None
      )
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
        bundle: Ui.Bundle
      )
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
    }

    case class Component(
      name: Name,
      title: Option[Title],
      version: Option[Version],
      modules: NonEmptyVector[Module]
    ) {
      def homePage: Name = Name("index.adoc")

      def canonize(ctx: Context): Component =
        copy(modules = modules.map(_.canonize(ctx)))

      def export(
        c: Realm.Cursor
      )(implicit context: Context): Unit = ExportFunction(context).apply(c)

      case class ExportFunction(context: Context)
          extends Function1[Realm.Cursor, Unit] with Context.Holder {

        private def _newline = "\n"

        def apply(c: Realm.Cursor): Unit = {
          val cc = c.enter(name.name)
          cc.set("antora.yml", _make_meta_yaml)
          val ccc = cc.enter("modules")
          modules.vector.foreach(_export(ccc, _))
        }

        private def _export(c: Realm.Cursor, p: Module) = {
          val cc = c.enter(p.name.name)
          cc.set("nav.adoc", _make_nav_adoc(p))
          if (p.isRoot) {
            _export_files(cc, p)
          } else {
            _export_files(cc, p)
          }
        }

        private def _export_files(c: Realm.Cursor, p: Module) = {
          p.ingredients.vector foreach {
            case m: Module.Ingredient.Pages => _export_pages(c, m)
            case m: Module.Ingredient.Images => _export_images(c, m)
            case m: Module.Ingredient.Container => _export_container(c, m)
          }
        }

        private def _export_pages(c: Realm.Cursor, p: Module.Ingredient.Pages) = {
          val tf = new RealmMaker.Transformer[Page] {
            def treeTransformerContext = context_realm

            override protected def make_Node(
              node: TreeNode[Page],
              content: Page
            ): TreeTransformer.Directive[Realm.Data] = {
              val da = new Dox2AsciidocConverter(context)
              val r = da.transform(content.dox)
              val s = r.fold(_.message, identity)
              val name = StringUtils.changeSuffix(node.name, "adoc")
              directive_leaf(name, StringData(s))
            }
          }
          val realm = RealmMaker.make(p.pages, tf)
          c.merge(p.name.name, realm)
        }

        private def _export_images(c: Realm.Cursor, p: Module.Ingredient.Images) = {
          c.merge(p.name.name, p.realm)
        }

        private def _export_container(c: Realm.Cursor, p: Module.Ingredient.Container) =
          c.merge(p.name.name, p.realm)

        private def _make_meta_yaml: String =
          CirceUtils.toYamlString(
            "name" -> name.name,
            "title" -> title.map(_.title),
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
            val title = StringUtils.makeTitleFromPathname(node.pathname)
            sb_println(title)
          }

          override def enter_Content(
            node: TreeNode[Module.Navigation.Reference],
            content: Module.Navigation.Reference
          ) {
            val filepath = StringUtils.changeSuffix(content.pathname.v.dropWhile(_ == '/'), "adoc")
            val title = content.title
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
      class Builder(name: Name, title: Option[Title]) {
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
        def apply(name: String): Builder = new Builder(Name(name), None)

        def apply(name: String, title: String): Builder =
          new Builder(Name(name), Some(Title(title)))
      }
    }

    case class Module(
      name: Name,
      ingredients: NonEmptyVector[Module.Ingredient],
      navigation: Module.Navigation
    ) {
      def isRoot = name.name == "ROOT"

      def canonize(ctx: Context) = copy(ingredients = ingredients.map(_.canonize(ctx)))
    }
    object Module {
      sealed trait Ingredient {
        def name: Name

        def canonize(ctx: Context): Ingredient
      }
      object Ingredient {
        case class Pages(pages: Tree[Page] = Tree.create()) extends Ingredient {
          val name = Name("pages")

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
              directive_leaf(content.copy(dox = c))
            }
          }

          def canonize(ctx: Context) = {
            val a = pages.transform(PagesCanonizeTransformer(ctx))
            copy(pages = a)
          }
        }
        case class Images(realm: Realm = Realm.create()) extends Ingredient {
          val name = Name("images")

          def canonize(ctx: Context) = this
        }
        case class Container(name: Name, realm: Realm = Realm.create()) extends Ingredient {
          def canonize(ctx: Context) = this
        }
      }

      case class Navigation(
        references: Tree[Navigation.Reference] = Tree.create()
      )
      object Navigation {
        case class Reference(pathname: PathName, title: String)

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
            }
          }
          _nodes.foldLeft(Z())(_+_).r
        }

        private def _navigation(ps: NonEmptyVector[Ingredient]): Navigation =
          ps.vector.collect {
            case m: Ingredient.Pages => _navigation(m)
          }.headOption.getOrElse(Navigation.empty)

        private def _navigation(p: Ingredient.Pages): Navigation = {
          case class Slot(pathname: String, title: String)

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
                val title = Dox.getTitleString(c.dox) getOrElse {
                  StringUtils.makeTitleFromPathname(c.name.name)
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
        val pb = _build_playbook()
        Antora(
          pb,
          _components.toList
        )
      }

      private def _build_playbook(): Playbook = _playbook getOrElse {
        val title = config.title
        val startpage = _components.headOption.map { x =>
          val name = x.name.name
          val file = x.homePage
          s"${x.name.name}::${file.name}"
        } getOrElse "index.adoc"
        val url = config.url
        val site = Playbook.Site(
          Title(title),
          Reference(startpage),
          url
        )
        val content = Playbook.Content(_sources)
//        val redirects = Playbook.Redirects(false)
        val ui = Playbook.Ui.default
        val output = Playbook.Output.default
        Playbook(site, content, ui, output)
      }

      private def _sources: List[Playbook.Content.Source] =
        _components.toList.map { x =>
          val startpath = x.name.name
          Playbook.Content.Source(
            new URI("./docs"),
            Paths.get(startpath)
          )
        }

      def setComponent(name: String, title: String) = {
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
        title: String = "SimpleModeling",
        url: Option[URL] = Some(new URI("https://www.simplemodeling.org").toURL)
      )
      object Config {
        val default = Config()
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
    private val _antora = new Antora.Builder(Antora.Builder.Config.default)

    def build(): Antora = _antora.build()

    override def enter(node: TreeNode[Node]) = node.getContent match {
      case Some(s) => _depth match {
        case 0 => _at_home(node, s)
        case 1 => _at_component(node, s)
        case 2 => _at_module(node, s)
        case _ => _at_ingredient(node, s)
      }
      case None =>
        _depth match {
          case 0 => _at_home(node)
          case 1 => _at_component(node)
          case 2 => _at_module(node)
          case _ => _at_ingredient(node)
        }
        _depth = _depth + 1
    }

    private def _at_home(node: TreeNode[Node]): Unit = _antora.setComponent(node.name, _category_title(node.name))

    private def _at_home(node: TreeNode[Node], c: Node): Unit = {}

    private def _at_component(node: TreeNode[Node]): Unit = _antora.setModule(node.name)

    private def _at_component(node: TreeNode[Node], c: Node): Unit = _antora.addNode(c)

    private def _at_module(node: TreeNode[Node]): Unit = RAISE.notImplementedYetDefect

    private def _at_module(node: TreeNode[Node], c: Node): Unit = _antora.addNode(c)

    private def _at_ingredient(node: TreeNode[Node]): Unit = RAISE.notImplementedYetDefect
    private def _at_ingredient(node: TreeNode[Node], c: Node): Unit = RAISE.notImplementedYetDefect

    override def leave(node: TreeNode[Node]) = node.getContent match {
      case Some(s) => {}
      case None =>
        _depth = _depth - 1
        _depth match {
          case 0 => _return_to_home(node)
          case 1 => _return_to_component(node)
          case 2 => _return_to_module(node)
          case _ => _return_to_ingredient(node)
        }
    }

    private def _return_to_home(node: TreeNode[Node]): Unit = _antora.pushComponent()
    private def _return_to_component(node: TreeNode[Node]): Unit = _antora.pushModule()
    private def _return_to_module(node: TreeNode[Node]): Unit = {}
    private def _return_to_ingredient(node: TreeNode[Node]): Unit = {}

    private def _category_title(name: String): String =
      config.categoryTitle(name)
  }
  object Builder {
    case class Config(metadata: MetaData) {
      def categoryTitle(name: String): String = metadata.categories.makeTitle(name)
    }
  }
}
