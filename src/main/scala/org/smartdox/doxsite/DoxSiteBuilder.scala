package org.smartdox.doxsite

import java.io.File
import java.net.URI
import java.time.Instant
import java.util.Locale
import scala.util.control.NonFatal
import org.goldenport.config.ConfigLoader
import org.goldenport.context.Consequence
import org.goldenport.i18n.LocaleUtils
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.realm.Realm.FileData
import org.goldenport.tree.TreeNode
import org.goldenport.tree.TreeTransformer
import org.goldenport.util.RegexUtils
import org.goldenport.util.StringUtils
import org.goldenport.value._
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.metadata.Category
import org.smartdox.metadata.PublishMetadata
import org.smartdox.parser.Dox2Parser
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Aug. 30, 2026
 *  version Aug. 30, 2026
 * @version Sep.  9, 2026
 * @author  ASAMI, Tomoharu
 */
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
                    case "md" => _markdown_page(node, m.string)
                    case "markdown" => _markdown_page(node, m.string)
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
        val (pathname, parserconfig) = _source_parser(node, Dox2Parser.Config.default)
        Dox2Parser.parseWithFilename(parserconfig, pathname, c)
      }
      _create_dox(node.name, dox, lastmodified)
    }

    private def _source_parser(
      node: TreeNode[Realm.Data],
      baseconfig: Dox2Parser.Config
    ): (String, Dox2Parser.Config) = {
      rule.doxSiteConfig.origin match {
        case Some(origin) =>
          val pathname = new File(origin, node.pathname).getCanonicalFile
          (pathname.toString, baseconfig.withResourceRoot(pathname.getParentFile.toPath))
        case None =>
          (node.pathname, baseconfig._with_virtual_resource_parent(_virtual_parent(node.pathname)))
      }
    }

    private def _virtual_parent(pathname: String): String = {
      val index = pathname.lastIndexOf('/')
      if (index < 0) "" else pathname.substring(0, index)
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

    private def _markdown_page(node: TreeNode[Realm.Data], c: String) = {
      val (pathname, parserconfig) = _source_parser(node, Dox2Parser.Config.markdown)
      val dox = Dox2Parser.parseWithFilename(parserconfig, pathname, c)
      _create_dox(node.name, dox)
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
      val index = DoxSite.publicPath(StringUtils.changeLeafRelative(pathname, "index.html"))
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
      doxSiteConfig: DoxSite.Config = DoxSite.Config.default
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
            case "md" => p.name
            case "markdown" => p.name
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
      def apply(p: TreeTransformer.Config): Rule = Rule(DoxSite.Config(inputTreeTransformerConfig = Some(p)))
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
    context: Option[TreeTransformer.Context[Realm.Data]] = None,
    articleMediaProjection: Option[PublishMetadata.ArticleMediaProjection] = None
  ) extends TreeTransformer[Node, Realm.Data] {
    def this(
      legacyGeneratorContext: Context,
      legacyRule: RealmBuilder.Rule
    ) = this(legacyGeneratorContext, legacyRule, None, None)

    def this(
      legacyGeneratorContext: Context,
      legacyRule: RealmBuilder.Rule,
      legacyTransformerContext: Option[TreeTransformer.Context[Realm.Data]]
    ) = this(legacyGeneratorContext, legacyRule, legacyTransformerContext, None)

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
        case m: Page => _to_html(node, m) // TreeTransformer.Directive.Content(m.toRealmData)
        case m: MetaDataNode => directive_empty
        case m: ImageNode => directive_leaf(FileData(m.file))
      }
    }

    private def _to_html(node: TreeNode[Node], p: Page): TreeTransformer.Directive.LeafNode[Realm.Data] = {
      val filtered = _filter(p.dox)
      val dox = rule.targetLocale.fold(filtered) { locale =>
        DoxSiteArticleMedia.projectArticleHeader(Dox.toDocument(filtered), node.pathnameRelative, locale, articleMediaProjection)
      }
      val htmlrule = Dox2HtmlTransformer.Rule.noCss
      val s = Consequence.from(Dox2HtmlTransformer(gcontext, htmlrule).transform(dox)).
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
