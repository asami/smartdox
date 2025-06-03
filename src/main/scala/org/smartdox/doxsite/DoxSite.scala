package org.smartdox.doxsite

import scalaz.{Tree => ZTree, _}, Scalaz._
import java.io.File
import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.context.Consequence
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
import org.goldenport.realm.RealmTransformer
import org.goldenport.value._
import org.goldenport.i18n.LocaleUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.OptionUtils.lastMonoid
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.Glossary
import org.smartdox.metadata.Notices
import org.smartdox.generator.Context
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Feb. 23, 2025
 *  version Feb. 25, 2025
 *  version Mar.  9, 2025
 *  version Apr. 29, 2025
 *  version May. 31, 2025
 * @version Jun.  4, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSite(
  config: DoxSite.Config,
  space: Tree[Node],
  metadata: MetaData
) {
  import DoxSite._

  def toRealm(context: Context): Realm = {
    val targets = List(LocaleUtils.en, LocaleUtils.ja) // TODO
    targets match {
      case Nil => _build_plain(context)
      case xs => _build_multi(context)
    }
  }

  private def _build_plain(
    context: Context
  ) = {
    val rule = RealmBuilder.Rule(config.outputTreeTransformerConfig)
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
  }

  private def _build_multi(
    context: Context
  ) = {
    val en = _build_en(context)
    val ja = _build_ja(context)
    val realm = Realm.create()
    val a = realm.merge("en", en)
    a.merge("ja", ja)
  }

  private def _build_en(
    context: Context
  ) = {
    val rule = RealmBuilder.Rule.en.withConfig(config.outputTreeTransformerConfig)
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
  }

  private def _build_ja(
    context: Context
  ) = {
    val rule = RealmBuilder.Rule.ja.withConfig(config.outputTreeTransformerConfig)
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
  }

  def traverse(p: TreeVisitor[Node]): Unit = space.traverse(p)
}

object DoxSite {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  case class Config(
    inputTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    transformTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    outputTreeTransformerConfig: Option[TreeTransformer.Config] = None,
    strategy: Option[Strategy] = None
  ) {
    def isAutoWire: Boolean = strategy.fold(true)(_.isAutoWire)
    def isAutoI18n: Boolean = strategy.fold(true)(_.isAutoI18n)
    def isNotice: Boolean = strategy.fold(true)(_.isNotice)
    def isGlossary: Boolean = strategy.fold(true)(_.isGlossary)
    def isLinkEnable: Boolean = strategy.fold(true)(_.isLinkEnable)

    def +(rhs: Config): Config = Config(
      lastMonoid(inputTreeTransformerConfig, rhs.inputTreeTransformerConfig),
      lastMonoid(transformTreeTransformerConfig, rhs.transformTreeTransformerConfig),
      lastMonoid(outputTreeTransformerConfig, rhs.outputTreeTransformerConfig),
      lastMonoid(strategy, rhs.strategy)
    )
  }
  object Config {
    val default = Config()

    implicit val configDecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configEncoder: Encoder[Config] = deriveConfiguredEncoder

    def create(p: Option[Strategy]): Config = default.copy(strategy = p)
  }

  sealed trait Strategy extends NamedValueInstance {
    def isAutoWire: Boolean = true
    def isAutoI18n: Boolean = true
    def isNotice: Boolean = true
    def isGlossary: Boolean = true
    def isLinkEnable: Boolean = true
  }
  object Strategy extends EnumerationClass[Strategy] {
    val elements = Vector(Production, Full, Draft)

    case object Production extends Strategy {
      def name = "production"
    }
    case object Full extends Strategy {
      def name = "full"
    }
    case object Draft extends Strategy {
      def name ="draft"
      override def isAutoWire: Boolean = false
      override def isNotice: Boolean = false
      override def isGlossary: Boolean = false
      override def isLinkEnable: Boolean = false
    }

    implicit val strategyDecoder: Decoder[Strategy] = Decoder.decodeString.emap(create)

    implicit val strategyEncoder: Encoder[Strategy] = Encoder.encodeString.contramap(_.name)

    def create(p: String): Either[String, Strategy] =
      get(p).toRight(s"Unknown strategy: $p")
  }

  class DoxSiteBuilder(
    override val rule: DoxSiteBuilder.Rule,
    context: TreeTransformer.Context[Node]
  ) extends TreeTransformer[Realm.Data, Node] {
    def treeTransformerContext = context

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
        case m: Realm.FileData => TreeTransformer.Directive.Default[Node]
        case m: Realm.BagData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ObjectData => TreeTransformer.Directive.Default[Node]
        case m: Realm.ApplicationData => TreeTransformer.Directive.Default[Node]
      }
    }

    private def _build_file(node: TreeNode[Realm.Data], p: Realm.StringData): TreeTransformer.Directive[Node] =
      _build_file(node, p.string)

    private def _build_file(node: TreeNode[Realm.Data], p: String): TreeTransformer.Directive[Node] =
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
                    case "dox" => _dox_page(name, m.string)
                    case "org" => _org_page(name, m.string)
                    case "md" => _markdown_page(name, m.string)
                    case "markdown" => _markdown_page(name, m.string)
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

    private def _dox_page(name: String, c: String) = {
      // println(s"_dox_page: $c")
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }

    private def _org_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }

    private def _markdown_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      List(Page(name, dox))
    }
  }
  object DoxSiteBuilder {
    case class Rule(
      doxSiteConfig: Config = Config.default
    ) extends TreeTransformer.Rule[Realm.Data, Node] {
      override def config = doxSiteConfig.outputTreeTransformerConfig
      override def getTargetName(p: TreeNode[Realm.Data]): Option[String] = {
        p.getNameSuffix.collect {
          case "dox" => s"${p.nameBody}.dox"
          case "org" => s"${p.nameBody}.dox"
          case "md" => s"${p.nameBody}.dox"
          case "markdown" => s"${p.nameBody}.dox"
        }
      }
    }
    object Rule {
      def apply(p: TreeTransformer.Config): Rule = Rule(Config(inputTreeTransformerConfig = Some(p)))
    }
  }

  class RealmBuilder(
    gcontext: Context,
    rule: RealmBuilder.Rule,
    context: Option[TreeTransformer.Context[Realm.Data]] = None
  ) extends TreeTransformer[Node, Realm.Data] {
    def treeTransformerContext = context getOrElse gcontext.realmContext

    override protected def make_Node(
      node: TreeNode[Node],
      content: Node
    ): TreeTransformer.Directive[Realm.Data] = {
      content match {
        case m: Page => _to_html(m) // TreeTransformer.Directive.Content(m.toRealmData)
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
      rule.targetLocale.fold(p)(x => Dox.transform(p, new LanguageFilterTransformer(gcontext.doxContext, x)))
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

  def create(
    context: Context,
    file: File,
    configname: Option[String],
    inconfig: DoxSite.Config
  ): DoxSite = {
    val a = Realm.create(file)
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
    configname: Option[String],
    inconfig: DoxSite.Config
  ): DoxSite = {
    val config = _config(inconfig, realm, configname)(context.i18nContext)
    val nodectx = TreeTransformer.Context.default[Node]
    val doxctx = context.doxContext
    val ctx = DoxSiteTransformer.Context(DoxSiteTransformer.Config(Some(config)), nodectx, doxctx)
    val rule = DoxSiteBuilder.Rule(config)
    val a1: Tree[Node] = realm.transformTree(new DoxSiteBuilder(rule, nodectx))
    val notices = _collect_notice(config, context, a1)
    val a = a1.transform(new DoxSitePreTransformer(config, ctx))
    val (b, glossary) = _build_glossary(config, ctx, a)
    val metadata = MetaData(glossary = glossary, notices = notices)
    val ctx1 = ctx.withMetaData(metadata)
    val c: Tree[Node] = _enable_link(config, ctx1, b)
    val d = c.transform(new DoxSitePostTransformer(config, ctx))
    new DoxSite(config, c, metadata)
  }

  private def _collect_notice(
    config: DoxSite.Config,
    gcontext: Context,
    p: Tree[Node]
  ): Notices =
    if (config.isNotice) {
      val noticecollector = new NoticeCollector(gcontext)
      p.traverse(noticecollector)
      noticecollector.notices
    } else {
      Notices.empty
    }

  private def _build_glossary(
    config: DoxSite.Config,
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): (Tree[Node], Glossary) =
    if (config.isGlossary) {
      val gb = new GlossaryBuilder(ctx)
      val b: Tree[Node] = p.transform(gb)
      (b, gb.glossary)
    } else {
      (p, Glossary.empty)
    }

  private def _enable_link(
    config: DoxSite.Config,
    ctx: DoxSiteTransformer.Context,
    p: Tree[Node]
  ): Tree[Node] =
    if (config.isLinkEnable)
      p.transform(new LinkEnabler(ctx))
    else
      p

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
    configname.fold(DoxSite.Config.default) { n =>
      val c = for {
        json <- ConfigLoader.loadConfigJson(realm, n)
        output <- _tree_transformer_config(json.hcursor.downField("output").focus)
      } yield {
        val strategy = None
        Config(None, None, output, strategy)
      }
      c.take
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
