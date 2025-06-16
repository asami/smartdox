package org.smartdox.doxsite

import scalaz.{Tree => ZTree, _}, Scalaz._
import scala.util.matching.Regex
import java.io.File
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
import org.goldenport.realm.RealmTransformer
import org.goldenport.value._
import org.goldenport.collection.NonEmptyVector
import org.goldenport.i18n.LocaleUtils
import org.goldenport.util.StringUtils
import org.goldenport.util.OptionUtils.lastMonoid
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.smartdox.metadata.MetaData
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.metadata.Glossary
import org.smartdox.metadata.Notices
import org.smartdox.generator.Context
import org.smartdox.service.operations.SiteParameters
import org.smartdox.transformers.Dox2HtmlTransformer
import org.smartdox.transformers.AutoWireTransformer
import org.smartdox.transformers.LanguageFilterTransformer

/*
 * @since   Feb. 23, 2025
 *  version Feb. 25, 2025
 *  version Mar.  9, 2025
 *  version Apr. 29, 2025
 *  version May. 31, 2025
 * @version Jun. 16, 2025
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
    val en = _build_locale(context, RealmBuilder.Rule.en.withConfig(config.outputTreeTransformerConfig))
    val ja = _build_locale(context, RealmBuilder.Rule.ja.withConfig(config.outputTreeTransformerConfig))
    val realm = Realm.create()
    val a = realm.merge("en", en)
    a.merge("ja", ja)
  }

  private def _build_locale(
    context: Context,
    rule: RealmBuilder.Rule
  ) = {
    val a = space.transform(new RealmBuilder(context, rule))
    Realm(a)
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

  private def _build_notices(p: Realm): Realm = {
    val xs = metadata.notices.take(5)
    for ((x, i) <- xs.zipWithIndex) {
      val c = x.yamlString
      val path = f"MET-INF/data/notice${i + 1}%02d.yaml"
      p.setContent(path, c)
    }
    p
  }
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
    strategy: Strategy = Strategy.Overview
  ) {
    def isAutoWire(p: Page): Boolean = strategy.isAutoWire(p)
    def isAutoI18n(p: Page): Boolean = strategy.isAutoI18n(p)
    def isNotice: Boolean = strategy.isNotice
    def isGlossary: Boolean = strategy.isGlossary
    def isLinkEnable: Boolean = strategy.isLinkEnable
    def isLinkEnable(p: Page): Boolean = strategy.isLinkEnable(p)

    def +(rhs: Config): Config = Config(
      lastMonoid(inputTreeTransformerConfig, rhs.inputTreeTransformerConfig),
      lastMonoid(transformTreeTransformerConfig, rhs.transformTreeTransformerConfig),
      lastMonoid(outputTreeTransformerConfig, rhs.outputTreeTransformerConfig),
      rhs.strategy
    )
  }
  object Config {
    val default = Config()

    implicit val configDecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configEncoder: Encoder[Config] = deriveConfiguredEncoder

    // def create(p: Option[Strategy]): Config = default.copy(strategy = p)
    def create(p: SiteParameters.Holder): Config = {
      val inconfig = p.target match {
        case Some(s) if s.nonEmpty =>
          Some(_tree_transformer_config(s))
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
  }

  sealed trait Strategy extends NamedValueInstance {
    def isAutoWire(p: Page): Boolean = documentStrategy(p).isAutoWire
    def isAutoI18n(p: Page): Boolean = documentStrategy(p).isAutoI18n
    def isNotice: Boolean = true
    def isGlossary: Boolean = true
    def isLinkEnable: Boolean = true
    def isLinkEnable(p: Page): Boolean = documentStrategy(p).isLinkEnable
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
    import DocumentMetaData._

    val elements = Vector(Production, Full, WowkInProgress, Draft, Preparetion, Overview)

    case object Production extends Strategy {
      val name = "production"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Full
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
    case object WowkInProgress extends Strategy {
      val name ="work-in-progress"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Skip
          case Status.WorkInProgress => DocumentStrategy.Full
          case Status.Draft => DocumentStrategy.Draft
          case Status.InPreparetion => DocumentStrategy.Skip
          case Status.Inactive => DocumentStrategy.Skip
        }
    }
    case object Draft extends Strategy {
      val name ="draft"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.Published => DocumentStrategy.Skip
          case Status.WorkInProgress => DocumentStrategy.Draft
          case Status.Draft => DocumentStrategy.Draft
          case Status.InPreparetion => DocumentStrategy.Skip
          case Status.Inactive => DocumentStrategy.Skip
        }
    }
    case object Preparetion extends Strategy {
      val name ="preparation"
      def documentStrategy(p: DocumentMetaData): DocumentStrategy =
        p.status match {
          case Status.InPreparetion => DocumentStrategy.Draft
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
          case Status.InPreparetion => DocumentStrategy.Draft
          case Status.Inactive => DocumentStrategy.Skip
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
    def isAutoI18n: Boolean = true
    def isNotice: Boolean = true
    def isGlossary: Boolean = true
    def isLinkEnable: Boolean = true
//    def targetStatus: List[DocumentMetaData.Status]
  }
  object DocumentStrategy extends EnumerationClass[DocumentStrategy] {
    import DocumentMetaData.Status

    val elements = Vector(Production, Full, WorkInProgress, Draft, Skip)

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
      //   Status.InPreparetion
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
    }

    implicit val strategyDecoder: Decoder[DocumentStrategy] = Decoder.decodeString.emap(_create)

    implicit val strategyEncoder: Encoder[DocumentStrategy] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, DocumentStrategy] =
      get(p).toRight(s"Unknown document strategy: $p")
  }

  class DoxSiteBuilder(
    override val rule: DoxSiteBuilder.Rule,
    context: TreeTransformer.Context[Node]
  )(implicit val ctx: DateTimeContext) extends TreeTransformer[Realm.Data, Node] {
    override def isCleanEmptyChildren = true
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
      _create_dox(name, dox)
    }

    private def _org_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      _create_dox(name, dox)
    }

    private def _markdown_page(name: String, c: String) = {
      val dox = Dox2Parser.parse(c)
      _create_dox(name, dox)
    }

    private def _create_dox(name: String, dox: Dox) =
      if (rule.strategy.isActive(dox))
        _create_page(name, dox)
      else
        Nil

    private def _create_page(name: String, dox: Dox) =
      List(Page(name, dox))
  }
  object DoxSiteBuilder {
    case class Rule(
      doxSiteConfig: Config = Config.default
    ) extends TreeTransformer.Rule[Realm.Data, Node] {
      def strategy = doxSiteConfig.strategy

      override def config = doxSiteConfig.inputTreeTransformerConfig
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

  def create(
    context: Context,
    file: File
  )(implicit ctx: DateTimeContext): DoxSite = create(context, file, None, DoxSite.Config.default)

  def create(
    context: Context,
    file: File,
    configname: Option[String],
    inconfig: DoxSite.Config
  )(implicit ctx: DateTimeContext): DoxSite = {
    val a = Realm.create(file)
    create(context, a, configname, inconfig)
  }

  def create(
    context: Context,
    realm: Realm,
    configname: String,
    inconfig: DoxSite.Config
  )(implicit ctx: DateTimeContext): DoxSite =
    create(context, realm, Some(configname), inconfig)

  def create(
    context: Context,
    realm: Realm,
    configname: Option[String],
    inconfig: DoxSite.Config
  )(implicit dctx: DateTimeContext): DoxSite = {
    val config = _config(inconfig, realm, configname)(context.i18NContext)
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
        Config(None, None, output)
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
