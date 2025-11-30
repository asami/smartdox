package org.smartdox.doxsite

import scala.util.control.NonFatal
import scala.util.matching.Regex
import org.goldenport.RAISE
import org.goldenport.util.StringUtils
import org.goldenport.tree._
import org.smartdox._
import org.smartdox.generator.{Context => GContext}
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.smartdox.parser.Dox2Parser

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 *  version Apr.  5, 2025
 *  version May. 31, 2025
 *  version Jun. 28, 2025
 *  version Jul. 23, 2025
 *  version Aug. 22, 2025
 *  version Oct. 26, 2025
 * @version Nov. 30, 2025
 * @version Nov. 30, 2025
 * @author  ASAMI, Tomoharu
 */
trait DoxSiteTransformer extends HomoTreeTransformer[Node] {
  def context: DoxSiteTransformer.Context

  def treeTransformerContext = context.nodeContext

  protected def dox_Transformers(
    ctx: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): List[HomoTreeTransformer[Dox]] = List(dox_Transformer(ctx, node, p))

  protected def dox_Transformer(
    ctx: DoxSiteTransformer.Context,
    node: TreeNode[Node],
    p: Page
  ): HomoTreeTransformer[Dox] = RAISE.notImplementedYetDefect("DoxSiteTransformer#dox_Transformer")

  override protected def make_Node(
    node: TreeNode[Node],
    content: Node
  ): TreeTransformer.Directive[Node] = content match {
    case m: Page => TreeTransformer.Directive.Node(make_page(node, m))
    case m: ImageNode => TreeTransformer.Directive.Node(make_image(node, m))
    case m => TreeTransformer.Directive.Default[Node]
  }

  protected def make_page(
    node: TreeNode[Node],
    p: Page
  ): TreeNode[Node] = make_Page(node, p)

  protected def make_Page(
    node: TreeNode[Node],
    page: Page
  ): TreeNode[Node] = {
    dox_Transformers(context, node, page) match {
      case Nil => node
      case xs =>
        val c = _make_dox(page, xs)
        val name = context.normalizeUriName(node.name)
        TreeNode.create(name, page.withDox(c))
    }
  }

  private def _make_dox(
    page: Page,
    xs: List[HomoTreeTransformer[Dox]]
  ): Dox = try {
    val a = Dox.toTree(page.dox)
    val b = xs.foldLeft(a)((z, x) => z.transform(x))
    Dox.toDox(b)
  } catch {
    case NonFatal(e) => _make_error_dox(page, e)
  }

  private def _make_error_dox(page: Page, e: Throwable) =
    Dox2Parser.errorDocument(page.name.name, e)

  protected def make_image(
    node: TreeNode[Node],
    p: ImageNode
  ): TreeNode[Node] = {
    val name = context.normalizeUriName(node.name)
    if (name == node.name)
      node
    else
      TreeNode.create(name, p.withName(name))
  }
}

object DoxSiteTransformer {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  case class Config(
    doxsiteConfig: Option[DoxSite.Config] = None
  ) {
    def treeConfig: Option[TreeTransformer.Config] = doxsiteConfig.flatMap(_.transformTreeTransformerConfig)

    def isGlossary = doxsiteConfig.fold(false)(_.isGlossary)
    def isGlossaryInDocument = doxsiteConfig.fold(false)(_.isGlossaryInDocument)
    def isLinkEnable = doxsiteConfig.fold(false)(_.isLinkEnable)
    def isAutoWire(p: Page) = p.isAutoWire getOrElse doxsiteConfig.fold(false)(_.isAutoWire(p))
    def isAutoI18n(p: Page) = doxsiteConfig.fold(false)(_.isAutoI18n(p))
  }
  object Config {
    val default = Config()

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    implicit val configdecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configencoder: Encoder[Config] = deriveConfiguredEncoder
  }

  case class Context(
    config: Config,
    generatorContext: GContext,
    nodeContext: TreeTransformer.Context[Node],
    doxContext: TreeTransformer.Context[Dox],
    metadata: MetaData = MetaData.empty
  ) {
    lazy val cache: DoxSiteCache = new DoxSiteCache(config.doxsiteConfig, generatorContext)

    def withMetaData(metadata: MetaData): Context = copy(metadata = metadata)

    def normalizeUriName(p: String): String = StringUtils.camelToUnderscore(p)

    def doxSiteConfig: DoxSite.Config = config.doxsiteConfig getOrElse DoxSite.Config.default

    def textMark = doxSiteConfig.textMark
  }
}
