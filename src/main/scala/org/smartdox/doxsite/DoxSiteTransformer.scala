package org.smartdox.doxsite

import scala.util.control.NonFatal
import scala.util.matching.Regex
import org.goldenport.RAISE
import org.smartdox._
import org.smartdox.transformer._
import org.smartdox.metadata._
import org.goldenport.tree._

/*
 * @since   Mar.  7, 2025
 *  version Mar.  9, 2025
 *  version Apr.  5, 2025
 *  version May. 31, 2025
 * @version Jun. 28, 2025
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
        val a = Dox.toTree(page.dox)
        val b = xs.foldLeft(a)((z, x) => z.transform(x))
        val c = Dox.toDox(b)
        TreeNode.create(node.name, Page(node.name, c))
    }
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
    def isLinkEnable = doxsiteConfig.fold(false)(_.isLinkEnable)
    def isAutoWire(p: Page) = doxsiteConfig.fold(false)(_.isAutoWire(p))
    def isAutoI18n(p: Page) = doxsiteConfig.fold(false)(_.isAutoI18n(p))
  }
  object Config {
    val default = Config()

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    implicit val configdecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configencoder: Encoder[Config] = deriveConfiguredEncoder
  }

  // case class Config(
  //   scope: Config.Scope = Config.Scope.All,
  //   includes: List[Regex] = Nil,
  //   excludes: List[Regex] = Nil
  // )
  // object Config {
  //   val empty = Config()

  //   implicit val circeconf = Configuration.default.
  //     withDefaults.withSnakeCaseMemberNames

  //   implicit val regexDecoder: Decoder[Regex] = Decoder.decodeString.emap { str =>
  //     try {
  //       Right(str.r)
  //     } catch {
  //       case NonFatal(e) => Left(s"Invalid regex: ${e.getMessage}")
  //     }
  //   }
  //   implicit val regexEncoder: Encoder[Regex] = Encoder.encodeString.contramap(_.regex)

  //   implicit val configdecoder: Decoder[Config] = deriveConfiguredDecoder
  //   implicit val configencoder: Encoder[Config] = deriveConfiguredEncoder

  //   sealed trait Scope {
  //     def name: String
  //   }
  //   object Scope {
  //     val elements = Vector(All, HomeOnly, ExcludeHome)

  //     case object All extends Scope {
  //       def name = "all"
  //     }
  //     case object HomeOnly extends Scope {
  //       def name = "home_only"
  //     }
  //     case object ExcludeHome extends Scope {
  //       def name = "exclude_home"
  //     }

  //     def create(p: String): Either[String, Scope] =
  //       elements.find(_.name == p).map(Right(_)) getOrElse {
  //         Left(s"Unknown Scope: $p")
  //       }

  //     implicit val scopeDecoder: Decoder[Scope] = Decoder.decodeString.emap(create)
  //     implicit val scopeEncoder: Encoder[Scope] = Encoder.encodeString.contramap(_.name)
  //   }
  // }

  case class Context(
    config: Config,
    nodeContext: TreeTransformer.Context[Node],
    doxContext: TreeTransformer.Context[Dox],
    metadata: MetaData = MetaData.empty
  ) {
    def withMetaData(metadata: MetaData): Context = copy(metadata = metadata)
  }
}
