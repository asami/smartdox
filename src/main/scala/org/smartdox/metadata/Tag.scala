package org.smartdox.metadata

import java.net.URI
import java.util.Locale
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import org.goldenport.value._
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NContext
import org.goldenport.datatype
import org.goldenport.tree._
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils
import org.goldenport.util.CirceUtils.Codec._

/*
 * @since   Jul. 22, 2025
 *  version Aug. 17, 2025
 * @version Sep.  3, 2025
 * @author  ASAMI, Tomoharu
 */
case class Tag(
  name: Tag.TagName,
  title: Option[Tag.TagTitle],
  description: I18NString = I18NString.empty,
  titleImage: Option[URI] = None
) {
}

object Tag {
  case class TagName(name: String) extends datatype.QualifiedName
  object TagName {
    implicit val nameDecoder: Decoder[TagName] = Decoder.decodeString.emap(x => Right(TagName(x)))

    implicit val nameEncoder: Encoder[TagName] = Encoder.encodeString.contramap(_.name)

    def apply(p: datatype.QualifiedName): TagName = TagName(p.name)
    def apply(p: Seq[String]): TagName = TagName(p.mkString("."))
  }

  case class TagTitle(title: I18NString) extends datatype.I18NTitle
  object TagTitle {
    implicit val titleDecoder: Decoder[TagTitle] =
      Decoder[I18NString].map(TagTitle.apply)

    implicit val titleEncoder: Encoder[TagTitle] =
      Encoder[I18NString].contramap(_.title)
  }
}

case class TagCollection(
  tags: Tree[Tag] = Tree.create()
) {
  def toHistory: History = History.empty // TODO
}

object TagCollection {
  def create(): TagCollection = TagCollection()
}
