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
import org.goldenport.collection.VectorMap
import org.goldenport.util.StringUtils
import org.goldenport.util.CirceUtils
import org.goldenport.util.CirceUtils.Codec._

/*
 * @since   Jul. 22, 2025
 *  version Jul. 22, 2025
 * @version Sep.  3, 2025
 * @author  ASAMI, Tomoharu
 */
case class Keyword(
  name: Keyword.KeywordName,
  title: Option[Keyword.KeywordTitle],
  description: I18NString = I18NString.empty,
  titleImage: Option[URI] = None
) {
}

object Keyword {
  case class KeywordName(name: String) extends datatype.Name
  object KeywordName {
    implicit val nameDecoder: Decoder[KeywordName] = Decoder.decodeString.emap(x => Right(KeywordName(x)))

    implicit val nameEncoder: Encoder[KeywordName] = Encoder.encodeString.contramap(_.name)
  }

  case class KeywordTitle(title: I18NString) extends datatype.I18NTitle
  object KeywordTitle {
    implicit val titleDecoder: Decoder[KeywordTitle] =
      Decoder[I18NString].map(KeywordTitle.apply)

    implicit val titleEncoder: Encoder[KeywordTitle] =
      Encoder[I18NString].contramap(_.title)
  }
}

case class KeywordCollection(
  keywords: VectorMap[String, Keyword] = VectorMap.empty
) {
  def toHistory: History = History.empty // TODO
}

object KeywordCollection {
  val empty = KeywordCollection()
}
