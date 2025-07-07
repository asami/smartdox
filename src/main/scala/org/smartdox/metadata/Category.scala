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
 * @since   Jun. 23, 2025
 *  version Jun. 28, 2025
 * @version Jul.  5, 2025
 * @author  ASAMI, Tomoharu
 */
case class Category(
  name: Category.CategoryName,
  title: Option[Category.CategoryTitle],
  uri: URI,
  description: I18NString = I18NString.empty,
  titleImage: Option[URI] = None,
  kind: Category.Kind = Category.Kind.Topics
) {
  private lazy val _key = StringUtils.makePathContainerRelativeBody(uri.toString)

  def isMatch(name: String) = _key equalsIgnoreCase name

  def effectiveTitle: String = title.map(_.title.en) getOrElse name.name

  def containerString = _key

  def containerUri: URI = new URI(containerString)

  def yamlString(ctx: I18NContext): String = {
    val json = this.asJson(Category.categoryEncoderWithLocale(ctx.locale))
    CirceUtils.toYamlString(json)
  }
}

object Category {
  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  case class CategoryName(name: String) extends datatype.Name
  object CategoryName {
    implicit val nameDecoder: Decoder[CategoryName] = Decoder.decodeString.emap(x => Right(CategoryName(x)))

    implicit val nameEncoder: Encoder[CategoryName] = Encoder.encodeString.contramap(_.name)
  }

  case class CategoryTitle(title: I18NString) extends datatype.I18NTitle
  object CategoryTitle {
    implicit val titleDecoder: Decoder[CategoryTitle] =
      Decoder[I18NString].map(CategoryTitle.apply)

    implicit val titleEncoder: Encoder[CategoryTitle] =
      Encoder[I18NString].contramap(_.title)
  }

  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Topics, Disciplines)

    case object Topics extends Kind {
      val name = "topics"
    }
    case object Disciplines extends Kind {
      val name = "disciplines"
    }

    implicit val kindDecoder: Decoder[Kind] = Decoder.decodeString.emap(_create)

    implicit val kindEncoder: Encoder[Kind] = Encoder.encodeString.contramap(_.name)

    private def _create(p: String): Either[String, Kind] =
      get(p).toRight(s"Unknown kind: $p")
  }

  def categoryDecoder(uri: URI): Decoder[Category] = Decoder.instance { cursor =>
    for {
      name <- cursor.downField("name").as[CategoryName]
      title <- cursor.downField("title").as[Option[CategoryTitle]]
      titleimage <- cursor.downField("title_image").as[Option[URI]]
      description <- cursor.downField("description").as[Option[I18NString]]
      kindopt <- cursor.downField("kind").as[Option[Kind]]
    } yield {
      val desc = description getOrElse I18NString.empty
      kindopt match {
        case Some(kind) => Category(name, title, uri, desc, titleimage, kind)
        case None => Category(name, title, uri, desc, titleimage)
      }
    }
  }

//  implicit val categoryEncoder: Encoder[Category] = deriveConfiguredEncoder

  def categoryEncoderWithLocale(locale: Locale): Encoder[Category] = Encoder.instance { c =>
    Json.obj(
      "name" -> CategoryName.nameEncoder(c.name),
      "title" -> Json.fromString(c.title.map(_.distill(locale)).getOrElse(c.name.name)),
      "uri" -> uriEncoder(c.uri),
      "description" -> I18NString.i18nStringWithLocaleEncoder(locale)(c.description),
      "title_image" -> c.titleImage.asJson(Encoder.encodeOption(uriEncoder)),
      "kind" -> Kind.kindEncoder(c.kind)
    )
  }

  def error(name: String, e: Throwable): Category =
    Category(CategoryName(name), None, new URI(name), I18NString(s"$name: $e"))
}

case class CategoryCollection(
  categories: VectorMap[String, Category] = VectorMap.empty
) {
  def makeTitle(name: String): String = categories.values.find(_.isMatch(name)).map(_.effectiveTitle) getOrElse StringUtils.capitalize(name)

  def categoryVector = categories.toValues
}

object CategoryCollection {
  val empty = CategoryCollection()
}
