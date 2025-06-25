package org.smartdox.metadata

import java.net.URI
import org.goldenport.value._
import org.goldenport.i18n.I18NString
import org.goldenport.datatype
import org.goldenport.collection.VectorMap
import org.goldenport.util.CirceUtils.Codec._

/*
 * @since   Jun. 23, 2025
 * @version Jun. 25, 2025
 * @author  ASAMI, Tomoharu
 */
case class Category(
  name: Category.CategoryName,
  title: Option[Category.CategoryTitle],
  uri: URI,
  kind: Category.Kind = Category.Kind.Topics
)

object Category {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  implicit val circeconf = Configuration.default.
    withDefaults.withSnakeCaseMemberNames

  case class CategoryName(name: String) extends datatype.Name
  object CategoryName {
    implicit val nameDecoder: Decoder[CategoryName] = Decoder.decodeString.emap(x => Right(CategoryName(x)))

    implicit val nameEncoder: Encoder[CategoryName] = Encoder.encodeString.contramap(_.name)
  }

  case class CategoryTitle(title: I18NString) extends datatype.I18NTitle
  object CategoryTitle {
    implicit val titleDecoder: Decoder[CategoryTitle] = deriveConfiguredDecoder

    implicit val titleEncoder: Encoder[CategoryTitle] = deriveConfiguredEncoder
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
      kindopt <- cursor.downField("kind").as[Option[Kind]]
    } yield kindopt.fold(
      Category(name, title, uri)
    )(
      kind => Category(name, title, uri, kind)
    )
  }

  implicit val categoryEncoder: Encoder[Category] = deriveConfiguredEncoder
}

case class CategoryCollection(
  categories: VectorMap[String, Category] = VectorMap.empty
)

object CategoryCollection {
  val empty = CategoryCollection()
}
