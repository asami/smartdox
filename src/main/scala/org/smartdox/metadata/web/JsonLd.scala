package org.smartdox.metadata.web

import java.util.Locale
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.smartdox.metadata.DocumentMetaData

/*
 * @since   Oct. 25, 2025
 *  version Oct. 25, 2025
 * @version Nov. 22, 2025
 * @author  ASAMI, Tomoharu
 */
/** === JSON-LD base trait === */
trait JsonLdTyped {
  def jsonLdType: String
  def id: Option[String]
}

/** === JSON-LD helper object === */
object JsonLd {
  def createArticle(locale: Locale, p: DocumentMetaData): Article = {
    val name = p.getTitleString(locale)
    val description = p.getEffectiveSummaryString(locale)
    val url = None // XXX
    val sameas = Nil
    val alternatename = None
    val core = ThingCore(
      name,
      description,
      url,
      sameas,
      alternatename
    )
    val headline = p.takeEffectiveHeadlineString(locale)
    val author: Either[Person, Organization] = p.getAuthorString(locale) match {
      case Some(s) => Left(Person.create(s))
      case None => p.getOrganizationString(locale) match {
        case Some(ss) => Right(Organization.create(ss))
        case None => Left(Person.create(""))
      }
    }
    val published = p.getPublishedString(locale)
    val modified = p.getModifiedString(locale)
    val mainentityofpage = None // TODO
    val image = Nil // TODO
    val id = None
    Article(
      core,
      headline,
      author,
      published,
      modified,
      mainentityofpage,
      image,
      id
    )
  }

  /** Builds a top-level JSON-LD document with @context. */
  def document[A](context: Json)(a: A)(implicit A: Encoder.AsObject[A]): Json = {
    val node = A.encodeObject(a)
    Json.obj("@context" -> context).deepMerge(Json.fromJsonObject(node))
  }

  /** Adds @type and @id to a JsonObject. */
  def withTypeAndId(obj: JsonObject, tpe: String, id: Option[String]): JsonObject = {
    val withType = obj.add("@type", Json.fromString(tpe))
    id.fold(withType)(v => withType.add("@id", Json.fromString(v)))
  }

  /** Common context constants. */
  val schemaOrg: Json = Json.fromString("https://schema.org")
}

/** === JsonObject extension (wrapped in object to avoid naming conflicts) === */
object JsonObjectSyntax {
  implicit final class JsonObjectOps(private val self: JsonObject) extends AnyVal {
    def addIfDefined(key: String, v: Option[Json]): JsonObject =
      v.fold(self)(j => self.add(key, j))
  }
}

/** === Common ThingCore === */
final case class ThingCore(
  name: Option[String] = None,
  description: Option[String] = None,
  url: Option[String] = None,
  sameAs: List[String] = Nil,
  alternateName: Option[String] = None
)
object ThingCore {
  def create(name: String): ThingCore = ThingCore(Some(name))

  implicit val enc: Encoder.AsObject[ThingCore] = deriveEncoder[ThingCore]
}

/** === ImageObject === */
final case class ImageObject(
  contentUrl: String,
  width: Option[Int] = None,
  height: Option[Int] = None,
  caption: Option[String] = None,
  id: Option[String] = None
) extends JsonLdTyped {
  val jsonLdType = "ImageObject"
}
object ImageObject {
  import JsonObjectSyntax._  // bring extension methods into scope

  implicit val enc: Encoder.AsObject[ImageObject] = Encoder.AsObject.instance { x =>
    val base = JsonObject(
      "contentUrl" -> Json.fromString(x.contentUrl)
    )
      .addIfDefined("width", x.width.map(Json.fromInt))
      .addIfDefined("height", x.height.map(Json.fromInt))
      .addIfDefined("caption", x.caption.map(Json.fromString))

    JsonLd.withTypeAndId(base, x.jsonLdType, x.id)
  }
}

/** === Person === */
final case class Person(
  core: ThingCore = ThingCore(),
  givenName: Option[String] = None,
  familyName: Option[String] = None,
  jobTitle: Option[String] = None,
  id: Option[String] = None
) extends JsonLdTyped {
  val jsonLdType = "Person"
}
object Person {
  import JsonObjectSyntax._

  def create(name: String): Person = Person(ThingCore.create(name))

  implicit val enc: Encoder.AsObject[Person] = Encoder.AsObject.instance { x =>
    val base = ThingCore.enc.encodeObject(x.core)
      .addIfDefined("givenName", x.givenName.map(Json.fromString))
      .addIfDefined("familyName", x.familyName.map(Json.fromString))
      .addIfDefined("jobTitle", x.jobTitle.map(Json.fromString))

    JsonLd.withTypeAndId(base, x.jsonLdType, x.id)
  }
}

/** === Organization === */
final case class Organization(
  core: ThingCore = ThingCore(),
  logo: Option[ImageObject] = None,
  id: Option[String] = None
) extends JsonLdTyped {
  val jsonLdType = "Organization"
}
object Organization {
  import JsonObjectSyntax._

  def create(name: String): Organization = Organization(ThingCore.create(name))

  implicit val enc: Encoder.AsObject[Organization] = Encoder.AsObject.instance { x =>
    val base = ThingCore.enc.encodeObject(x.core)
      .addIfDefined("logo", x.logo.map(_.asJson))

    JsonLd.withTypeAndId(base, x.jsonLdType, x.id)
  }
}

/** === Article === */
final case class Article(
  core: ThingCore = ThingCore(),
  headline: String,
  author: Either[Person, Organization],
  datePublished: Option[String] = None,
  dateModified: Option[String] = None,
  mainEntityOfPage: Option[String] = None,
  image: List[ImageObject] = Nil,
  id: Option[String] = None
) extends JsonLdTyped {
  val jsonLdType = "Article"
}
object Article {
  import JsonObjectSyntax._

  implicit val enc: Encoder.AsObject[Article] = Encoder.AsObject.instance { x =>
    val base0 = ThingCore.enc.encodeObject(x.core)
      .add("headline", Json.fromString(x.headline))
      .add("author", x.author.fold(_.asJson, _.asJson))
      .addIfDefined("datePublished", x.datePublished.map(Json.fromString))
      .addIfDefined("dateModified", x.dateModified.map(Json.fromString))
      .addIfDefined("mainEntityOfPage", x.mainEntityOfPage.map(Json.fromString))

    val base = if (x.image.nonEmpty) base0.add("image", x.image.asJson) else base0
    JsonLd.withTypeAndId(base, x.jsonLdType, x.id)
  }
}

