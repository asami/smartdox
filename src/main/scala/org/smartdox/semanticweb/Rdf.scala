package org.smartdox.semanticweb

import java.net.URI
import org.goldenport.context.Consequence

/*
 * @since   Nov. 12, 2025
 * @version Nov. 29, 2025
 * @author  ASAMI, Tomoharu
 */
case class Rdf(
)

object Rdf {
  sealed trait Node {
    def value: String
  }
  object Node {
    case class Uri(value: String) extends Node
    case class Literal(
      value: String,
      datatype: Option[String] = None, // TODO
      lang: Option[String] = None // TODO
    ) extends Node
    case class Blank(value: String) extends Node
  }

  case class Triple(
    subject: Node,
    predicate: Node.Uri,
    obj: Node
  )

  case class Graph(
    baseUri: Option[URI] = None,
    triples: Vector[Triple] = Vector.empty
  ) {
    def +(triple: Triple): Graph = copy(triples = triples :+ triple)

    def toJsonLd: String = {
      import io.circe.syntax._
      import io.circe.{Encoder, Json}

      implicit val nodeEncoder: Encoder[Node] = Encoder.instance {
        case Node.Uri(v)         => Json.fromString(v)
        case Node.Literal(v, dt, lang) =>
          val base = Json.obj("@value" -> Json.fromString(v))
          val withDt = dt.map(d => base.deepMerge(Json.obj("@type" -> Json.fromString(d)))).getOrElse(base)
          lang.map(l => withDt.deepMerge(Json.obj("@language" -> Json.fromString(l)))).getOrElse(withDt)
        case Node.Blank(v)   => Json.obj("@id" -> Json.fromString(s"_:$v"))
      }

      val json = triples.map { t =>
        Json.obj(
          "@id" -> t.subject.asJson,
          t.predicate.value -> Json.arr(t.obj.asJson)
        )
      }

      Json.obj("@graph" -> Json.fromValues(json)).spaces2
    }

    def toTurtle: String = {
      triples.map { t =>
        s"${renderNode(t.subject)} ${t.predicate.value} ${renderNode(t.obj)} ."
      }.mkString("\n")
    }

    private def renderNode(n: Node): String = n match {
      case Node.Uri(v)         => s"<$v>"
      case Node.Literal(v, None, None) => s""""${v.replace("\"", "\\\"")}""""
      case Node.Literal(v, Some(dt), _) => s""""${v.replace("\"", "\\\"")}"^^<$dt>"""
      case Node.Literal(v, None, Some(lang)) => s""""${v.replace("\"", "\\\"")}"@${lang}"""
      case Node.Blank(v)   => s"_:$v"
    }
  }
  object Graph {
    def apply(baseuri: URI): Graph = Graph(Some(baseuri))
    def apply(triples: Seq[Triple]): Graph = new Graph(triples = triples.toVector)

    def create(baseuri: String): Consequence[Graph] = Consequence(
      Graph(Some(new URI(baseuri)))
    )
  }

  case class RdfDataset(
    graphs: Map[String, Graph] = Map.empty
  ) {
    def defaultGraph: Option[Graph] = graphs.get("@default")

    def addGraph(name: String, graph: Graph): RdfDataset =
      copy(graphs = graphs + (name -> graph))

    def toJsonLd: String = {
      import io.circe.syntax._
      import io.circe.Json

      val json = graphs.map { case (name, g) =>
        Json.obj("name" -> Json.fromString(name), "graph" -> io.circe.parser.parse(g.toJsonLd).getOrElse(Json.Null))
      }
      Json.obj("@dataset" -> Json.fromValues(json)).spaces2
    }
  }

  case class PrefixMap(prefixes: Map[String, String]) {
    def expand(name: String): String =
      if (name.contains(":")) {
        val Array(pre, local) = name.split(":", 2)
        prefixes.get(pre).map(_ + local).getOrElse(name)
      } else name

    def shortForm(uri: String): String =
      prefixes.collectFirst {
        case (pre, ns) if uri.startsWith(ns) => s"$pre:${uri.stripPrefix(ns)}"
      }.getOrElse(uri)

    def toJson: io.circe.Json = {
      import io.circe.syntax._
      prefixes.asJson
    }
  }

  object PrefixMap {
    val defaultForSimpleModeling: PrefixMap = PrefixMap(Map(
      "sm"     -> "https://www.simplemodeling.org/ontology#",
      "cml"    -> "https://www.simplemodeling.org/cml#",
      "sdx"    -> "https://www.smartdox.org/ontology#",
      "bok"    -> "https://www.simplemodeling.org/bok#",
      "schema" -> "https://schema.org/",
      "rdfs"   -> "http://www.w3.org/2000/01/rdf-schema#",
      "rdf"    -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xsd"    -> "http://www.w3.org/2001/XMLSchema#",
      "dcterms"-> "http://purl.org/dc/terms/",
      "foaf"   -> "http://xmlns.com/foaf/0.1/",
      "skos"   -> "http://www.w3.org/2004/02/skos/core#",
      "owl"    -> "http://www.w3.org/2002/07/owl#",
      "prov"   -> "http://www.w3.org/ns/prov#"
    ))
  }

  case class GraphWithPrefix(
    graph: Graph,
    prefixes: PrefixMap = PrefixMap(Map(
      "sm" -> "https://www.simplemodeling.org/ontology#",
      "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
      "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xsd" -> "http://www.w3.org/2001/XMLSchema#"
    ))
  ) {
    def toJsonLdVocab: String = {
      import io.circe.syntax._
      import io.circe.{Encoder, Json}

      implicit val nodeEncoder: Encoder[Node] = Encoder.instance {
        case Node.Uri(v)         => Json.fromString(prefixes.shortForm(v))
        case Node.Literal(v, dt, lang) =>
          val base = Json.obj("@value" -> Json.fromString(v))
          val withDt = dt.map(d => base.deepMerge(Json.obj("@type" -> Json.fromString(prefixes.shortForm(d))))).getOrElse(base)
          lang.map(l => withDt.deepMerge(Json.obj("@language" -> Json.fromString(l)))).getOrElse(withDt)
        case Node.Blank(v)   => Json.obj("@id" -> Json.fromString(s"_:$v"))
      }

      val grouped = graph.triples.groupBy(_.subject).map { case (subject, triples) =>
        val base = Json.obj("@id" -> Json.fromString(prefixes.shortForm(subject.asInstanceOf[Node.Uri].value)))
        val props = triples.flatMap {
          case Triple(_, Node.Uri("rdf:type"), obj) =>
            Some("@type" -> Json.arr(Json.fromString(prefixes.shortForm(obj.asInstanceOf[Node.Uri].value))))
          case Triple(_, pred, obj) =>
            Some(prefixes.shortForm(pred.value) -> Json.arr(obj.asJson))
        }
        base.deepMerge(Json.obj(props: _*))
      }

      val json = Json.obj(
        "@context" -> prefixes.toJson,
        "@graph" -> Json.fromValues(grouped)
      )
      json.spaces2
    }
  }
}
