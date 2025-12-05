package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf.{Graph, Triple, Node}

/*
 * RdfRenderer: Prefix-aware JSON-LD / Turtle renderer
 * ------------------------------------------------------------
 * Features:
 *  - JSON-LD Profiles (SmartDox, SimpleModel, BoK)
 *  - Node ordering (profile-specific @graph / Turtle subject ordering)
 *  - Property ordering (rdf:type → rdfs:label → rdfs:comment → rdfs:subClassOf → others)
 *  - CURIE-first rendering based on @context prefixes
 *  - Turtle formatting with prefix header and subject grouping
 *  - JSON-LD pretty / compact via Policy
 *  - Pretty @context with stable key ordering
 *
 * @since   Nov. 20, 2025
 * @version Nov. 20, 2025
 * @author  ASAMI, Tomoharu
 */
object RdfRenderer {

  // ============================================================
  // Context Ordering (for pretty @context)
  // ============================================================

  private val ContextKeyOrder: Seq[String] = Seq(
    "@vocab", "@base", "@language",
    "rdf", "rdfs", "owl", "xsd",
    "smorg", "bok", "sd", "sm"
  )

  private def sortContextKeys(keys: Iterable[String]): Seq[String] = {
    val keySet = keys.toSet
    val known = ContextKeyOrder.filter(keySet)
    val unknown = (keySet -- ContextKeyOrder.toSet).toSeq.sorted
    known ++ unknown
  }

  // ============================================================
  // Rendering Policy
  // ============================================================

  case class Policy(
    prettyJson: Boolean = true,     // pretty-print whole JSON-LD
    prettyContext: Boolean = true,   // pretty-print @context
    jsonIndent: Int = 2              // indent spaces
  )
  object Policy {
    val default = Policy()
  }

  // ============================================================
  // JSON-LD Profiles
  // ============================================================

  sealed trait JsonLDProfile {
    def name: String
    def defaultContext: Map[String, Any]
    def includeOntologyNode: Boolean

    /** Node ordering within @graph / Turtle (profile-specific) */
    def nodeSortKey(id: String): Int

    /** Property ordering within a node (default: type → label → comment → subClassOf → others) */
    def propertySortKey(iri: String): Int =
      JsonLDProfile.defaultPropertySortKey(iri)
  }

  object JsonLDProfile {

    private val commonPrefixes: Map[String, Any] = Map(
      "rdf"  -> Vocabulary.Rdf.namespace,
      "rdfs" -> Vocabulary.Rdfs.namespace,
      "owl"  -> Vocabulary.Owl.namespace
    )

    // local name extractor for IRIs and CURIEs
    private[semanticweb] def localName(id: String): String = {
      val curieIdx = id.indexOf(':')
      if (curieIdx >= 0 && !id.startsWith("http")) {
        // Already a CURIE like smorg:Ontology
        id.substring(curieIdx + 1)
      } else {
        val hashIdx = id.lastIndexOf('#')
        val slashIdx = id.lastIndexOf('/')
        val idx = math.max(hashIdx, slashIdx)
        if (idx >= 0 && idx < id.length - 1) id.substring(idx + 1) else id
      }
    }

    // Common property ordering for all profiles
    private[semanticweb] def defaultPropertySortKey(iri: String): Int = {
      localName(iri) match {
        case "type"       => 0
        case "label"      => 1
        case "comment"    => 2
        case "subClassOf" => 3
        case _            => 100
      }
    }

    /** SmartDox profile: document-centric ordering */
    case object SmartDox extends JsonLDProfile {
      override val name = "smartdox"
      override val defaultContext =
        commonPrefixes ++ Map("sd" -> SmartDoxOntology.namespace)
      override val includeOntologyNode = false

      override def nodeSortKey(id: String): Int = {
        val ln = localName(id)
        ln match {
          case "" | "Ontology" => 0  // ontology root / ontology class
          case "Document"      => 10
          case "Meta"          => 15
          case "Section"       => 20
          case "Paragraph"     => 30
          case "List"          => 40
          case "Item"          => 41
          case "Table"         => 50
          case "Figure"        => 60
          case "CodeBlock"     => 70
          case "Link"          => 80
          case _               => 1000
        }
      }
    }

    /** SimpleModel profile: domain model centric ordering */
    case object SimpleModel extends JsonLDProfile {
      override val name = "simplemodel"
      override val defaultContext =
        commonPrefixes ++ Map("sm" -> SimpleModelOntology.namespace)
      override val includeOntologyNode = true

      override def nodeSortKey(id: String): Int = {
        if (id.endsWith("#")) 0 // ontology root
        else {
          localName(id) match {
            case "Ontology"     => 5
            case "DomainObject" => 10
            case "Entity"       => 20
            case "Value"        => 30
            case "Rule"         => 40
            case "Service"      => 50
            case "Event"        => 60
            case "Component"    => 70
            case "Subsystem"    => 80
            case _              => 1000
          }
        }
      }
    }

    /** BoK / Meta-BoK profile: ontology, schema, site, kb ordering */
    case object BoK extends JsonLDProfile {
      override val name = "bok"
      override val defaultContext =
        commonPrefixes ++ Map("bok" -> BokOntology.namespace)
      override val includeOntologyNode = true

      override def nodeSortKey(id: String): Int = {
        if (id.endsWith("#")) 0 // ontology root
        else {
          localName(id) match {
            case "Ontology"      => 10
            case "Schema"        => 20
            case "Site"          => 30
            case "Vocabulary"    => 40
            case "KnowledgeBase" => 50
            case "Module"        => 60
            case "Component"     => 70
            case "System"        => 80
            case _               => 1000
          }
        }
      }
    }
  }

  import JsonLDProfile._

  // ============================================================
  // Prefix Helpers
  // ============================================================

  // Extract namespace part from an IRI (up to last '#' or '/')
  private def namespaceOf(iri: String): String = {
    val hashIdx  = iri.lastIndexOf('#')
    val slashIdx = iri.lastIndexOf('/')
    val idx = math.max(hashIdx, slashIdx)
    if (idx >= 0) iri.substring(0, idx + 1) else iri
  }

  // Well-known namespaces used in SimpleModeling ecosystem
  private val WellKnownPrefixes: Seq[(String, String)] = Seq(
    SimpleModelingOrgOntology.namespace -> "smorg",
    BokOntology.namespace               -> "bok",
    SmartDoxOntology.namespace          -> "sd",
    SimpleModelOntology.namespace       -> "sm"
  )

  // Collect namespaces appearing in Graph
  private def namespacesInGraph(graph: Graph): Set[String] =
    graph.triples.flatMap { t =>
      Seq(t.subject, t.predicate, t.obj)
    }.collect {
      case Node.Uri(iri) => namespaceOf(iri)
    }.toSet

  // Add missing well-known prefixes into context
  private def enrichContextWithGraph(
    context: Map[String, Any],
    graph: Graph
  ): Map[String, Any] = {
    val existingNs: Set[String] =
      context.collect {
        case (k, v: String) if !k.startsWith("@") && isNamespace(v) => v
      }.toSet

    val graphNs = namespacesInGraph(graph)

    val toAdd: Seq[(String, String)] =
      WellKnownPrefixes.collect {
        case (ns, prefix)
          if graphNs.contains(ns) &&
             !existingNs.contains(ns) &&
             !context.contains(prefix) =>
          prefix -> ns
      }

    context ++ toAdd
  }

  // Build prefix table using context and graph
  private def buildPrefixTable(context: Map[String, Any], graph: Graph): Seq[(String, String)] = {
    val enriched = enrichContextWithGraph(context, graph)
    buildPrefixTable(enriched)
  }

  private def isNamespace(value: String): Boolean =
    value.contains("://") && (value.endsWith("#") || value.endsWith("/"))

  /** Build (namespace -> prefix) table from @context */
  private def buildPrefixTable(context: Map[String, Any]): Seq[(String, String)] =
    context.collect {
      case (prefix, ns: String)
        if !prefix.startsWith("@") && isNamespace(ns) =>
        (ns, prefix)
    }.toSeq.sortBy(_._1.length).reverse

  /** CURIE converter */
  private def curie(iri: String, table: Seq[(String, String)]): String =
    table.collectFirst {
      case (ns, p) if iri.startsWith(ns) =>
        p + ":" + iri.substring(ns.length)
    }.getOrElse(iri)

  // ============================================================
  // Turtle Rendering (Profile-aware, grouped by subject)
  // ============================================================

  /** Profile-aware Turtle rendering (grouped by subject) */
  def toTurtle(
    graph: Graph,
    profile: JsonLDProfile
  ): String = {
    val context = profile.defaultContext
    val prefixTable = buildPrefixTable(context, graph)

    // group triples by subject
    val grouped: Map[Node, Seq[Triple]] = graph.triples.groupBy(_.subject)

    // subject ordering: use profile.nodeSortKey for URI subjects
    val sortedSubjects: Seq[Node] =
      grouped.keys.toSeq.sortBy {
        case Node.Uri(uri)  => (profile.nodeSortKey(uri), uri)
        case Node.Blank(id) => (Int.MaxValue - 1, "_:b" + id)
        case other          => (Int.MaxValue, other.toString)
      }

    val body =
      sortedSubjects
        .map { subj =>
          renderSubjectBlockTurtle(subj, grouped(subj), prefixTable, Some(profile))
        }.mkString("\n")

    val prefixHeader =
      prefixTable.map { case (ns, prefix) =>
        s"@prefix $prefix: <$ns> ."
      }.mkString("\n")

    if (prefixHeader.nonEmpty)
      prefixHeader + "\n\n" + body
    else
      body
  }

  /** Context-aware Turtle rendering (no profile, grouped by subject) */
  def toTurtle(
    graph: Graph,
    context: Map[String, Any]
  ): String = {
    val prefixTable = buildPrefixTable(context, graph)

    val grouped: Map[Node, Seq[Triple]] = graph.triples.groupBy(_.subject)

    // default node ordering: lexicographic by node id
    val sortedSubjects: Seq[Node] =
      grouped.keys.toSeq.sortBy(nodeToId)

    val body =
      sortedSubjects
        .map { subj =>
          renderSubjectBlockTurtle(subj, grouped(subj), prefixTable, None)
        }.mkString("\n")

    val prefixHeader =
      prefixTable.map { case (ns, prefix) =>
        s"@prefix $prefix: <$ns> ."
      }.mkString("\n")

    if (prefixHeader.nonEmpty)
      prefixHeader + "\n\n" + body
    else
      body
  }

  /** Render one subject block with grouped predicates and objects */
  private def renderSubjectBlockTurtle(
    subject: Node,
    triples: Seq[Triple],
    table: Seq[(String, String)],
    profileOpt: Option[JsonLDProfile]
  ): String = {
    val subjStr = renderSubjectTurtle(subject, table)

    // group by predicate
    val byPred: Map[Node, Seq[Triple]] = triples.groupBy(_.predicate)

    // property ordering: profile-aware if available, otherwise default ordering
    val sortedPredicates: Seq[Node] =
      byPred.keys.toSeq.sortBy {
        case Node.Uri(iri) =>
          val key = profileOpt match {
            case Some(p) => p.propertySortKey(iri)
            case None    => JsonLDProfile.defaultPropertySortKey(iri)
          }
          (key, iri)
        case other =>
          (Int.MaxValue, other.toString)
      }

    val lines: Seq[String] =
      sortedPredicates.zipWithIndex.map { case (predNode, idx) =>
        val predStr = renderPredicateTurtle(predNode, table)

        val objs: Seq[String] =
          byPred(predNode).map(t => renderObjectTurtle(t.obj, table))

        val objStr = objs.mkString(", ")

        val sep = if (idx == sortedPredicates.size - 1) " ." else " ;"

        if (idx == 0)
          s"$subjStr $predStr $objStr$sep"
        else
          s"  $predStr $objStr$sep"
      }

    lines.mkString("\n")
  }

  private def renderTripleTurtle(t: Triple, table: Seq[(String, String)]): String = {
    val subj = renderSubjectTurtle(t.subject, table)
    val pred = renderPredicateTurtle(t.predicate, table)
    val obj  = renderObjectTurtle(t.obj, table)
    s"$subj $pred $obj ."
  }

  private def renderSubjectTurtle(n: Node, table: Seq[(String, String)]): String = n match {
    case Node.Uri(uri) =>
      val c = curie(uri, table)
      val prefixes = table.map(_._2)
      val isCurie = prefixes.exists(p => c.startsWith(p + ":"))
      if (isCurie) c else "<" + uri + ">"
    case Node.Blank(id) => "_:b" + id
    case _ =>
      throw new IllegalArgumentException("Turtle subject must be URI or Blank")
  }

  private def renderPredicateTurtle(n: Node, table: Seq[(String, String)]): String = n match {
    case Node.Uri(uri) => curie(uri, table)
    case _ =>
      throw new IllegalArgumentException("Turtle predicate must be URI")
  }

  private def renderObjectTurtle(n: Node, table: Seq[(String, String)]): String = n match {
    case Node.Uri(uri) =>
      val c = curie(uri, table)
      val prefixes = table.map(_._2)
      val isCurie = prefixes.exists(p => c.startsWith(p + ":"))
      if (isCurie) c else "<" + uri + ">"
    case Node.Blank(id) => "_:b" + id
    case Node.Literal(v, None, None) =>
      "\"" + escape(v) + "\""
    case Node.Literal(v, Some(dt), None) =>
      "\"" + escape(v) + "\"^^" + curie(dt, table)
    case Node.Literal(v, None, Some(lang)) =>
      "\"" + escape(v) + "\"" + "@" + lang
    case Node.Literal(v, Some(_), Some(lang)) =>
      "\"" + escape(v) + "\"" + "@" + lang
  }

  // ============================================================
  // JSON-LD Rendering
  // ============================================================

  def toJsonLD(
    graph: Graph,
    profile: JsonLDProfile,
    userContext: Map[String, Any] = Map.empty,
    policy: Policy = Policy()
  ): String = {

    // 1. Merge profile default context and user context
    val mergedContext = profile.defaultContext ++ userContext

    // 2. Build prefix table excluding @vocab/@base
    val nonVocabCtx = mergedContext.filterNot { case (k, _) => k == "@vocab" || k == "@base" }
    val prefixTable = buildPrefixTable(nonVocabCtx, graph)

    // 3. Group triples by subject
    val grouped =
      graph.triples
        .groupBy(t => nodeToId(t.subject))
        .filter(_._1.nonEmpty)

    // 4. Filter ontology root if profile says so
    val filtered =
      if (profile.includeOntologyNode) grouped
      else grouped.filterNot { case (id, _) => id.endsWith("#") }

    // 5. Sort nodes by profile-specific nodeSortKey
    val nodes =
      filtered.toSeq.sortBy { case (id, _) =>
        (profile.nodeSortKey(id), id)
      }

    // 6. Render each node
    val jsonNodes =
      nodes.map { case (id, triples) =>
        renderNodeJson(id, triples, prefixTable, profile, policy)
      }.mkString(
        if (policy.prettyJson) ",\n" else ","
      )

    // 7. Assemble final JSON-LD string
    if (policy.prettyJson) {
      implicit val indentSpaces: Int = policy.jsonIndent
      val ind1 = indent(1)
      val ind2 = indent(2)
      s"""{
$ind1"@context": ${jsonMap(mergedContext, policy)},
$ind1"@graph": [
$jsonNodes
$ind1]
}"""
    } else {
      s"""{
  "@context": ${jsonMap(mergedContext, policy)},
  "@graph": [$jsonNodes]
}"""
    }
  }

  private def nodeToId(n: Node): String = n match {
    case Node.Uri(uri)  => uri
    case Node.Blank(id) => "_:b" + id
    case _              => ""
  }

  private def renderNodeJson(
    id: String,
    triples: Seq[Triple],
    table: Seq[(String, String)],
    profile: JsonLDProfile,
    policy: Policy
  ): String = {

    val rawId = if (id.startsWith("_:")) id else curie(id, table)
    val idRepr = if (rawId.endsWith(":")) rawId.dropRight(1) else rawId

    // ---- group by predicate ----
    val grouped: Map[String, Seq[Node]] =
      triples.groupBy(_.predicate).map { case (predNode, ts) =>
        val pred = predNode match {
          case Node.Uri(uri) =>
            val c = curie(uri, table)
            if (c.endsWith(":")) c.dropRight(1) else c
          case _ =>
            throw new IllegalArgumentException("Predicate must be URI")
        }
        pred -> ts.map(_.obj)
      }

    // ---- sorted predicates ----
    val sortedPredKeys: Seq[String] =
      grouped.keys.toSeq.sortBy { pred =>
        val iriCandidate =
          table.collectFirst {
            case (ns, prefix) if pred.startsWith(prefix + ":") =>
              ns + pred.drop(prefix.length + 1)
          }.getOrElse(pred)
        profile.propertySortKey(iriCandidate)
      }

    // pretty printed
    if (policy.prettyJson) {
      implicit val indentSpaces: Int = policy.jsonIndent
      val ind2 = indent(2)
      val ind3 = indent(3)

      val props = sortedPredKeys.map { pred =>
        val objs = grouped(pred).map(obj => renderObjectJson(obj, table))
        val value =
          if (objs.size == 1) objs.head
          else objs.mkString("[", ", ", "]")

        s"""$ind3"$pred": $value"""
      }.mkString(",\n")

      s"""$ind2{
$ind3"@id": "$idRepr",
$props
$ind2}"""
    } else {
      // compact
      val props = sortedPredKeys.map { pred =>
        val objs = grouped(pred).map(obj => renderObjectJson(obj, table))
        val value =
          if (objs.size == 1) objs.head
          else objs.mkString("[", ", ", "]")

        s"""      "$pred": $value"""
      }.mkString(",\n")

      s"""    {
      "@id": "$idRepr",
$props
    }"""
    }
  }

  private def renderObjectJson(n: Node, table: Seq[(String, String)]): String = n match {
    case Node.Uri(uri) =>
      val c = curie(uri, table)
      s"""{"@id": "$c"}"""
    case Node.Blank(id) =>
      s"""{"@id": "_:b$id"}"""
    case Node.Literal(v, None, None) =>
      "\"" + escape(v) + "\""
    case Node.Literal(v, Some(dt), None) =>
      val cdt = curie(dt, table)
      s"""{"@value": "${escape(v)}", "@type": "$cdt"}"""
    case Node.Literal(v, None, Some(lang)) =>
      s"""{"@value": "${escape(v)}", "@language": "$lang"}"""
    case Node.Literal(v, Some(_), Some(lang)) =>
      s"""{"@value": "${escape(v)}", "@language": "$lang"}"""
  }

  // ============================================================
  // JSON helpers
  // ============================================================

  private def escape(s: String): String =
    s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' =>
        "\\u%04x".format(c.toInt)
      case c =>
        c.toString
    }

  private def jsonMapCompact(m: Map[String, Any]): String = {
    val entries = m.map {
      case (k, v: String) =>
        s""""$k": "$v""""
      case (k, v) =>
        s""""$k": "$v""""
    }.mkString(", ")
    "{ " + entries + " }"
  }

  private def jsonMapPretty(m: Map[String, Any])(implicit indentSpaces: Int): String = {
    val ind1 = indent(1)
    val ind2 = indent(2)
    val orderedKeys = sortContextKeys(m.keys)
    val entries = orderedKeys.map { k =>
      m(k) match {
        case v: String =>
          s"""$ind2"$k": "$v""""
        case v =>
          s"""$ind2"$k": "$v""""
      }
    }.mkString(",\n")
    s"{\n$entries\n$ind1}"
  }

  private def jsonMap(m: Map[String, Any], policy: Policy): String = {
    if (policy.prettyContext) {
      implicit val spaces: Int = policy.jsonIndent
      jsonMapPretty(m)
    } else {
      jsonMapCompact(m)
    }
  }

  private def indent(n: Int)(implicit spaces: Int): String =
    " " * (n * spaces)
}
