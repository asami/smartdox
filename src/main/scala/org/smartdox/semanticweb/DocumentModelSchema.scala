package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf._
import org.smartdox.semanticweb.Vocabulary._
import org.smartdox.semanticweb.Vocabulary.Rdf.node.{`type` => RdfType}

/*
 * DocumentModelSchema
 *
 * Builds the RDF graph for the Document Model layer:
 *   - Concept        (Glossary Term)
 *   - KnowledgeUnit  (Articles, Bibliography entries)
 *   - Relation       (Document relationships)
 *   - Category       (Document classification)
 *
 * This schema extracts SmartDox-based resources and produces
 * a semantic graph aligned with DocumentModelOntology.
 *
 * @since   Nov. 28, 2025
 * @version Nov. 28, 2025
 * @author  ASAMI, Tomoharu
 */
object DocumentModelSchema extends SchemaModel {

  override val prefix: String = Vocabulary.DocumentModel.prefix
  override val namespace: String = Vocabulary.DocumentModel.namespace

  override lazy val jsonldContext = DocumentModelOntology.jsonldContext

  // ------------------------------------------------------------
  // Document-level Instances
  // ------------------------------------------------------------

  case class ConceptInstance(id: String, label: String)
  case class KnowledgeUnitInstance(id: String, title: String)
  case class RelationInstance(subject: String, predicate: String, obj: String)
  case class CategoryInstance(id: String, label: String)

  // ------------------------------------------------------------
  // RDF Graph Construction
  // ------------------------------------------------------------

  case class DocumentModel(
      concepts: Seq[ConceptInstance],
      kus: Seq[KnowledgeUnitInstance],
      relations: Seq[RelationInstance],
      categories: Seq[CategoryInstance]
  ) {

    def toGraph: Graph = {
      val conceptTriples = concepts.flatMap { c =>
        val s = Node.Uri(c.id)
        Seq(
          Triple(s, RdfType, Node.Uri(DocumentModelOntology.Concept)),
          Triple(s, Rdfs.node.label, Node.Literal(c.label))
        )
      }

      val kuTriples = kus.flatMap { ku =>
        val s = Node.Uri(ku.id)
        Seq(
          Triple(s, RdfType, Node.Uri(DocumentModelOntology.KnowledgeUnit)),
          Triple(s, Rdfs.node.label, Node.Literal(ku.title))
        )
      }

      val relationTriples = relations.map { r =>
        Triple(
          Node.Uri(r.subject),
          Node.Uri(r.predicate),
          Node.Uri(r.obj)
        )
      }

      val mentionTriples = relations.collect {
        case RelationInstance(subj, pred, obj) if pred == DocumentModelOntology.mentions =>
          Triple(Node.Uri(subj), Node.Uri(DocumentModelOntology.mentions), Node.Uri(obj))
      }

      val mentionedInTriples = relations.collect {
        case RelationInstance(subj, pred, obj) if pred == DocumentModelOntology.mentionedIn =>
          Triple(Node.Uri(subj), Node.Uri(DocumentModelOntology.mentionedIn), Node.Uri(obj))
      }

      // Placeholder: glossaryFor relations (DocumentModel ⇒ SimpleModel)
      val glossaryForTriples: Seq[Triple] = Seq.empty

      val categoryTriples = categories.flatMap { cat =>
        val s = Node.Uri(cat.id)
        Seq(
          Triple(s, RdfType, Node.Uri(DocumentModelOntology.Category)),
          Triple(s, Rdfs.node.label, Node.Literal(cat.label))
        )
      }

      Graph(
        conceptTriples ++
        kuTriples ++
        relationTriples ++
        mentionTriples ++
        mentionedInTriples ++
        categoryTriples ++
        glossaryForTriples
      )
    }
  }

  // ------------------------------------------------------------
  // Extraction from Site Resources
  // ------------------------------------------------------------

  def fromSiteResources(res: Seq[Site.SiteResource]): DocumentModel = {

    val concepts = res.collect {
      case Site.SiteResource.Glossary(id, meta) =>
        ConceptInstance(id, meta.getTitleString(java.util.Locale.JAPANESE).getOrElse(id))
    }

    val kus = res.collect {
      case Site.SiteResource.Article(id, meta) =>
        KnowledgeUnitInstance(id, meta.getTitleString(java.util.Locale.JAPANESE).getOrElse(id))
      case Site.SiteResource.Bibliography(id, meta) =>
        KnowledgeUnitInstance(id, meta.getTitleString(java.util.Locale.JAPANESE).getOrElse(id))
    }

    val categories: Seq[CategoryInstance] = Seq.empty

    val relations: Seq[RelationInstance] = Seq.empty

    DocumentModel(concepts, kus, relations, categories)
  }
}
