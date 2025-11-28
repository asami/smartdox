package org.smartdox.semanticweb

import org.smartdox.semanticweb.Rdf.Node.Uri

/**
 * Vocabulary definitions for RDF, RDFS, OWL, and Dublin Core Terms namespaces.
 *
 * This object provides URIs and node representations for common RDF/OWL terms,
 * facilitating semantic web development and interoperability.
 *
 * @since   Nov. 11, 2025
 * @version Nov. 22, 2025
 * @author  ASAMI, Tomoharu
 */
object Vocabulary {

  /** RDF Vocabulary: RDF syntax namespace and common RDF terms */
  object Rdf {
    val prefix    = "rdf"
    val namespace = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    def uri(local: String): String = namespace + local

    val `type`      = uri("type")
    val Property    = uri("Property")
    val Statement   = uri("Statement")
    val subject     = uri("subject")
    val predicate   = uri("predicate")
    val `object`    = uri("object")
    val value       = uri("value")
    val first       = uri("first")
    val rest        = uri("rest")
    val nil         = uri("nil")

    object node {
      val `type`     = Uri(Vocabulary.Rdf.`type`)
      val Property   = Uri(Vocabulary.Rdf.Property)
      val Statement  = Uri(Vocabulary.Rdf.Statement)
      val subject    = Uri(Vocabulary.Rdf.subject)
      val predicate  = Uri(Vocabulary.Rdf.predicate)
      val `object`   = Uri(Vocabulary.Rdf.`object`)
      val value      = Uri(Vocabulary.Rdf.value)
      val first      = Uri(Vocabulary.Rdf.first)
      val rest       = Uri(Vocabulary.Rdf.rest)
      val nil        = Uri(Vocabulary.Rdf.nil)
    }
  }

  /** RDFS Vocabulary: RDF Schema namespace and common schema terms */
  object Rdfs {
    val prefix    = "rdfs"
    val namespace = "http://www.w3.org/2000/01/rdf-schema#"
    def uri(local: String): String = namespace + local

    val Class         = uri("Class")
    val subClassOf    = uri("subClassOf")
    val subPropertyOf = uri("subPropertyOf")
    val domain        = uri("domain")
    val range         = uri("range")
    val label         = uri("label")
    val comment       = uri("comment")
    val seeAlso       = uri("seeAlso")
    val isDefinedBy   = uri("isDefinedBy")
    val Resource      = uri("Resource")

    object node {
      val Class         = Uri(Vocabulary.Rdfs.Class)
      val subClassOf    = Uri(Vocabulary.Rdfs.subClassOf)
      val subPropertyOf = Uri(Vocabulary.Rdfs.subPropertyOf)
      val domain        = Uri(Vocabulary.Rdfs.domain)
      val range         = Uri(Vocabulary.Rdfs.range)
      val label         = Uri(Vocabulary.Rdfs.label)
      val comment       = Uri(Vocabulary.Rdfs.comment)
      val seeAlso       = Uri(Vocabulary.Rdfs.seeAlso)
      val isDefinedBy   = Uri(Vocabulary.Rdfs.isDefinedBy)
      val Resource      = Uri(Vocabulary.Rdfs.Resource)
    }
  }

  /** OWL Vocabulary: Web Ontology Language namespace and common ontology terms */
  object Owl {
    val prefix    = "owl"
    val namespace = "http://www.w3.org/2002/07/owl#"
    def uri(local: String): String = namespace + local

    val Ontology           = uri("Ontology")
    val Class              = uri("Class")
    val ObjectProperty     = uri("ObjectProperty")
    val DatatypeProperty   = uri("DatatypeProperty")
    val AnnotationProperty = uri("AnnotationProperty")
    val sameAs             = uri("sameAs")
    val equivalentClass    = uri("equivalentClass")
    val equivalentProperty = uri("equivalentProperty")
    val inverseOf          = uri("inverseOf")
    val FunctionalProperty = uri("FunctionalProperty")
    val Restriction        = uri("Restriction")
    val onProperty         = uri("onProperty")
    val someValuesFrom     = uri("someValuesFrom")
    val allValuesFrom      = uri("allValuesFrom")
    val unionOf            = uri("unionOf")
    val intersectionOf     = uri("intersectionOf")
    val Thing              = uri("Thing")
    val Nothing            = uri("Nothing")
    val TransitiveProperty = uri("TransitiveProperty")

    object node {
      val Class              = Uri(Vocabulary.Owl.Class)
      val ObjectProperty     = Uri(Vocabulary.Owl.ObjectProperty)
      val DatatypeProperty   = Uri(Vocabulary.Owl.DatatypeProperty)
      val FunctionalProperty = Uri(Vocabulary.Owl.FunctionalProperty)
      val TransitiveProperty = Uri(Vocabulary.Owl.TransitiveProperty)
      val inverseOf          = Uri(Vocabulary.Owl.inverseOf)
      val onProperty         = Uri(Vocabulary.Owl.onProperty)
      val someValuesFrom     = Uri(Vocabulary.Owl.someValuesFrom)
      val allValuesFrom      = Uri(Vocabulary.Owl.allValuesFrom)
      val unionOf            = Uri(Vocabulary.Owl.unionOf)
      val versionInfo        = Uri(Vocabulary.Owl.uri("versionInfo"))
    }
  }

  /** Dublin Core Terms Vocabulary: metadata terms for resources */
  object Dcterms {
    val prefix    = "dcterms"
    val namespace = "http://purl.org/dc/terms/"
    def uri(local: String): String = namespace + local

    val BibliographicResource = uri("BibliographicResource")
    val coverage    = uri("coverage")
    val contributor = uri("contributor")
    val creator     = uri("creator")
    val date        = uri("date")
    val description = uri("description")
    val format      = uri("format")
    val hasPart     = uri("hasPart")
    val identifier  = uri("identifier")
    val isPartOf    = uri("isPartOf")
    val language    = uri("language")
    val modified    = uri("modified")
    val publisher   = uri("publisher")
    val relation    = uri("relation")
    val rights      = uri("rights")
    val source      = uri("source")
    val subject     = uri("subject")
    val title       = uri("title")
    val alternative = uri("alternative")
    val type_       = uri("type")

    object node {
      import org.smartdox.semanticweb.Rdf.Node.Uri
      val alternative = Uri(Vocabulary.Dcterms.alternative)
      val BibliographicResource = Uri(Vocabulary.Dcterms.BibliographicResource)
      val coverage    = Uri(Vocabulary.Dcterms.coverage)
      val contributor = Uri(Vocabulary.Dcterms.contributor)
      val creator     = Uri(Vocabulary.Dcterms.creator)
      val date        = Uri(Vocabulary.Dcterms.date)
      val description = Uri(Vocabulary.Dcterms.description)
      val format      = Uri(Vocabulary.Dcterms.format)
      val hasPart     = Uri(Vocabulary.Dcterms.hasPart)
      val identifier  = Uri(Vocabulary.Dcterms.identifier)
      val isPartOf    = Uri(Vocabulary.Dcterms.isPartOf)
      val language    = Uri(Vocabulary.Dcterms.language)
      val modified    = Uri(Vocabulary.Dcterms.modified)
      val publisher   = Uri(Vocabulary.Dcterms.publisher)
      val relation    = Uri(Vocabulary.Dcterms.relation)
      val rights      = Uri(Vocabulary.Dcterms.rights)
      val source      = Uri(Vocabulary.Dcterms.source)
      val subject     = Uri(Vocabulary.Dcterms.subject)
      val title       = Uri(Vocabulary.Dcterms.title)
      val type_       = Uri(Vocabulary.Dcterms.type_)
    }
  }

  object Schema {
    val prefix    = "schema"
    val namespace = "https://schema.org/"

    def uri(local: String): String = namespace + local
  }
  object Smorg {
    val prefix    = "smorg"
    val namespace = "https://www.simplemodeling.org/ontology/simplemodelingorg#"
    def uri(local: String): String = namespace + local
  }

  object SmorgSchema {
    val prefix    = "smorgschema"
    val namespace = "https://www.simplemodeling.org/schema/simplemodelingorg#"
    def uri(local: String): String = namespace + local
  }

  object SimpleModeling {
    val prefix    = "smd"
    val namespace = "https://www.simplemodeling.org/simplemodeling/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object SimpleModel {
    val prefix    = "smodel"
    val namespace = "https://www.simplemodeling.org/simplemodel/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object SimpleModelSchema {
    val prefix    = "smodelschema"
    val namespace = "https://www.simplemodeling.org/simplemodel/schema/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object SmartDox {
    val prefix    = "sdx"
    val namespace = "https://www.simplemodeling.org/smartdox/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object DocumentModel {
    val prefix    = "doc"
    val namespace = "https://www.simplemodeling.org/documentmodel/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object Bok {
    val prefix    = "bok"
    val namespace = "https://www.simplemodeling.org/bok/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object BokSchema {
    val prefix    = "bokschema"
    val namespace = "https://www.simplemodeling.org/bok/schema/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object Category {
    val prefix    = "cat"
    val namespace = "https://www.simplemodeling.org/category/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object CategorySchema {
    val prefix    = "catschema"
    val namespace = "https://www.simplemodeling.org/category/schema/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object Glossary {
    val prefix    = "glo"
    val namespace = "https://www.simplemodeling.org/glossary/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object Bibliography {
    val prefix    = "bib"
    val namespace = "https://www.simplemodeling.org/bibliography/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object Project {
    val prefix    = "proj"
    val namespace = "https://www.simplemodeling.org/project/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }

  object ComponentRepository {
    val prefix    = "comp"
    val namespace = "https://www.simplemodeling.org/component/ontology/0.1-SNAPSHOT#"
    def uri(local: String): String = namespace + local
  }
}
