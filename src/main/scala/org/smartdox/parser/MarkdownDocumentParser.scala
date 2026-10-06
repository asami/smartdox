package org.smartdox.parser

import scala.collection.JavaConverters._
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.parser.Parser
import org.goldenport.context.Consequence
import org.smartdox.{Document, I18NFragment, Section}

/*
 * @since   Oct.  6, 2026
 * @version Oct.  6, 2026
 */
private[smartdox] object MarkdownDocumentParser {
  def parseC(config: Dox2Parser.Config, source: String): Consequence[Document] =
    Consequence.execute { _parse(config, source) }

  def parse(config: Dox2Parser.Config, source: String): Document =
    _parse(config, source)

  private def _parse(config: Dox2Parser.Config, source: String): Document = {
    val (body, metadata) = Dox2ParserFrontMatter.split(config, source)
    val extensions = List(TablesExtension.create()).asJava
    val parser = Parser.builder().extensions(extensions).build()
    val parsed = parser.parse(body)
    val document = MarkdownDocumentAst.convert(config, parsed)
    val merged = metadata.fold(document)(Dox2ParserFrontMatter.merge(document, _))
    if (merged.head.metadata.title.nonEmpty)
      merged
    else {
      val title = merged.body.contents.collectFirst {
        case section: Section => I18NFragment.create(section.title)
      }
      merged.copy(head = merged.head.copy(metadata = merged.head.metadata.copy(title = title)))
    }
  }
}
