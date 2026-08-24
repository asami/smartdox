package org.smartdox.service.operations

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import org.smartdox.generator.Context
import org.smartdox.parser.Dox2Parser
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Aug. 24, 2026
 * @version Aug. 24, 2026
 * @author  ASAMI, Tomoharu
 */
private[operations] object SmartDoxPublicationProjection {
  private[operations] sealed trait Result {
    def sourceName: String
    def sourceDigest: String
  }

  private[operations] final case class Projected(
    sourceName: String,
    sourceDigest: String,
    html: String,
    pdfRenderer: PdfOperationClass.PdfRenderer
  ) extends Result

  private[operations] final case class Rejected(
    sourceName: String,
    sourceDigest: String
  ) extends Result

  def project(sourceName: String, source: String): Result = {
    val sourcedigest = _source_digest(source)
    _canonical_config(sourceName) match {
      case Some(_) if _contains_include_directive(source) => Rejected(sourceName, sourcedigest)
      case Some(config) =>
        Projected(
          sourceName,
          sourcedigest,
          _html(Dox2Parser.parse(config, source)),
          PdfOperationClass.PdfRenderer.Latex
        )
      case None => Rejected(sourceName, sourcedigest)
    }
  }

  private def _canonical_config(sourcename: String): Option[Dox2Parser.Config] = {
    val suffix = _suffix(sourcename)
    suffix match {
      case Some("dox") => Some(_non_resolving_config(Dox2Parser.Config.smartdox))
      case Some("md") | Some("markdown") => Some(_non_resolving_config(Dox2Parser.Config.markdown))
      case _ => None
    }
  }

private def _contains_include_directive(source: String): Boolean =
  source.linesIterator.exists { line =>
    val trimmedline = line.trim
    trimmedline.matches("include::[^\\[\\]\\s]+\\[[^\\]]*\\]") ||
      trimmedline.matches("(?i)#\\+INCLUDE(?:[ \\t]*:[ \\t]*|[ \\t]+)[^\\s\\r\\n]+(?:[ \\t]+.*)?")
  }

  private def _non_resolving_config(config: Dox2Parser.Config): Dox2Parser.Config =
    new Dox2Parser.Config(
      config.isDebug,
      config.isLocation,
      config.blocksConfig,
      config.linesConfig,
      config.file,
      config.style
    ) {
      override def isResolve: Boolean = false
    }

  private def _suffix(sourcename: String): Option[String] = {
    val index = sourcename.lastIndexOf('.')
    if (index >= 0 && index + 1 < sourcename.length)
      Some(sourcename.substring(index + 1))
    else
      None
  }

  private def _source_digest(source: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def _html(dox: org.smartdox.Dox): String =
    Dox2HtmlTransformer(Context.create(), Dox2HtmlTransformer.Rule.default).
      transform(dox).
      take
}
