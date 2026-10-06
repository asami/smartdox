package org.smartdox.parser

import java.net.URI
import java.nio.file.Paths
import org.goldenport.RAISE
import org.goldenport.parser.{CharEvent, ParseResult}
import org.smartdox.{Dox, ReferenceImg}
import org.smartdox.parser.DoxInlineParser._

/*
 * @since   Aug. 29, 2026
 *  version Aug. 30, 2026
 * @version Oct.  6, 2026
 * @author  ASAMI, Tomoharu
 */
private[parser] object MarkdownImageInlineParser {
  def state(config: Config, parent: DoxInlineParseState, evt: CharEvent): DoxInlineParseState =
    if (evt.next.contains('['))
      MarkdownImageDelimiterState(config, MarkdownImageAltState(config, parent), '[', "![")
    else
      parent.returnFrom(evt.c)

  private case class MarkdownImageDelimiterState(
    config: Config,
    parent: DoxInlineParseState,
    delimiter: Char,
    source: String
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def end_Result(): ParseResult[Dox] =
      MarkdownImageAdmission.malformed(config, source, None)

    override protected def character_State(c: Char): DoxInlineParseState =
      if (c == delimiter)
        leave_none
      else
        RAISE.noReachDefect(this, s"MarkdownImageDelimiterState#character_State($this): $c")
  }

  private case class MarkdownImageAltState(
    config: Config,
    parent: DoxInlineParseState,
    alt: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def end_Result(): ParseResult[Dox] =
      MarkdownImageAdmission.malformed(config, s"![${alt.mkString}", None)

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case ']' if evt.next.contains('(') =>
          MarkdownImageDelimiterState(
            config,
            MarkdownImagePathState(config, parent, alt.mkString),
            '(',
            s"![${alt.mkString}]("
          )
        case ']' if evt.next.contains('[') =>
          MarkdownImageDelimiterState(
            config,
            MarkdownImageReferenceState(config, parent, alt.mkString),
            '[',
            s"![${alt.mkString}]["
          )
        case ']' =>
          MarkdownImageAdmission.malformed(config, s"![${alt.mkString}]", None)
        case c =>
          copy(alt = alt :+ c)
      }
  }

  private case class MarkdownImageReferenceState(
    config: Config,
    parent: DoxInlineParseState,
    alt: String,
    reference: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def use_bracket: Boolean = true

    override protected def end_Result(): ParseResult[Dox] =
      MarkdownImageAdmission.malformed(config, s"![$alt][${reference.mkString}", None)

    override protected def close_Bracket_State(evt: CharEvent): DoxInlineParseState =
      MarkdownImageAdmission.malformed(config, s"![$alt][${reference.mkString}]", None)

    override protected def open_Bracket_State(c: Char): DoxInlineParseState =
      character_State(c)

    override protected def character_State(c: Char): DoxInlineParseState =
      copy(reference = reference :+ c)
  }

  private case class MarkdownImagePathState(
    config: Config,
    parent: DoxInlineParseState,
    alt: String,
    rawpath: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def end_Result(): ParseResult[Dox] = {
      val rawpathtext = rawpath.mkString
      MarkdownImageAdmission.malformed(
        config,
        s"![$alt]($rawpathtext",
        if (rawpathtext.isEmpty) None else Some(rawpathtext)
      )
    }

    override protected def character_State(c: Char): DoxInlineParseState =
      if (c == ')') {
        val path = rawpath.mkString
        val source = s"![$alt]($path)"
        leave_to(MarkdownImageAdmission.admit(config, alt, source, path))
      } else {
        copy(rawpath = rawpath :+ c)
      }
  }

}

private[parser] object MarkdownImageAdmission {
  def admit(config: Config, alt: String, source: String, rawPath: String): ReferenceImg = {
    if (_is_non_exact_path(rawPath))
      malformed(config, source, Some(rawPath))
    val uri = try {
      new URI(rawPath)
    } catch {
      case _: java.net.URISyntaxException => malformed(config, source, Some(rawPath))
    }
    if (uri.getRawQuery != null || uri.getRawFragment != null)
      malformed(config, source, Some(rawPath))
    val path = Option(uri.getPath).getOrElse("")
    if (rawPath.isEmpty || uri.isAbsolute || uri.getRawAuthority != null || path.isEmpty)
      _unsupported_resource(config, source, Some(rawPath))
    config._resource_origin_context match {
      case ResourceOrigin.Physical(rootpath) =>
        val candidate = try {
          Paths.get(path)
        } catch {
          case _: java.nio.file.InvalidPathException => _unsupported_resource(config, source, Some(rawPath))
        }
        if (candidate.isAbsolute)
          _unsupported_resource(config, source, Some(rawPath))
        val normalizedroot = rootpath.toAbsolutePath.normalize
        val resolved = normalizedroot.resolve(candidate).normalize
        if (!resolved.startsWith(normalizedroot))
          _unsupported_resource(config, source, Some(rawPath))
        val relative = normalizedroot.relativize(resolved).toString.replace(java.io.File.separatorChar, '/')
        _create_reference(config, alt, relative, source, rawPath)
      case ResourceOrigin.Virtual(parentsegments) =>
        val relative = _normalize_virtual_path(parentsegments, path).
          getOrElse(_unsupported_resource(config, source, Some(rawPath)))
        _create_reference(config, alt, relative, source, rawPath)
      case ResourceOrigin.Absent =>
        _unsupported_resource(config, source, Some(rawPath))
    }
  }

  private def _create_reference(
    config: Config,
    alt: String,
    relative: String,
    source: String,
    rawpath: String
  ): ReferenceImg = {
    if (!config.isImageFile(relative))
      _unsupported_resource(config, source, Some(rawpath))
    val normalized = new URI(null, null, relative, null)
    ReferenceImg(normalized, Some(alt), location = config.location)
  }

  private def _normalize_virtual_path(
    parentsegments: Vector[String],
    path: String
  ): Option[String] = {
    if (path.startsWith("/"))
      None
    else {
      val rootsize = parentsegments.size
      var segments = parentsegments
      var escaped = false
      path.split("/", -1).foreach {
        case "" | "." =>
        case ".." =>
          if (segments.size > rootsize)
            segments = segments.dropRight(1)
          else
            escaped = true
        case segment =>
          segments = segments :+ segment
      }
      if (escaped) None else Some(segments.drop(rootsize).mkString("/"))
    }
  }

  def malformed(config: Config, source: String, rawPath: Option[String]): Nothing =
    _raise("image.markdown.malformed", config, source, rawPath)

  private def _is_non_exact_path(path: String): Boolean =
    path.exists(c => Character.isWhitespace(c) || c == '\"' || c == '<' || c == '>')

  private def _unsupported_resource(config: Config, source: String, rawpath: Option[String]): Nothing =
    _raise("image.markdown.unsupported-resource", config, source, rawpath)

  private def _raise(name: String, config: Config, source: String, rawpath: Option[String]): Nothing = {
    val location = config.location.map(_.toString).getOrElse("<absent>")
    val path = rawpath.getOrElse("<absent>")
    throw new IllegalArgumentException(s"$name: location=$location source=$source raw-path=$path")
  }
}
