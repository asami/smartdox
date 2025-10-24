package org.smartdox.parser.resolver

import org.goldenport.context.Consequence
import org.goldenport.io.FileTextResolver
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.parser.DoxResolver
import org.smartdox.parser.Dox2Parser

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class TextResolver(
  context: DoxResolver.Context
) extends DoxResolver.Provider {
  private def _parse_context = context.parseContext

  def resolve(path: String): Consequence[Dox] = {
    val ctx = context.fileTextResolverContext
    val resolver = new FileTextResolver(ctx)
    for {
      s <- resolver.resolve(path)
      dox <- _parse(path, s)
    } yield dox
  }

  private def _parse(path: String, s: String): Consequence[Dox] =
    StringUtils.getSuffix(path) match {
      case Some(suffix) => suffix match {
        case "dox" => _parse_dox(s)
        case m => _parse_source(path, m, s)
      }
      case None => _parse_text(s)
    }

  private def _parse_dox(s: String): Consequence[Dox] = {
    val parser = new Dox2Parser(_parse_context)
    for {
      a <- Consequence.from(parser.apply(s))
      r <- _adjust_dox(a)
    } yield r
  }

  private def _adjust_dox(p: Dox): Consequence[Dox] = Consequence {
    p match {
      case m: Document => m.head.title match {
        case Some(s) => Section(s, m.body.contents)
        case None => m.body.toContent
      }
      case m => m
    }
  }

  private def _parse_source(path: String, suffix: String, s: String): Consequence[Dox] = {
    val kind = suffix
    val caption = StringUtils.pathLastComponent(path)
    Consequence.success(Program.create(s, Some(kind), Some(caption)))
  }

  private def _parse_text(s: String): Consequence[Dox] =
    Consequence.success(Text(s))
}
