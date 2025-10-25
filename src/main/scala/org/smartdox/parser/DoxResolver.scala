package org.smartdox.parser

import org.goldenport.context.Consequence
import org.goldenport.io.FileTextResolver
import org.goldenport.util.StringUtils
import org.smartdox.{Dox, Error}
import org.smartdox.Text
import org.smartdox.Document
import org.smartdox.Section
import org.smartdox.Program
import org.smartdox.parser.DoxLinesParser.BlockMacro
import org.smartdox.parser.resolver._

/*
 * @since   Jul. 17, 2025
 *  version Jul. 19, 2025
 *  version Aug. 10, 2025
 * @version Oct. 26, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxResolver(context: DoxResolver.Context) {
  def resolve(directive: BlockMacro.Include): Consequence[Dox] = {
    val path = directive.target
    val params = _adjust(directive.parameters, path)
    val ctx = context.withParameters(params)
    // val ftrc = ctx.fileTextResolverContext
    // _resolve(ftrc, path)
    _resolve(ctx, path)
  }

  private def _adjust(params: FileTextResolver.Parameters, path: String) = {
    def _dox_params_ = params
    def _scala_params_ = params
    def _verbatim_params_ = params

    StringUtils.getSuffix(path) match {
      case Some(s) => s match {
        case "dox" => _dox_params_
        case "scala" => _scala_params_
        case m => _verbatim_params_
      }
      case None => _verbatim_params_
    }
  }

  private def _resolve(
    ctx: DoxResolver.Context,
    path: String
  ): Consequence[Dox] = {
    val resolver = StringUtils.getSuffix(path).collect {
      case "xls" => new ExcelResolver(ctx)
      case "xlsx" => new ExcelResolver(ctx)
      case "csv" => new CsvResolver(ctx)
    }.getOrElse(new TextResolver(ctx))
    resolver.resolve(path)
  }

  // def resolve(path: String): Consequence[Dox] = {
  //   val resolver = StringUtils.getSuffix(path) match {
  //     case "xlsx" => new ExclResolver(context)
  //     case _ => new TextResolver(context)
  //   }
  //   resolver.resolve(path)
  // }

  // def resolve(path: String): Consequence[Dox] = {
  //   val ftrc = context.fileTextResolverContext
  //   _resolve(ftrc, path)
  // }

  // private def _resolve(ctx: FileTextResolver.Context, path: String): Consequence[Dox] = {
  //   val resolver = new FileTextResolver(ctx)
  //   for {
  //     s <- resolver.resolve(path)
  //     dox <- _parse(path, s)
  //   } yield dox
  // }

  // private def _parse(path: String, s: String): Consequence[Dox] =
  //   StringUtils.getSuffix(path) match {
  //     case Some(suffix) => suffix match {
  //       case "dox" => _parse_dox(s)
  //       case m => _parse_source(path, m, s)
  //     }
  //     case None => _parse_text(s)
  //   }

  // private def _parse_dox(s: String): Consequence[Dox] = {
  //   val parser = new Dox2Parser(context.parseContext)
  //   for {
  //     a <- Consequence.from(parser.apply(s))
  //     r <- _adjust_dox(a)
  //   } yield r
  // }

  // private def _adjust_dox(p: Dox): Consequence[Dox] = Consequence {
  //   p match {
  //     case m: Document => m.head.title match {
  //       case Some(s) => Section(s, m.body.contents)
  //       case None => m.body.toContent
  //     }
  //     case m => m
  //   }
  // }

  // private def _parse_source(path: String, suffix: String, s: String): Consequence[Dox] = {
  //   val kind = suffix
  //   val caption = StringUtils.pathLastComponent(path)
  //   Consequence.success(Program.create(s, Some(kind), Some(caption)))
  // }

  // private def _parse_text(s: String): Consequence[Dox] =
  //   Consequence.success(Text(s))
}

object DoxResolver {
  case class Context(
    parseContext: Dox2Parser.ParseContext
  ) {
    def fileResolverContext = parseContext.fileResolverContext
    def fileTextResolverContext = parseContext.fileTextResolverContext

    def withParameters(params: FileTextResolver.Parameters) =
      copy(parseContext = parseContext.withParameters(params))
  }

  trait Provider {
    def resolve(path: String): Consequence[Dox]
  }
}
