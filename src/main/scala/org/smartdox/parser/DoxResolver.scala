package org.smartdox.parser

import org.goldenport.context.Consequence
import org.goldenport.io.FileTextResolver
import org.goldenport.util.StringUtils
import org.smartdox.{Dox, Error}
import org.smartdox.parser.DoxLinesParser.BlockMacro

/*
 * @since   Jul. 17, 2025
 * @version Jul. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxResolver(context: DoxResolver.Context) {
  def resolve(directive: BlockMacro.Include): Consequence[Dox] = {
    val path = directive.target
    val params = _adjust(directive.parameters, path)
    val ctx = context.withParameters(params)
    val ftrc = ctx.fileTextResolverContext
    _resolve(ftrc, path)
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

  def resolve(path: String): Consequence[Dox] = {
    val ftrc = context.fileTextResolverContext
    _resolve(ftrc, path)
  }

  private def _resolve(ctx: FileTextResolver.Context, path: String): Consequence[Dox] = {
    val resolver = new FileTextResolver(ctx)
    resolver.resolve(path) match {
      case Some(s) =>
        val parser = new Dox2Parser(context.parseContext)
        Consequence.from(parser.apply(s))
      case None => Consequence.resourceNotFound(path)
    }
  }
}

object DoxResolver {
  case class Context(
    parseContext: Dox2Parser.ParseContext
  ) {
    def fileTextResolverContext = parseContext.fileTextResolverContext

    def withParameters(params: FileTextResolver.Parameters) =
      copy(parseContext = parseContext.withParameters(params))
  }
}
