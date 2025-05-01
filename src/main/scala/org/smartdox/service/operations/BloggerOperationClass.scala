package org.smartdox.service.operations

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.realm.Realm
// import org.goldenport.util.StringUtils
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => DoxContext}
import org.smartdox.generators.Dox2BloggerGenerator

/*
 * @since   Dec. 31, 2020
 *  version Jan.  3, 2021
 *  version Feb.  1, 2021
 *  version Jan.  1, 2025
 * @version Mar. 31, 2025
 * @author  ASAMI, Tomoharu
 */
case object BloggerOperationClass extends OperationClassWithOperation {
  val request = BloggerCommand.specification
  val response = BloggerResult.specification
  val specification = spec.Operation("blogger", request, response)

  def apply(env: Environment, req: Request): Response = {
    val arg1 = req.arguments(0)
    val s = arg1.toInputText
    val filename = arg1.asString
    val dox = Dox2Parser.parseWithFilename(filename, s)
    val r = _generate(env, filename, dox)
    FileRealmResponse(r)
  }

  // private def _parse(filename: String, p: String): Dox = {
  //   def _default_ = Dox2Parser.Config.default

  //   val cfg = StringUtils.getSuffix(filename).fold(_default_) {
  //     case "dox" => Dox2Parser.Config.smartdox
  //     case "org" => Dox2Parser.Config.orgmode
  //     case "md" => Dox2Parser.Config.markdown
  //     case "markdown" => Dox2Parser.Config.markdown
  //     case _ => _default_
  //   }
  //   _parse(cfg, p)
  // }

  // private def _parse(cfg: Dox2Parser.Config, p: String): Dox =
  //   Dox2Parser.parse(cfg, p)

  private def _generate(env: Environment, filename: String, p: Dox): Realm = {
    val ctx = DoxContext.create(env)
    val b = Realm.Builder()
    b.setObject(filename, p)
    val in = b.build()
    val htmltx = new Dox2BloggerGenerator(ctx)
    htmltx.generate(in)
  }

  object BloggerCommand {
    object params {
      val in = spec.Parameter.argumentFile("in")
    }

    def specification: spec.Request = spec.Request(
      params.in
    )
  }

  object BloggerResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
