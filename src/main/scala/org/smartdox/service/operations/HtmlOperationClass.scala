package org.smartdox.service.operations

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.realm.Realm
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => DoxContext}
import org.smartdox.generators.Dox2HtmlGenerator

/*
 * @since   Dec. 30, 2020
 *  version Jan.  1, 2021
 *  version Feb.  7, 2021
 * @version Apr.  4, 2025
 * @author  ASAMI, Tomoharu
 */
case object HtmlOperationClass extends OperationClassWithOperation {
  val request = HtmlCommand.specification
  val response = HtmlResult.specification
  val specification = spec.Operation("html", request, response)

  def apply(env: Environment, req: Request): Response = {
    val arg1 = req.arguments(0)
    val s = arg1.toInputText
    val filename = arg1.asString
    val dox = _parse(s)
//    println(s"HtmlOperationClass#apply $dox")
    val r = _generate(env, filename, dox)
    FileRealmResponse(r)
  }

  private def _parse(p: String): Dox = {
    val cfg = Dox2Parser.Config.default // TODO
    Dox2Parser.parse(cfg, p)
  }

  private def _generate(env: Environment, filename: String, p: Dox): Realm = {
    val ctx = DoxContext.create(env)
    val b = Realm.Builder()
    b.setObject(filename, p)
    val in = b.build()
    val htmltx = new Dox2HtmlGenerator(ctx)
    htmltx.generate(in)
  }

  object HtmlCommand {
    object params {
      val in = spec.Parameter.argumentFile("in")
    }

    def specification: spec.Request = spec.Request(
      params.in
    )
  }

  object HtmlResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
