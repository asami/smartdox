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
 * @since   Dec. 31, 2020
 * @version Dec. 31, 2020
 * @author  ASAMI, Tomoharu
 */
case object BloggerOperationClass extends OperationClassWithOperation {
  val request = spec.Request.empty
  val response = spec.Response.empty
  val specification = spec.Operation("blogger", request, response)

  def apply(env: Environment, req: Request): Response = {
    val s = req.arguments(0).toInputText
    val dox = _parse(s)
    val r = _generate(env, dox)
    FileRealmResponse(r)
  }

  private def _parse(p: String): Dox = {
    val cfg = Dox2Parser.Config.default // TODO
    Dox2Parser.parse(cfg, p)
  }

  private def _generate(env: Environment, p: Dox): Realm = {
    val ctx = DoxContext.create(env)
    val b = Realm.Builder()
    b.setObject("x.org", p)
    val in = b.build()
    val htmltx = new Dox2HtmlGenerator(ctx)
    htmltx.generate(in)
  }
}
