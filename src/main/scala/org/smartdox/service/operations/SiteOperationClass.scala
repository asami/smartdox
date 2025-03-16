package org.smartdox.service.operations

import java.io.File
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => DoxContext}
import org.smartdox.generators.DoxSiteGenerator

/*
 * @since   Feb. 28, 2025
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case object SiteOperationClass extends OperationClassWithOperation {
  val request = SiteCommand.specification
  val response = SiteResult.specification
  val specification = spec.Operation("site", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = SiteCommand.create(req)
    val r = execute(env, cmd)
    FileRealmResponse(r.out)
  }

  def execute(env: Environment, cmd: SiteCommand): SiteResult = {
    val realm = Realm.create(cmd.in)
    val ctx = DoxContext.create(env)
    val site = new DoxSiteGenerator(ctx)
    val out = site.generate(realm)
    SiteResult(out)
  }

  trait Command {
  }

  trait Result {
  }

  case class SiteCommand(in: File) extends Command {
  }
  object SiteCommand {
    object params {
      val in = spec.Parameter.argumentFile("in")
    }

    def create(req: Request): SiteCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[SiteCommand] =
      for {
        in <- req.cFile(params.in)
      } yield {
        SiteCommand(in)
      }

    def specification: spec.Request = spec.Request(
      params.in
    )
  }

  case class SiteResult(
    out: Realm
  ) extends Result {
  }
  object SiteResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
