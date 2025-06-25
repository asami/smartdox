package org.smartdox.service.operations

import java.io.File
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import org.goldenport.tree.TreeTransformer
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.generator.{Context => DoxContext}
import org.smartdox.generators.DoxSiteGenerator
import org.smartdox.doxsite.DoxSite

/*
 * @since   Feb. 28, 2025
 *  version Mar.  2, 2025
 * @version Jun. 23, 2025
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
    val realm = Realm.create(DoxSite.realmConfig, cmd.in)
    val ctx = DoxContext.create(env)
    val config = DoxSite.Config.create(cmd)
    val site = new DoxSiteGenerator(ctx, config)
    val out = site.generate(realm)
    SiteResult(out)
  }

  case class SiteCommand(
    siteParameters: SiteParameters
  ) extends Command with SiteParameters.Holder {
  }
  object SiteCommand {
    object params extends SiteParameters.Specification {
    }

    def create(req: Request): SiteCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[SiteCommand] =
      for {
        sp <- SiteParameters.createC(req)
      } yield {
        SiteCommand(sp)
      }

    def specification: spec.Request = SiteParameters.request
  }

  case class SiteResult(
    out: Realm
  ) extends Result {
  }
  object SiteResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
