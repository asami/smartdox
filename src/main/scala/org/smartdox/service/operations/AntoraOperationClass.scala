package org.smartdox.service.operations

import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NString
import org.goldenport.cli._
import org.goldenport.realm.Realm
import org.goldenport.util.StringUtils
import org.smartdox.Dox
import org.smartdox.parser.Dox2Parser
import org.smartdox.service.{Context => ServiceContext}
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.AntoraGenerator
import org.smartdox.doxsite.DoxSite

/*
 * @since   Apr. 18, 2025
 *  version Apr. 18, 2025
 *  version May.  2, 2025
 * @version Jun. 28, 2025
 * @author  ASAMI, Tomoharu
 */
case object AntoraOperationClass extends OperationClassWithOperation {
  val request = AntoraCommand.specification
  val response = AntoraResult.specification
  val specification = spec.Operation("antora", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = AntoraCommand.create(req)
    val r = execute(env, cmd)
    FileRealmResponse(r.out)
  }

  def execute(env: Environment, cmd: AntoraCommand): AntoraResult = {
    val realm = Realm.create(DoxSite.realmConfig, cmd.in)
    val sctx = env.toAppEnvironment[ServiceContext]
    val ctx = GeneratorContext.create(sctx)
    val config = DoxSite.Config.create(cmd)
    val antora = new AntoraGenerator(ctx, config)
    val out = antora.generate(realm)
    AntoraResult(out)
  }

  case class AntoraCommand(
    siteParameters: SiteParameters
  ) extends Command with SiteParameters.Holder {
  }
  object AntoraCommand {
    object params extends SiteParameters.Specification {
    }

    def create(req: Request): AntoraCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[AntoraCommand] =
      for {
        sp <- SiteParameters.createC(req)
      } yield {
        AntoraCommand(sp)
      }

    def specification: spec.Request = SiteParameters.request
  }

  case class AntoraResult(
    out: Realm
  ) extends Result {
  }
  object AntoraResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
