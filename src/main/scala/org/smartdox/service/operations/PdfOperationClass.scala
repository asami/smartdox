package org.smartdox.service.operations

import org.goldenport.context.Consequence
import org.goldenport.cli._
import org.goldenport.realm.Realm
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.AntoraGenerator
import org.smartdox.doxsite.DoxSite

/*
 * @since   Apr.  9, 2026
 * @version Apr.  9, 2026
 * @author  ASAMI, Tomoharu
 */
case object PdfOperationClass extends OperationClassWithOperation {
  val request = PdfCommand.specification
  val response = PdfResult.specification
  val specification = spec.Operation("pdf", request, response)

  def apply(env: Environment, req: Request): Response = {
    val cmd = PdfCommand.create(req)
    val r = execute(env, cmd)
    FileRealmResponse(r.out)
  }

  // PDF generation currently delegates to the Antora-based backend.
  def execute(env: Environment, cmd: PdfCommand): PdfResult = {
    val realm = SiteInputRealm.create(cmd)
    val ctx = GeneratorContext.create(env)
    val config = DoxSite.Config.create(cmd)
    val antora = new AntoraGenerator(ctx, config)
    val out = antora.generate(realm)
    PdfResult(out)
  }

  case class PdfCommand(
    siteParameters: SiteParameters
  ) extends Command with SiteParameters.Holder {
  }
  object PdfCommand {
    object params extends SiteParameters.Specification {
    }

    def create(req: Request): PdfCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[PdfCommand] =
      for {
        sp <- SiteParameters.createC(req)
      } yield {
        PdfCommand(sp)
      }

    def specification: spec.Request = SiteParameters.request
  }

  case class PdfResult(
    out: Realm
  ) extends Result {
  }
  object PdfResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
