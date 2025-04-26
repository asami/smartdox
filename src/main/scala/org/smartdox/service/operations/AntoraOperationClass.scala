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
import org.smartdox.generators.AntoraGenerator

/*
 * @since   Apr. 18, 2025
 * @version Apr. 18, 2025
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
    val realm = Realm.create(cmd.in)
    val ctx = DoxContext.create(env)
    val antora = new AntoraGenerator(ctx)
    val out = antora.generate(realm)
    AntoraResult(out)
  }

  trait Command {
  }

  trait Result {
  }

  case class AntoraCommand(in: File) extends Command {
  }
  object AntoraCommand {
    object params {
      val in = spec.Parameter.argumentFile("in")
    }

    def create(req: Request): AntoraCommand =
      cCreate(req).take

    def cCreate(req: Request): Consequence[AntoraCommand] =
      for {
        in <- req.cFile(params.in)
      } yield {
        AntoraCommand(in)
      }

    def specification: spec.Request = spec.Request(
      params.in
    )
  }

  case class AntoraResult(
    out: Realm
  ) extends Result {
  }
  object AntoraResult {
    def specification: spec.Response = spec.Response(spec.XRealm)
  }
}
