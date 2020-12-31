package org.smartdox.service

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, _}
import org.goldenport.value._
import org.smartdox.service.operations._

/*
 * @since   Dec. 30, 2020
 * @version Dec. 31, 2020
 * @author  ASAMI, Tomoharu
 */
class SmartDoxService(
  config: Config,
  environment: Environment,
  services: Services,
  operations: Operations
) {
  private val _engine = Engine.standard(services, operations)

  def execute(args: Array[String]) = _engine.apply(environment, args)

  def run(args: Array[String]) {
    execute(args)
  }
}

object SmartDoxService {
  case object SmartDoxServiceClass extends ServiceClass {
    val name = "smartdox"
    val defaultOperation = None
    val operations = Operations(
      HtmlOperationClass,
      BloggerOperationClass
    )
  }

  def main(args: Array[String]) {
    val env0 = Environment.create(args)
    val config = Config.create(env0)
    val context = new Context(env0, config)
    val env = env0.withAppEnvironment(context)
    val services = Services(
      SmartDoxServiceClass
    )
    val dox = new SmartDoxService(config, env, services, SmartDoxServiceClass.operations) // TODO
    dox.run(args)
  }
}
