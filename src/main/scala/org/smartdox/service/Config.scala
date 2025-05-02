package org.smartdox.service

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.value._

/*
 * @since   Dec. 31, 2020
 *  version Dec. 31, 2020
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  isLocation: Boolean = true
) {
}

object Config {
  def create(env: Environment): Config = {
    Config(
      env.config
    )
  }

  // def create(p: service.Config): Config = create(p.environment)
}
