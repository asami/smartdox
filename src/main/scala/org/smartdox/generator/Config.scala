package org.smartdox.generator

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.value._
import org.smartdox.service

/*
 * @since   Jul.  5, 2020
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig
) {
}

object Config {
  def create(p: service.Config) = Config(p.cliConfig)
}
