package org.smartdox.service

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.recorder.{ForwardRecorder, Recorder}

/*
 * @since   Dec. 31, 2020
 * @version Dec. 31, 2020
 * @author  ASAMI, Tomoharu
 */
case class Context(
  val environment: Environment,
  val config: Config
) extends Environment.AppEnvironment with ForwardRecorder {
  protected def forward_Recorder: Recorder = recorder

  def recorder = environment.recorder
  def isPlatformWindows: Boolean = environment.isPlatformWindows
}
