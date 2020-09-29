package org.smartdox.generator

import org.goldenport.recorder.{ForwardRecorder, Recorder}
import org.goldenport.cli.Environment

/*
 * @since   Jul.  5, 2020
 * @version Jul.  5, 2020
 * @author  ASAMI, Tomoharu
 */
class Context(
  val environment: Environment,
  val config: Config
) extends Environment.AppEnvironment with ForwardRecorder {
  protected def forward_Recorder: Recorder = recorder
  private def recorder = environment.recorder
}
