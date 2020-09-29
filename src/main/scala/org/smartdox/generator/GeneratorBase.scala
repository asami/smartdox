package org.smartdox.generator

import org.goldenport.recorder.Recordable

/*
 * @since   Jun. 21, 2020
 * @version Jun. 21, 2020
 * @author  ASAMI, Tomoharu
 */
trait GeneratorBase extends Recordable {
  def context: Context

  set_Recorder(context)
}
