package org.smartdox.generator

import org.goldenport.recorder.Recordable

/*
 * @since   Jun. 21, 2020
 * @version Apr. 28, 2025
 * @author  ASAMI, Tomoharu
 */
trait GeneratorBase extends Recordable with Context.Holder {
  def context: Context

  set_Recorder(context)
}
