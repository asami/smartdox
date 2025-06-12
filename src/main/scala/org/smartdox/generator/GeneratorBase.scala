package org.smartdox.generator

import org.goldenport.recorder.Recordable

/*
 * @since   Jun. 21, 2020
 *  version Apr. 28, 2025
 * @version Jun.  9, 2025
 * @author  ASAMI, Tomoharu
 */
trait GeneratorBase extends Recordable with Context.Holder {
  def context: Context

  implicit protected val dt_context = context.dateTimeContext

  set_Recorder(context)
}
