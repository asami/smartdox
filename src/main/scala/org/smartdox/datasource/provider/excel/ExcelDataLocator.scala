package org.smartdox.datasource.provider.excel

import org.goldenport.io.ResourceLocator
import org.goldenport.io.ResourceHandle
import org.smartdox._
import org.smartdox.datasource._

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
case class ExcelDataLocator(
  resource: ResourceLocator,
  resourceHandle: ResourceHandle
) extends DataLocator {
}
