package org.smartdox.datasource.provider.csv

import org.goldenport.io.ResourceLocator
import org.goldenport.io.ResourceHandle
import org.smartdox._
import org.smartdox.datasource._

/*
 * @since   Oct. 26, 2025
 * @version Oct. 26, 2025
 * @author  ASAMI, Tomoharu
 */
case class CsvDataLocator(
  resource: ResourceLocator,
  resourceHandle: ResourceHandle
) extends DataLocator {
}
