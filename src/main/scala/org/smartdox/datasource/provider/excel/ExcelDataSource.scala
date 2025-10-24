package org.smartdox.datasource.provider.excel

import org.goldenport.context._
import org.goldenport.record.v3._
import org.goldenport.record.v2.bag.ExcelBag
import org.smartdox.datasource._

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class ExcelDataSource(
  locator: ExcelDataLocator
) extends DataSource {
  def readRecordTable(): Consequence[ITable] = Consequence {
    val bag = ExcelBag.loadResource(locator.resourceHandle)
    bag.toTable
  }
}
