package org.smartdox.datasource.provider.csv

import org.goldenport.context._
import org.goldenport.record.v3._
import org.goldenport.record.v2.bag.CsvBag
import org.smartdox.datasource._

/*
 * @since   Oct. 26, 2025
 * @version Oct. 26, 2025
 * @author  ASAMI, Tomoharu
 */
class CsvDataSource(
  locator: CsvDataLocator
) extends DataSource {
  def readRecordTable(): Consequence[ITable] = Consequence {
    val bag = CsvBag.loadResource(locator.resourceHandle)
    bag.toTable
  }
}
