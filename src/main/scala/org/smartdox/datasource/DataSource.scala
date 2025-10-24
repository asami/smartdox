package org.smartdox.datasource

import org.goldenport.context.Consequence
import org.goldenport.record.v3.ITable
import org.smartdox._

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
trait DataSource {
  def readTable(): Consequence[Table] = {
    for {
      t <- readRecordTable()
      r <- Table.createC(t)
    } yield r
  }

  def readRecordTable(): Consequence[ITable]
}
