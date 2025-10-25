package org.smartdox.parser.resolver

import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.io.FileResolver
import org.goldenport.io.ResourceLocator
import org.goldenport.io.ResourceHandle
import org.smartdox._
import org.smartdox.parser.DoxResolver
import org.smartdox.datasource.provider.csv._

/*
 * @since   Oct. 26, 2025
 * @version Oct. 26, 2025
 * @author  ASAMI, Tomoharu
 */
class CsvResolver(
  context: DoxResolver.Context
) extends BinaryResolver(context) {
  protected def create_Dox(l: ResourceLocator, p: ResourceHandle): Consequence[Dox] = {
    val dl = CsvDataLocator(l, p)
    val ds = new CsvDataSource(dl)
    for {
      t <- ds.readRecordTable()
      r <- Table.createC(t)
    } yield r
  }
}
