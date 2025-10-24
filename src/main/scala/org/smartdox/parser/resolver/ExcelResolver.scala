package org.smartdox.parser.resolver

import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.io.FileResolver
import org.goldenport.io.ResourceLocator
import org.goldenport.io.ResourceHandle
import org.smartdox._
import org.smartdox.parser.DoxResolver
import org.smartdox.datasource.provider.excel._

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class ExcelResolver(
  context: DoxResolver.Context
) extends BinaryResolver(context) {
  protected def create_Dox(l: ResourceLocator, p: ResourceHandle): Consequence[Dox] = {
    val dl = ExcelDataLocator(l, p)
    val ds = new ExcelDataSource(dl)
    for {
      t <- ds.readRecordTable()
      r <- Table.createC(t)
    } yield r
  }
}
