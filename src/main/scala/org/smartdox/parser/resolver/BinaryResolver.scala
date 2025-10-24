package org.smartdox.parser.resolver

import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.io.FileResolver
import org.goldenport.io.ResourceLocator
import org.goldenport.io.ResourceHandle
import org.goldenport.io.LocatorBagResourceHandle
import org.goldenport.bag.ChunkBag
import org.smartdox._
import org.smartdox.parser.DoxResolver

/*
 * @since   Oct. 24, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class BinaryResolver(
  context: DoxResolver.Context
) extends DoxResolver.Provider {
  def resolve(path: String): Consequence[Dox] = {
    val ctx = context.fileResolverContext
    val fr = new FileResolver(ctx)
    for {
      lb <- fr.resolveWithLocator(path)
      dox <- create_dox(lb._1, lb._2)
    } yield dox
  }

  protected def create_dox(l: ResourceLocator, p: ChunkBag): Consequence[Dox] = {
    val rh = new LocatorBagResourceHandle(l, p)
    create_Dox(l, rh)
  }

  protected def create_Dox(l: ResourceLocator, p: ResourceHandle): Consequence[Dox]
}
