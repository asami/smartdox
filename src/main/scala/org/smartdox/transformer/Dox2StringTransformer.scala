package org.smartdox.transformer

import scalaz._
import Scalaz._
import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.tree._
import org.goldenport.util.StringBuildFeature
import org.smartdox._
import Dox._

/*
 * @since   Jan. 12, 2012
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2StringTransformer extends DoxTreeVisitor with StringBuildFeature {
  def transform(dox: Dox): Consequence[String] = Consequence {
    val tree = Dox.toTree(dox)
    tree.traverse(this)
    sb_to_string()
  }

  protected def to_text(ps: Seq[Dox]): String = Dox.toText(ps)
}
