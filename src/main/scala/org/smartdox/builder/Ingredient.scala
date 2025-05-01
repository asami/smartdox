package org.smartdox.builder

import org.goldenport.parser.ParseLocation
import org.goldenport.bag.ChunkBag
import org.smartdox._

/*
 * @since   Aug. 11, 2020
 *  version Aug. 11, 2020
 * @version Dec. 22, 2024
 * @author  ASAMI, Tomoharu
 */
sealed trait Ingredient extends Dox {
}

case class FigureIngredient(
  binary: ChunkBag,
  src: String,
  title: String,
  location: Option[ParseLocation] = None
) extends Ingredient {
  override protected def equals_Value(o: Dox) = o match {
    case m: FigureIngredient => super.equals(m)
    case _ => false
  }
}
