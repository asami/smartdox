package org.smartdox.specdoc

// import scala.collection.mutable
// import org.goldenport.value.{GTreeNodeBase, GKey}
// import org.goldenport.sdoc.{SDoc, SEmpty}
// import com.asamioffice.goldenport.text.UString
import org.goldenport.values.Designation
import org.smartdox._

/*
 * Derived from SDFeature.java since Feb. 21, 2007.
 *
 * @since   Sep.  4, 2008
 *  version Oct. 18, 2008
 *  version Jun. 14, 2020
 *  version Aug. 15, 2020
 * @version Dec. 14, 2020
 */
case class SDFeature(
  designation: Designation,
  value: Dox,
  description: Dox = EmptyDox
) {
  def name: String = designation.name
  def key: Symbol = designation.key
  // final def description_is(aDesc: Dox): SDFeature = {
  //   description = aDesc
  //   this
  // }

  // final def label_is(aLabel: Dox): SDFeature = {
  //   label = aLabel
  //   this
  // }
}

object SDFeature {
  val empty = SDFeature(???, ???, ???)
}
