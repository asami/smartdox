package org.smartdox.specdoc

// import scala.collection.mutable.ArrayBuffer
// import org.goldenport.value.{GTreeNodeBase, GKey}
// import org.goldenport.sdoc.SDoc
// import org.goldenport.sdoc.inline._
import org.goldenport.values.Designation
import org.smartdox._

/*
 * @since   Sep. 13, 2008
 *  version Jul. 15, 2010
 *  version Jun. 14, 2020
 *  version Jul. 27, 2020
 *  version Oct. 11, 2020
 * @version Dec. 14, 2020
 * @author  ASAMI, Tomoharu
 */
case class SDFeatureSet(
  // designation: Designation,
  features: List[SDFeature] = Nil
) {
  final def apply(key: GKey): SDFeature = {
    features.find(key == _.key) match {
      case Some(feature) => feature
      case None => SDFeature.empty // new SDFeature(new GKey("Null"), "-")
    }
  }

  final def tableHead: List[Inline] = Dox.list("項目", "値", "説明")

  def toTable: Table = {
    val b = Table.Builder.header(tableHead)
    for (x <- features) {
      b.append(Dox.text(x.name), x.value, x.description)
    }
    b.apply()
  }
}

object SDFeatureSet {
  val empty = SDFeatureSet()
}

// class SDFeatureSet {
//   private val _features = new ArrayBuffer[SDFeature]

//   final def apply(key: GKey): SDFeature = {
//     _features.find(key == _.key) match {
//       case Some(feature) => feature
//       case None => new SDFeature(new GKey("Null"), "-")
//     }
//   }

//   final def features: List[SDFeature] = _features.toList

//   final def tableHead: List[SDoc] = List("項目", "値", "説明")

//   final def tableRows: List[List[SDoc]] = {
//     for (feature <- _features.toList) yield List(key_literal(feature.key), feature.value, feature.description)
//   }

//   private def key_literal(aKey: GKey): SDoc = {
//     SIAnchor(aKey.label) unresolvedRef_is SHelpRef(aKey.name)
//   }

//   final def +=(aFeature: SDFeature): SDFeature = {
//     _features += aFeature
//     aFeature
//   }

//   final def +=(aKey: GKey, aValue: SDoc) {
//     val feature = new SDFeature(aKey, aValue)
//     _features += feature
//     feature
//   }
// }

