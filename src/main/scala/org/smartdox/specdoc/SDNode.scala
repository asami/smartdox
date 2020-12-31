package org.smartdox.specdoc

import scala.collection.mutable.{Buffer, ArrayBuffer}
// import org.goldenport.value.{GTreeNodeBase, GTable, PlainTable, GKey}
// import org.goldenport.sdoc._
// import org.goldenport.sdoc.inline.{SIAnchor, SElementRef}
// import org.goldenport.sdoc.parts.SHistory
// import org.goldenport.entities.specdoc.plain._
import org.goldenport.RAISE
import org.goldenport.tree.TreeNodeBase
import org.goldenport.values.Designation
import org.smartdox._
import org.smartdox.gadget._
import com.asamioffice.goldenport.text.UJavaString

/**
 * SDNode
 * derived from SDNode.java since Feb. 17, 2007
 *
 * <ul>
 *   <li>Name (+ subtitle or headline)</li>
 *   <li>Summary</li>
 *   <li>Overview</li>
 *   <li>Features</li>
 *   <li>Grammar</li>
 *   <li>Slot summary</li>
 *   <li>Description</li>
 *   <li>Slots</li>
 * </ul>
 *
 * @since   Sep.  4, 2008
 *  version Aug.  6, 2009
 *  version Feb. 22, 2012
 *  version Jun. 15, 2020
 *  version Jul. 24, 2020
 *  version Aug. 13, 2020
 *  version Sep. 13, 2020
 *  version Oct. 17, 2020
 *  version Nov.  1, 2020
 * @version Dec. 27, 2020
 * @author  ASAMI, Tomoharu
 */
trait SDNode extends TreeNodeBase[SDNode] {
  def description: Description
  override def name = description.name
  def sdocTitle: Dox = Dox.empty
  def subtitle: Dox = Dox.empty
  def resume: Resume = description.resume
  def caption: Dox = resume.caption
  def explanation: Dox = description.content

  def note: Dox

  def featureSet: SDFeatureSet

  final def features: Seq[SDFeature] = featureSet.features

  final def feature(key: Designation) = featureSet(key.key)
  final def feature(key: GKey) = featureSet(key)

  final def effectiveTitle: Inline = {
    if (!sdocTitle.isEmpty)
      sdocTitle match {
        case m: Inline => m
        case m => RAISE.noReachDefect
      }
    else if (title != null)
      Text(title)
    else
      Text(name)
  }

  def categories: List[SDCategory]

  def isMatch(p: SDCategory): Boolean = categories.contains(p)

  final def addFeature(aKey: Designation, aValue: Dox): SDFeature = {
    // require(aKey != null && aValue != null)
    // val feature = new SDFeature(aKey, aValue)
    // _features += feature
    // feature
    ???
  }

  final def anchor: SIAnchor = {
    // SIAnchor(effectiveTitle) unresolvedRef_is element_Ref
    ???
  }

  def featureTable: Table = ???

  def getFeatureTable: Option[Table] = featureTable.toOption

  def specification: Dox = Dox.empty // Dox.text("specification") // TODO // _specification

  def history: History = History.empty // TODO

//   final def featureTable: GTable[Dox] = {
//     val table = new PlainTable[Dox]
//     table.setHead(featureHead)
//     featureRows.foreach(table += _)
// /* 2008-10-13
//     for (feature <- featureRows)
//       table += (feature.key, feature.value, feature.description)
// */
//     table
//   }
}
// abstract class SDNode(aName: String) extends TreeNodeBase[SDNode] {
//   type TreeNode_TYPE = SDNode
//   content = this
//   set_name(aName)
//   var sdocTitle: Dox = Dox.empty
//   var subtitle: Dox = Dox.empty
//   val resume = Summary() // TODO
//   private var _overview: Dox = Dox.empty
//   private var _specification: Dox = Dox.empty
//   private var _description: Dox = Dox.empty
//   private var _features = new SDFeatureSet
//   var note: Dox = Dox.empty
//   val categories: Buffer[SDCategory] = new ArrayBuffer[SDCategory]
//   val history: SHistory = new SHistory

//   def this() = this(null)

//   def caption: Dox = resume.caption
//   def caption_=(aDoc: Dox) { resume.caption = aDoc }

//   def brief: Dox = resume.brief
//   def brief_=(aDoc: Dox) { resume.brief = aDoc }

//   def summary: Dox = resume.summary
//   def summary_=(aDoc: Dox) { resume.summary = aDoc }

//   def overview: Dox = _overview
//   def overview_=(aDoc: Dox) {
//     require(aDoc != null)
//     _overview = aDoc
//   }

//   def specification: Dox = _specification
//   def specification_=(aDoc: Dox) {
//     require(aDoc != null)
//     _specification = aDoc
//   }

//   def description: Dox = _description
//   def description_=(aDoc: Dox) {
//     require(aDoc != null)
//     _description = aDoc
//   }

//   final def qualifiedName: String = {
//     UJavaString.pathname2className(pathname)
//   }

//   final def effectiveTitle: Dox = {
//     if (!sdocTitle.isNil) sdocTitle
//     else if (title != null) SText(title)
//     else SText(name)
//   }

//   final def anchor: SIAnchor = {
//     SIAnchor(effectiveTitle) unresolvedRef_is element_Ref
//   }

//   protected def element_Ref: SElementRef

//   final def features: Seq[SDFeature] = _features.features

//   final def feature(key: GKey) = _features(key)

//   final def featureHead: Seq[Dox] = _features.tableHead
//   final def featureRows: Seq[Seq[Dox]] = _features.tableRows

//   final def featureTable: GTable[Dox] = {
//     val table = new PlainTable[Dox]
//     table.setHead(featureHead)
//     featureRows.foreach(table += _)
// /* 2008-10-13
//     for (feature <- featureRows)
//       table += (feature.key, feature.value, feature.description)
// */
//     table
//   }

//   final def addFeature(aKey: GKey, aValue: Dox): SDFeature = {
//     require(aKey != null && aValue != null)
//     val feature = new SDFeature(aKey, aValue)
//     _features += feature
//     feature
//   }

// /* 2008-10-21
//   // DSL
//   final def feature(aKey: GKey, aValue: Dox): SDFeature = {
//     addFeature(aKey, aValue)
//   }
// */

//   final def effectiveSummary: Dox = resume.effectiveSummary

//   protected def new_Node(aName: String): SDNode = {
//     new PlainPackage(aName)
//   }
// }
