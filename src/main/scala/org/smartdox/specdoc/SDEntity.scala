package org.smartdox.specdoc

import org.goldenport.values.Designation
import org.smartdox._

/*
 * Derived from SDPackageNode.java since Feb. 18, 2007
 *
 * @since   Sep.  4, 2008
 *  version Oct. 23, 2008
 *  version Jun. 15, 2020
 *  version Jul. 27, 2020
 *  version Aug. 13, 2020
 * @version Sep. 21, 2020
 */
case class SDEntity(
  designation: Designation,
  categories: List[SDCategory] = Nil,
  description: Description = Description.empty,
  note: Dox = Dox.empty,
  featureSet: SDFeatureSet = SDFeatureSet.empty,
  subEntityList: Seq[SDEntity] = Nil
) extends SDNode {
  def new_Node(name: String): TreeNode_TYPE = ???

  def effectiveSummary: Dox = ???
  def overview: Dox = ???

  def subEntities(p: SDCategory): Seq[SDEntity] = ???

  def subEntitiesTable(category: SDCategory): Table = ???
}

object SDEntity {
  def apply(name: String): SDEntity = SDEntity(Designation(name))

  def apply(
    name: String,
    category: SDCategory,
    entities: Seq[SDEntity]
  ): SDEntity = SDEntity(Designation(name), List(category), subEntityList = entities)

  def apply(
    designation: Designation,
    category: SDCategory,
    entities: Seq[SDEntity]
  ): SDEntity = SDEntity(designation, List(category), subEntityList = entities)

  def apply(
    name: String,
    category: SDCategory,
    description: Dox
  ): SDEntity = SDEntity(Designation(name), List(category), Description(description))

  def apply(
    designation: Designation,
    category: SDCategory,
    features: Seq[SDFeature],
    description: Description
  ): SDEntity = SDEntity(
    designation,
    List(category),
    description,
    featureSet = SDFeatureSet(features.toList)
  )
}

// class SDEntity(aName: String) extends SDNode(aName) {
//   var category: SDCategory = _

//   def this() = this(null)

//   override protected def element_Ref: SElementRef = {
//     parent match {
//       case pkg: SDPackage => new SElementRef(UJavaString.pathname2className(pkg.pathname),
// 					 name)
//       case entity: SDEntity => new SElementRef(UJavaString.pathname2packageName(entity.pathname), entity.name, name)
//       case _ => sys.error("not implemented yet.")
//     }
//   }

//   // XXX DSL?
//   final def entity(anEntity: SDEntity): SDEntity = addSubEntity(anEntity)

//   final def addSubEntity(anEntity: SDEntity): SDEntity = {
//     addChild(anEntity).asInstanceOf[SDEntity]
//   }

//   final def subEntities: Seq[SDEntity] = {
//     children.filter(_.isInstanceOf[SDEntity]).asInstanceOf[Seq[SDEntity]]
//   }

//   final def subEntities(category: SDCategory): Seq[SDEntity] = {
//     for (child <- children
// 	 if (child.isInstanceOf[SDEntity] &&
// 	     child.asInstanceOf[SDEntity].category == category)
//        ) yield child.asInstanceOf[SDEntity]
//   }

//   final def subEntitiesTable(aCategory: SDCategory): GTable[SDoc] = {
//     val table = new PlainTable[SDoc]
//     table.setHead(aCategory.tableHead)
//     for (entity <- subEntities(aCategory)) {
//       table += aCategory.tableRow(entity)
//     }
//     table
//   }
// }
