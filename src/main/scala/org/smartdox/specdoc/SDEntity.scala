package org.smartdox.specdoc

import org.goldenport.values.Designation
import org.goldenport.extension.IRecord
import org.smartdox._

/*
 * Derived from SDPackageNode.java since Feb. 18, 2007
 *
 * @since   Sep.  4, 2008
 *  version Oct. 23, 2008
 *  version Jun. 15, 2020
 *  version Jul. 27, 2020
 *  version Aug. 13, 2020
 *  version Sep. 21, 2020
 *  version Nov. 21, 2020
 *  version Dec. 27, 2020
 *  version Oct. 28, 2024
 * @version Mar.  6, 2025
 */
case class SDEntity(
  description: Description,
  categories: List[SDCategory] = Nil,
  note: Dox = Dox.empty,
  featureSet: SDFeatureSet = SDFeatureSet.empty,
  subEntityList: Seq[SDEntity] = Nil
) extends SDNode {
  def new_Node(name: String): TreeNode_TYPE = ???

  protected def show_Name: String = s"SDNode($name)"
  protected def show_String: String = to_show(description)

  def summary: Dox = description.summary
  def brief: Dox = description.brief

//  def overview: Dox = Dox.text("overview") // TODO

  override def featureTable: Table = featureSet.toTable

  // override def featureTable: Table = {
  //   val head = THead.data("A", "B") // TODO
  //   val data = Vector(
  //     IRecord.data("A" -> "a", "B" -> "b")
  //   )
  //   val body = TBody.create(head, data)
  //   val foot = None
  //   val caption = Caption("featureTable SDEntity") // TODO
  //   val label = None
  //   Table(Some(head), body, foot, Some(caption), label)
  // }

  def subEntities(p: SDCategory): Seq[SDEntity] = subEntityList.filter(_.isMatch(p))

  def getSubEntityTable(category: SDCategory): Option[Table] =
    subEntityTable(category).toOption

  def subEntityTable(category: SDCategory): Table = {
    val head = THead.data("名前", "説明") // TODO
    val data = subEntities(category).map(_sub_entity_to_record)
    val body = TBody.create(head, data)
    val foot = None
    val side = None
    val cg = None
    val caption = Caption("subEntityTable in SDEntity") // TODO
    val label = None
    Table(Some(head), body, foot, side, cg, Some(caption), label)
  }

  private def _sub_entity_to_record(p: SDEntity) = {
    IRecord.data("名前" -> p.name, "説明" -> p.brief) // TODO
  }
}

object SDEntity {
  def apply(name: String): SDEntity = SDEntity(Description.name(name))

  def apply(
    name: String,
    category: SDCategory,
    entities: Seq[SDEntity]
  ): SDEntity = SDEntity(Description.name(name), List(category), subEntityList = entities)

  def apply(
    designation: Designation,
    category: SDCategory,
    entities: Seq[SDEntity]
  ): SDEntity = SDEntity(Description(designation), List(category), subEntityList = entities)

  def apply(
    name: String,
    category: SDCategory,
    description: Dox
  ): SDEntity = SDEntity(Description.name(name, description), List(category))

  def apply(
    designation: Designation,
    category: SDCategory,
    description: Dox
  ): SDEntity = SDEntity(
    Description(designation, description),
    List(category)
  )

  def apply(
    designation: Designation,
    category: SDCategory,
    description: Description
  ): SDEntity = SDEntity(
    description.withDesignation(designation),
    List(category)
  )

  def apply(
    designation: Designation,
    category: SDCategory,
    features: Seq[SDFeature],
    description: Description
  ): SDEntity = SDEntity(
    description.withDesignation(designation),
    List(category),
    featureSet = SDFeatureSet(features.toList)
  )

  def apply(
    designation: Designation,
    category: SDCategory,
    description: Description,
    entities: Seq[SDEntity]
  ): SDEntity = SDEntity(
    description.withDesignation(designation),
    List(category),
    subEntityList = entities
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
