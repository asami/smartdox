package org.smartdox.specdoc

import org.goldenport.bag.ChunkBag
import org.smartdox._

/*
 * Derived from SDPackageNode.java since Feb. 18, 2007
 *
 * @since   Sep.  4, 2008
 *  version Oct. 22, 2008
 *  version Jun. 15, 2020
 *  version Jul. 24, 2020
 * @version Sep. 21, 2020
 */
case class SDPackage(
  description: Description,
  note: Dox,
  featureSet: SDFeatureSet,
  categories: List[SDCategory] = Nil
) extends SDNode {
  def new_Node(name: String): TreeNode_TYPE = ???

  def entities: Seq[SDEntity] = children.collect {
    case m: SDEntity => m
  }

  def effectiveSummary: Dox = ???
  def overview: Dox = ???

  override def featureTable: Table = ???

  def entitiesTable(category: SDCategory): Table = ???

  override def specification: Dox = ???

//  def summaries: Seq[SDSummary] = ???
}

object SDPackage {
  val empty = new SDPackage(Description.empty, Dox.empty, SDFeatureSet.empty)

  class Builder(node: SDPackage) {
    var children: Vector[SDPackage.Builder] = Vector.empty
    var description: Description = Description.empty
    var categories: Vector[SDCategory] = Vector.empty
    var featureSet: SDFeatureSet = SDFeatureSet.empty
//    var figures: Vector[SDFigure] = Vector.empty
    var contents: Vector[Dox] = Vector.empty
    var entities: Vector[SDEntity] = Vector.empty

    def apply(): SDPackage = ???

    def addUpPackage(name: String): SDPackage.Builder = {
      val pkg = SDPackage(name)
      val r = new Builder(pkg)
      children = children :+ r
      r
    }

    def setDescription(p: Description): SDPackage.Builder = {
      description = p
      this
    }

    def setFeatureSet(p: SDFeatureSet): SDPackage.Builder = {
      featureSet = p
      this
    }

    def addCategories(ps: Seq[SDCategory]): SDPackage.Builder = {
      categories = categories ++ ps
      this
    }

    def addFigure(binary: ChunkBag, src: String, name: String): SDPackage.Builder = {
      contents = contents :+ Figure(BinaryImg(src, binary), name)
      this
    }

    def addEntity(p: SDEntity): SDPackage.Builder = {
      entities = entities :+ p
      this
    }
  }

  def apply(name: String): SDPackage = SDPackage(Description(name), Dox.empty, SDFeatureSet.empty)
}
