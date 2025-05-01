package org.smartdox.specdoc

import org.goldenport.bag.ChunkBag
import org.goldenport.extension.IRecord
import org.goldenport.io.MimeType
import org.goldenport.util.StringUtils
import org.smartdox._

/*
 * Derived from SDPackageNode.java since Feb. 18, 2007
 *
 * @since   Sep.  4, 2008
 *  version Oct. 22, 2008
 *  version Jun. 15, 2020
 *  version Jul. 24, 2020
 *  version Sep. 21, 2020
 *  version Oct. 19, 2020
 *  version Nov.  1, 2020
 *  version Dec. 27, 2020
 *  version Jul. 12, 2021
 *  version Oct. 28, 2024
 * @version Mar.  6, 2025
 */
case class SDPackage(
  description: Description,
  note: Dox,
  featureSet: SDFeatureSet,
  categories: List[SDCategory] = Nil
) extends SDNode {
  def new_Node(name: String): TreeNode_TYPE = ???

  protected def show_Name: String = s"SDPackage($name)"
  protected def show_String: String = to_show(description)

  def entities: Seq[SDEntity] = children.collect {
    case m: SDEntity => m
  }

  def effectiveSummary: Dox = description.effectiveSummary

  def overview: Dox = description.effectiveSummary

  override def featureTable: Table = featureSet.toTable

  // override def featureTable: Table = {
  //   val head = THead.data("A", "B") // TODO
  //   val data = Vector(
  //     IRecord.data("A" -> "a", "B" -> "b")
  //   )
  //   val body = TBody.create(head, data)
  //   val foot = None
  //   val caption = Caption("featureTable") // TODO
  //   val label = None
  //   Table(Some(head), body, foot, Some(caption), label)
  // }

  def entityTable(category: SDCategory): Table = {
    val head = THead.data("名前", "説明") // TODO
    val data = entities.filter(_.isMatch(category)).map(_entity_to_record)
    val body = TBody.create(head, data)
    val foot = None
    val side = None
    val cg = None
    val caption = Caption("entityTable in SDPackage") // TODO
    val label = None
    Table(Some(head), body, foot, side, cg, Some(caption), label)
  }

  def getEntityTable(category: SDCategory): Option[Table] =
    entityTable(category).toOption

  private def _entity_to_record(p: SDEntity): IRecord = {
    IRecord.data("名前" -> p.name, "説明" -> p.brief) // TODO
  }

//  override def specification: Dox = Dox.text("specificaion") // TODO

//  def summaries: Seq[SDSummary] = ???

  override def toString() = {
    val cs = children.map(_.toString).mkString(",")
    s"SDPackage[${description.name}]($cs)"
  }
}

object SDPackage {
  val empty = new SDPackage(Description.empty, Dox.empty, SDFeatureSet.empty)

  class Builder(name: String) {
    private var children: Vector[SDPackage.Builder] = Vector.empty
    private var description: Description = Description.name(name)
    private var categories: Vector[SDCategory] = Vector.empty
    private var featureSet: SDFeatureSet = SDFeatureSet.empty
//    var figures: Vector[SDFigure] = Vector.empty
    private var contents: Vector[Dox] = Vector.empty
    private var entities: Vector[SDEntity] = Vector.empty

    def apply(): SDPackage = {
      val r = SDPackage(description, Dox.empty, featureSet, categories.toList)
      println(s"SDPackage#Builder#apply[$name] $description")
      println(s"SDPackage#Builder#apply[$name] children $children")
      println(s"SDPackage#Builder#apply[$name] entities $entities")
      // XXX contents
      for (x <- entities)
        r.addChild(x)
      for (x <- children)
        r.addChild(x.apply())
      println(s"SDPackage#Builder#apply[$name] result $r")
      r
    }

    def addUpPackage(name: String): SDPackage.Builder = {
      val r = new Builder(name)
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

    def addFigure(binary: ChunkBag, name: String, title: String): SDPackage.Builder = {
      val (n, s) = StringUtils.pathnameBodySuffix(name)
      val mime = s.flatMap(MimeType.getBySuffix) getOrElse MimeType.application_octet_stream
      addFigure(binary, name, mime, title)
    }

    def addFigure(binary: ChunkBag, name: String, mime: MimeType, title: String): SDPackage.Builder = {
      contents = contents :+ Figure(BinaryImg(name, mime, binary), title)
      this
    }

    def addEntity(p: SDEntity): SDPackage.Builder = {
      entities = entities :+ p
      this
    }
  }

  def apply(name: String): SDPackage = SDPackage(Description.name(name), Dox.empty, SDFeatureSet.empty)
}
