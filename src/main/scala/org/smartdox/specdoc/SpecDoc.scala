package org.smartdox.specdoc

import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.values.Designation
import org.goldenport.bag.ChunkBag
import org.goldenport.io.MimeType
import org.smartdox._

/*
 * Derived from SpecDocModel since Feb. 17, 2007
 *
 * @since   Sep.  4, 2008
 *  version Apr. 17, 2011
 *  version Feb. 22, 2012
 *  version Jun. 13, 2020
 *  version Jul. 24, 2020
 *  version Sep. 21, 2020
 *  version Oct. 11, 2020
 *  version Dec.  7, 2020
 *  version Jul. 12, 2021
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class SpecDoc(title: Inline, root: SDPackage) extends Realm.StringApplicationData {
  def print = s"SpecDoc($title)"

  def packages: Seq[SDPackage] = root.children.collect {
    case m: SDPackage => m
  }

  def marshall: String = RAISE.notImplementedYetDefect
}

object SpecDoc {
  class Builder(
//    root: SDPackage,
    current: SDPackage.Builder
  ) {
    def apply(): SpecDoc = {
      val a = current.apply()
      println(s"SpecDoc#Builder#apply: $a")
      val title = Dox.text("Specification") // TODO
      SpecDoc(title, a)
    }

    def addUpPackage(name: String): Builder = {
      println(s"SpecDoc#addUpPackage $name")
      new Builder(/* root, */ current.addUpPackage(name))
    }

    def setDescription(p: Description): Builder = {
      current.setDescription(p)
      this
    }

    def setFeatureSet(p: SDFeatureSet): Builder = {
      current.setFeatureSet(p)
      this
    }

    def addCategories(ps: Seq[SDCategory]): Builder = {
      current.addCategories(ps)
      this
    }

    def addEntity(p: SDEntity): Builder = {
      current.addEntity(p)
      this
    }

    def addFigure(binary: ChunkBag, name: String, title: String): Builder = {
      current.addFigure(binary, name, title)
      this
    }

    def addFigure(binary: ChunkBag, name: String, mime: MimeType, title: String): Builder = {
      current.addFigure(binary, name, mime, title)
      this
    }
  }
  object Builder {
    def create(title: String): Builder = {
      new Builder(new SDPackage.Builder(title))
    }
  }
}
