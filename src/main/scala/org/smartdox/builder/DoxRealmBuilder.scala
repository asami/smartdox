package org.smartdox.builder

import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.smartdox._
import org.smartdox.generator.Context
import org.smartdox.transformers.Dox2HtmlTransformer

/*
 * @since   Jul. 26, 2020
 *  version Aug. 13, 2020
 *  version Oct. 18, 2020
 *  version Nov. 14, 2020
 *  version Dec. 20, 2020
 *  version Jan. 12, 2021
 *  version Apr.  3, 2025
 * @version May. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxRealmBuilder(
  val context: Context,
  val htmlParameter: DoxBuilder.Parameter
) {
  import DoxRealmBuilder._

  private var _cursors: Vector[Cursor] = Vector.empty

  def createCursor: Cursor = {
    val r = new SinglePageCursor(this)
    _cursors = _cursors :+ r
    r
  }

  def build(): Realm = _cursors.foldLeft(Realm.create)((z, x) => z + x.build)
}

object DoxRealmBuilder {
  sealed trait Cursor {
    def enterTopic(title: Inline, subtitle: String): Unit
    def enterTopic(title: String, subtitle: String): Unit
    def leaveTopic(): Unit
    def enterPage(title: Inline, subtitle: String): Unit
    def enterPage(title: String, subtitle: String): Unit
    def leavePage(): Unit
    def enterDivision(title: Inline): Unit
    def enterDivision(title: String): Unit
    def leaveDivision(): Unit

    def setDescription(p: Description): Unit
    def setResume(p: Resume): Unit
    def addContent(p: Dox): Unit
    def addTable(caption: String, p: Table): Unit
    def addTable(caption: String, p: Option[Table]): Unit = p.foreach(addTable(caption, _))

    def build(): Realm
  }

  class SinglePageCursor(val builder: DoxRealmBuilder) extends Cursor {
    private val _dox_builder = DoxBuilder.create(builder.context, builder.htmlParameter)
    private val _dox_cursor = _dox_builder.createCursor

    def enterTopic(title: Inline, subtitle: String): Unit = _dox_cursor.enterPage(title, subtitle)
    def enterTopic(title: String, subtitle: String): Unit = _dox_cursor.enterPage(title, subtitle)
    def leaveTopic(): Unit = _dox_cursor.leavePage()
    def enterPage(title: Inline, subtitle: String): Unit = _dox_cursor.enterDivision(title, subtitle)
    def enterPage(title: String, subtitle: String): Unit = _dox_cursor.enterDivision(title, subtitle)
    def leavePage(): Unit = _dox_cursor.leaveDivision()
    def enterDivision(title: Inline): Unit = _dox_cursor.enterDivision(title) 
    def enterDivision(title: String): Unit = _dox_cursor.enterDivision(title)
    def leaveDivision(): Unit = _dox_cursor.leaveDivision()

    def setDescription(p: Description): Unit = ??? // _dox_cursor.setDescription(p)

    def setResume(p: Resume): Unit = {
      (p.captionOption, p.briefOption, p.summaryOption) match {
        case (Some(c), Some(b), Some(s)) =>
          if (false)
            _caption_brief_summary(c, b, s)
          else
            Unit
        case _ => Unit
      }
    }

    private def _caption_brief_summary(caption: Dox, brief: Dox, summary: Dox) = {
      RAISE.unsupportedOperationFault
    }

    def addContent(p: Dox): Unit = _dox_cursor.addContent(p)

    def addTable(caption: String, p: Table): Unit = _dox_cursor.addTable(caption, p)

    def build(): Realm = {
      val name = "model.org" // TODO
      val doc = _dox_builder.build()
      val b = Realm.Builder()
      b.setObject(name, doc)
      b.build
    }

    def build0(): Realm = {
      val name = "model.org" // TODO
      val doc = _dox_builder.build()
      val rule = Dox2HtmlTransformer.Rule.default
      val r = for {
        html <- Dox2HtmlTransformer(builder.context, rule).transform(doc)
      } yield {
        val b = Realm.Builder()
        b.set(name, html)
        b.build()
      }
      r.take
    }
  }

  class MultiPageCursor(val builder: DoxRealmBuilder) extends Cursor {
    private val _dox_builder = DoxBuilder.create(builder.context)
    private val _dox_cursor = _dox_builder.createCursor

    def enterTopic(title: Inline, subtitle: String): Unit = RAISE.notImplementedYetDefect
    def enterTopic(title: String, subtitle: String): Unit = RAISE.notImplementedYetDefect
    def leaveTopic(): Unit = RAISE.notImplementedYetDefect
    def enterPage(title: Inline, subtitle: String): Unit = _dox_cursor.enterPage(title, subtitle)
    def enterPage(title: String, subtitle: String): Unit = _dox_cursor.enterPage(title, subtitle)
    def leavePage(): Unit = _dox_cursor.leavePage()
    def enterDivision(title: Inline): Unit = _dox_cursor.enterDivision(title) 
    def enterDivision(title: String): Unit = _dox_cursor.enterDivision(title)
    def leaveDivision(): Unit = _dox_cursor.leaveDivision()

    def setDescription(p: Description): Unit = RAISE.notImplementedYetDefect // _dox_cursor.setDescription(p)
    def setResume(p: Resume): Unit = RAISE.notImplementedYetDefect // _dox_cursor.setResume(p)
    def addContent(p: Dox): Unit = _dox_cursor.addContent(p)
    def addTable(caption: String, p: Table): Unit = _dox_cursor.addTable(caption, p)

    def build(): Realm = {
      ???
    }
  }
}
