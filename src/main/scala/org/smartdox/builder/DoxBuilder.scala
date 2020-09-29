package org.smartdox.builder

// import org.goldenport.RAISE
// import org.goldenport.i18n.I18NString
// import org.goldenport.cli.{Config => CliConfig, Environment}
// import org.goldenport.value._
import org.goldenport.i18n.I18NString
import org.goldenport.realm.Realm
import org.smartdox._
import org.smartdox.generator.Context


// See org.goldenport.entities.smartdoc.builder.SmartDocBuilder.
/*
 * Derived from SmartDocMakerModel.java since Sep. 27, 2005.
 * Derived from SmartDocBuilder: Sep.  6, 2008 - Oct. 20, 2008
 * 
 * @since   Jul.  5, 2020
 * @version Jul. 26, 2020
 * @author  ASAMI, Tomoharu
 */
class DoxBuilder(val context: Context) {
  import DoxBuilder._

  def createCursor: Cursor = ???

  def build(): Dox = ???

  def buildSection(): Dox = ???

  def add(p: String): DoxBuilder = ???
}

object DoxBuilder {
  class Cursor() {
    def enterTopic(title: Dox, subtitle: String): Unit = ???
    def leaveTopic(): Unit = ???
    def enterPage(title: Dox, usbtitle: String): Unit = ???
    def leavePage(): Unit = ???
    def enterDivision(title: Dox): Unit = ???
    def leaveDivision(): Unit = ???

    def addDescription(p: Dox): Unit = ???
    def addTable(caption: String, p: Table): Unit = ???
  }
}
