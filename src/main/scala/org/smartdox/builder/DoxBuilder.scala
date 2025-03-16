package org.smartdox.builder

import scala.collection.mutable
import org.goldenport.RAISE
// import org.goldenport.i18n.I18NString
// import org.goldenport.cli.{Config => CliConfig, Environment}
// import org.goldenport.value._
import org.goldenport.i18n.I18NString
import org.goldenport.realm.Realm
import org.goldenport.tree.{Tree, TreeCursor}
import org.goldenport.tree.{TreeNode, Printer, TreeVisitor}
import org.smartdox._
import org.smartdox.generator.{Context => GContext}

// See org.goldenport.entities.smartdoc.builder.SmartDocBuilder.
/*
 * Derived from SmartDocMakerModel.java since Sep. 27, 2005.
 * Derived from SmartDocBuilder: Sep.  6, 2008 - Oct. 20, 2008
 * 
 * @since   Jul.  5, 2020
 *  version Jul. 26, 2020
 *  version Oct. 18, 2020
 *  version Nov. 24, 2020
 *  version Dec. 20, 2020
 * @version Feb. 23, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxBuilder(val context: DoxBuilder.Context) {
  import DoxBuilder._

  private val _tree = Tree.create[Dox]()

  def createCursor(): Cursor = new Cursor(this, _tree.cursor)

  def build(): Dox = {
    println(s"DoxBuilder#build: ${show}")
    // println(s"${_tree.root.content}")
    // println(s"${_tree.root.children}")
    // println(s"""${_tree.root.children}""")
    // println(s"""${_tree.root.children.map(x => x.content).mkString(",")}""")
    _tree.root.children.map(_build) match {
      case Nil => EmptyDox
      case x :: Nil => x
      case xs => Fragment(xs)
    }
  }

  private def _build(p: TreeNode[Dox]): Dox = {
    import scalaz.{Success, Failure}

    val node = p.content
    val cs = p.children.map(_build)
    node.copyV(cs.toList) match {
      case Success(r) => r
      case Failure(msgs) => RAISE.noReachDefect(msgs.list.mkString(";"))
    }
  }

  def buildSection(): Dox = ???

  def add(p: String): DoxBuilder = ???

  def show: String = {
    val printer = new Printer[Dox](_show)
    _tree.traverse(printer)
    printer.print
//    _tree.root.children.map(_show).mkString(",")
  }

  private def _show(p: TreeNode[Dox]): String = {
    s"""${p.content}\n${p.children.map(_show).mkString(",")}"""
  }

  private def _show(p: Dox): String = p.toString
}

object DoxBuilder {
  class Context(
    val context: GContext,
    val parameter: Parameter
  ) {
    def css = parameter.css
    def csslink = parameter.csslink
  }
  object Context {
    def create(): Context = new Context(GContext.create(), Parameter.empty)
    def create(args: Array[String]): Context = RAISE.notImplementedYetDefect
    def create(c: GContext): Context = new Context(c, Parameter.empty)
    def create(c: GContext, p: Parameter): Context = new Context(c, p)
  }

  case class Parameter(
    css: Option[String] = None,
    csslink: Option[String] = None
  )
  object Parameter {
    val empty = Parameter()
  }

  class Cursor(val builder: DoxBuilder, c: TreeCursor[Dox]) {
    def context = builder.context
//    val _nodes = mutable.ArrayBuffer[Dox]()

    // def enterTopic(title: Dox, subtitle: String): Unit = ???
    // def enterTopic(title: String, subtitle: String): Unit = enterTopic(Dox.text(title), subtitle)
    // def leaveTopic(): Unit = ???
    def enterPage(title: Inline, subtitle: String): Unit = {
      val h = Head(
        List(title),
        css = context.css,
        csslink = context.csslink
      )
      val b = Body.empty
      val d = Document(h, b)
      c.enterContent(d)
    }

    def enterPage(title: String, subtitle: String): Unit = enterPage(Dox.text(title), subtitle)

    def leavePage(): Unit = {
      c.leave()
    }

    def enterDivision(title: Inline, subtitle: String): Unit = {
      val t = List(title) // subtitle
      val s = Section(t, Nil)
      c.enterContent(s)
    }

    def enterDivision(title: String, subtitle: String): Unit =
      enterDivision(Dox.text(title), subtitle)

    def enterDivision(title: Inline): Unit = {
      val s = Section(List(title), Nil)
      c.enterContent(s)
    }

    def enterDivision(title: String): Unit = enterDivision(Dox.text(title))

    def leaveDivision(): Unit = {
      c.leave()
    }

    def addContent(p: String): Unit = addContent(Dox.text(p))

    def addContent(p: Dox): Unit = {
      c.add(p)
      // _nodes :+ p
      // c.add(Dox.toDox(_nodes))
    }

    def addTable(caption: String, p: Table): Unit =
      addContent(p.withCaption(caption))

    // def addDescription(p: Dox): Unit = ???
    // def setDescription(p: Description): Unit = ???
    // def setResume(p: Resume): Unit = ???
  }

  // class DoxVisitor() extends TreeVisitor[Dox] {
  //   def result: Dox = ???

  //   override def enter(node: TreeNode[Dox]): Unit = {
  //   }

  //   override def leave(node: TreeNode[Dox]): Unit = {
  //   }
  // }

  def create(): DoxBuilder = new DoxBuilder(Context.create())
  def create(args: Array[String]): DoxBuilder = new DoxBuilder(Context.create(args))
  def create(c: GContext): DoxBuilder = new DoxBuilder(Context.create(c))
  def create(c: GContext, p: Parameter): DoxBuilder = new DoxBuilder(Context.create(c, p))
}
