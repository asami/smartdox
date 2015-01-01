package org.smartdox.util

import scalaz._, Scalaz._, Tree._
import org.goldenport.Z
import org.smartdox._

/*
 * @since   Sep.  9, 2014
 * @version Sep.  9, 2014
 * @author  ASAMI, Tomoharu
 */
case class TechDocTransformer() {
  def transform(doc: Document): Document = {
    val head = doc.head
    val title = doc.head.title
    val body = _build_doc(doc.body, title)
    doc.copy(body = body)
  }

  private def _build_doc(body: Body, title: InlineContents) = {
    val t = Html5("h1", List("class" -> "title"), title)
    val c = _build_contents(body.contents)
    body.copy(contents = t :: c)
  }

  private def _build_contents(xs: List[Dox]) = {
    val f = Fragment(xs)
    val r = _build_content(f)
    r.contents
  }

  private def _build_content(dox: Dox): List[Dox] = {
    val tree = Dox.tree(dox)
    val r = _transform(tree)
    Dox.untree(r) match {
      case f: Fragment => f.contents
      case x => sys.error("???")
    }
  }

  private def _transform(tree: Tree[Dox]): Tree[Dox] = {
    Z.replace(tree) {
      case (d: Underline, _) => (Text(s"_${d.toText}_"), Stream.empty)
      case (d: Italic, _) => (Text(s"/${d.toText}/"), Stream.empty)
    }
  }

  // private def _transform0(tree: Tree[Dox]): Tree[Dox] = {
  //   _transform(tree.loc).toTree
  // }

  // private def _transform(loc: TreeLoc[Dox]): TreeLoc[Dox] = {
  //   loc.getLabel match {
  //     case d: Underline => loc.setTree(leaf(Text(s"_${d.toText}_")))
  //     case _ => _transform_children(loc)
  //   }
  // }

  // private def _transform_children(loc: TreeLoc[Dox]): TreeLoc[Dox] = {
  //   loc.firstChild match {
  //     case Some(s) => _transform_child(s)
  //     case None => loc
  //   }
  // }

  // @annotation.tailrec
  // private def _transform_child(loc: TreeLoc[Dox]): TreeLoc[Dox] = {
  //   val a = _transform(loc)
  //   a.right match {
  //     case Some(s) => _transform_child(s)
  //     case None => a
  //   }
  // }

  // private def _build_content(x: Dox): List[Dox] = {
  //   x match {
  //     case d: Section => _build_section(d)
  //     case d: Div => _build_div(d)
  //     case d: Paragraph => _build_paragraph(d)
  //     case d: Text => _build_text(d)
  //     case d: Bold => _build_bold(d)
  //     case d: Italic => _build_italic(d)
  //     case d: Underline => _build_underline(d)
  //     case d: Code => _build_code(d)
  //     case d: Pre => _build_pre(d)
  //     case d: Ul => _build_ul(d)
  //     case d: Ol => _build_ol(d)
  //     case d: Li => _build_li(d)
  //     case d: Del => _build_del(d)
  //     case d: Hyperlink => _build_hyperlink(d)
  //     case d: ReferenceImg => _build_referenceimg(d)
  //     case d: Table => _build_table(d)
  //     case d: Space => _build_space(d)
  //     case d: Dl => _build_dl(d)
  //     case d: Dt => _build_dt(d)
  //     case d: Dd => _build_dd(d)
  //     case d: Fragment => _build_fragment(d)
  //     case d: Figure => _build_figure(d)
  //     case d: DotImg => _build_dotimg(d)
  //     case d: DitaaImg => _build_ditaaimg(d)
  //   }
  // }

  // private def _build_section(d: Section): List[Dox] = {

  // }

  // private def _build_div(d: Div): List[Dox] = {

  // }

  // private def _build_paragraph(d: Paragraph): List[Dox] = {

  // }

  // private def _build_text(d: Text): List[Dox] = {

  // }

  // private def _build_bold(d: Bold): List[Dox] = {

  // }

  // private def _build_italic(d: Italic): List[Dox] = {

  // }

  // private def _build_underline(d: Underline): List[Dox] = {

  // }

  // private def _build_code(d: Code): List[Dox] = {

  // }

  // private def _build_pre(d: Pre): List[Dox] = {

  // }

  // private def _build_ul(d: Ul): List[Dox] = {

  // }

  // private def _build_ol(d: Ol): List[Dox] = {

  // }

  // private def _build_li(d: Li): List[Dox] = {

  // }

  // private def _build_del(d: Del): List[Dox] = {

  // }

  // private def _build_hyperlink(d: Hyperlink): List[Dox] = {

  // }

  // private def _build_referenceimg(d: Referenceimg): List[Dox] = {

  // }

  // private def _build_table(d: Table): List[Dox] = {

  // }

  // private def _build_space(d: Space): List[Dox] = {

  // }

  // private def _build_dl(d: Dl): List[Dox] = {

  // }

  // private def _build_dt(d: Dt): List[Dox] = {

  // }

  // private def _build_dd(d: Dd): List[Dox] = {

  // }

  // private def _build_fragment(d: Fragment): List[Dox] = {

  // }

  // private def _build_figure(d: Figure): List[Dox] = {

  // }

  // private def _build_dotimg(d: DotImg): List[Dox] = {

  // }

  // private def _build_ditaaimg(d: DitaaImg): List[Dox] = {

  // }
}
