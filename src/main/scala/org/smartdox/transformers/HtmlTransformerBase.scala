package org.smartdox.transformers

import org.w3c.dom.{Node}
import org.goldenport.xml.dom.DomUtils

/*
 * @since   Feb.  2, 2021
 * @version Feb.  2, 2021
 * @author  ASAMI, Tomoharu
 */
trait HtmlTransformerBase {
  def isPretty: Boolean
  def isDocument: Boolean

  protected final def to_html(dom: Node): String =
    (isPretty, isDocument) match {
      case (true, true) => DomUtils.toHtmlPrettyText(dom)
      case (true, false) => DomUtils.toHtmlFragmentText(dom) // XXX
      case (false, true) => DomUtils.toHtmlText(dom)
      case (false, false) => DomUtils.toText(dom) // XXX
    }

}
