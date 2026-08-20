package org.smartdox.transformers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.{Node}
import org.goldenport.xml.dom.DomUtils

/*
 * @since   Feb.  2, 2021
 * @version Aug. 21, 2026
 * @author  ASAMI, Tomoharu
 */
trait HtmlTransformerBase {
  def isPretty: Boolean
  def isDocument: Boolean

  protected final def to_html(dom: Node): String =
    (isPretty, isDocument) match {
      case (true, true) => _html_text(dom, ispretty = true)
      case (true, false) => DomUtils.toHtmlFragmentText(dom) // XXX
      case (false, true) => _html_text(dom, ispretty = false)
      case (false, false) => DomUtils.toText(dom) // XXX
    }

  private def _html_text(dom: Node, ispretty: Boolean): String = {
    val output = new ByteArrayOutputStream()
    val transformer = TransformerFactory.newInstance().newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.setOutputProperty(OutputKeys.METHOD, "xml")
    transformer.setOutputProperty(OutputKeys.INDENT, if (ispretty) "yes" else "no")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    if (ispretty)
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
    transformer.transform(new DOMSource(dom), new StreamResult(output))
    new String(output.toByteArray, StandardCharsets.UTF_8)
  }

}
