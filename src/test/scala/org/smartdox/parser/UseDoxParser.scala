package org.smartdox.parser

import scalaz._
import Scalaz._
import org.scalatest.Matchers
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Jan. 27, 2012
 *  version Feb.  5, 2014
 *  version Sep.  9, 2014
 *  version Mar. 10, 2016
 * @version Jun. 17, 2025
 * @author  ASAMI, Tomoharu
 */
trait UseDoxParser extends Matchers with ScalazMatchers {
  private val _org_mode = Dox2Parser.Config.orgmode

  protected final def parse_orgmode_simple(in: String, out: String) {
    parse_orgmode(in, 
        """<!DOCTYPE html><html><head/><body>%s</body></html>""".format(out))
  }

  protected def parse_orgmode_full(in: String, out: String): Dox = {
    parse_orgmode(in, out)
  }

  protected def parse_orgmode_auto_title(in: String, out: String): Dox = {
    parse_orgmode(in, out)
  }

  protected final def parse_orgmode(in: String, out: String): Dox = {
    val dox = Dox2Parser.parse(_org_mode, in)
    dox.toString() should be (out)
    dox
  }

  protected final def parse_orgmode_z(in: String, out: String) {
    parse_orgmode(in, out)
  }

  protected def parse_document(in: String): Document = {
    val dox = Dox2Parser.parse(_org_mode, in)
    dox.asInstanceOf[Document]
  }

  protected def parse_document_full(in: String): Document = {
    parse_document(in)
  }

/*
  protected def parse_orgmode_simple(in: String, out: String) {
    parse_orgmode(in, 
        """<!DOCTYPE html><html><head/><body>%s</body></html>""".format(out))
  }

  protected def parse_document(in: String): Document = {
    val result = DoxParser.parseOrgmode(in)
    result should be ('successful)
    result.get.asInstanceOf[Document]
  }

  protected def parse_document_full(in: String): Document = {
    val result = new DoxParser(true).parseOrgmode(in)
    result should be ('successful)
    result.get.asInstanceOf[Document]
  }

  protected def parse_orgmode(in: String, out: String): Dox = {
    val result = DoxParser.parseOrgmode(in)
    result should be ('successful)
    val dox = result.get
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_full(in: String, out: String): Dox = {
    val result = new DoxParser(true).parseOrgmode(in)
    result should be ('successful)
    val dox = result.get
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_auto_title(in: String, out: String) {
    val result = DoxParser.parseOrgmodeAutoTitle(in)
    result should be ('successful)
    val dox = result.get
    dox.toString() should be (out)
  }

  protected def parse_orgmode_z(in: String, out: String) {
    val result = DoxParser.parseOrgmodeZ(in)
    result should success
    result match {
      case Success(dox) => dox.toString should be (out)
      case Failure(_) => ???
    }
  }
*/
}
