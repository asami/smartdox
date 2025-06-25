package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatest.Matchers
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Oct. 14, 2018
 *  version Nov. 11, 2018
 *  version Dec. 24, 2018
 *  version May. 31, 2024
 *  version Sep.  3, 2024
 *  version Oct. 26, 2024
 *  version Nov. 23, 2024
 *  version Jan.  1, 2025
 * @version Jun. 20, 2025
 * @author  ASAMI, Tomoharu
 */
trait UseDox2Parser extends Matchers with ScalazMatchers {
  protected def parse_orgmode_simple(in: String, out: String): Dox = {
    parse_orgmode(
      in,
      s"""<!DOCTYPE html><html><head/><body>$out</body></html>"""
    )
  }

  protected def parse_orgmode_simple_debug(in: String, out: String): Dox = {
    parse_orgmode_debug(
      in,
      s"""<!DOCTYPE html><html><head/><body>$out</body></html>"""
    )
  }

  protected def parse_orgmode(in: String, out: String): Dox = {
    val result = Dox2Parser.parse(in)
    // result should be ('successful)
    val dox = result
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_debug(in: String, out: String): Dox = {
    val result = Dox2Parser.parse(in)
    // result should be ('successful)
    val dox = result
    try {
      dox.toString() should be (out)
      dox
    } catch {
      case e: Throwable =>
        println(s"  result: ${dox.toString}")
        println(s"expected: $out")
        throw e
    }
  }

  protected def parse_orgmode_full(in: String, out: String): Dox = {
    val c = Dox2Parser.Config.orgmodeInline
    val result = Dox2Parser.parse(c, in)
    val dox = result
    dox.toString() should be (out)
    dox
  }

  protected def parse_orgmode_auto_title(in: String, out: String): Dox = {
    ???
  }

  protected final def parse_dox(in: String): Dox =
    parse_dox(Dox2Parser.Config.smartdox, in)

  protected final def parse_dox(c: Dox2Parser.Config, in: String): Dox =
    Dox2Parser.parse(c, in)

  protected final def parse_dox(c: Dox2Parser.Config, in: String, out: String): Dox = {
    val result = Dox2Parser.parse(c, in)
    val dox = result
    dox.toString() should be (out)
    dox
  }

  protected final def parse_dox_simple(c: Dox2Parser.Config, in: String, out: String): Dox = {
    val s = """<!DOCTYPE html><html><head/><body>%s</body></html>""".format(out)
    parse_dox(c, in, s)
  }

  protected final def parse_markdown(in: String): Dox =
    parse_dox(Dox2Parser.Config.markdown, in)

  protected final def parse_markdown(in: String, out: String): Dox =
    parse_dox(Dox2Parser.Config.markdown, in, out)

  protected final def parse_markdown_simple(in: String, out: String): Dox =
    parse_dox_simple(Dox2Parser.Config.markdown, in, out)

  protected final def parse_model(in: String): Dox =
    parse_dox(Dox2Parser.Config.literateModel, in)

  protected final def parse_model(in: String, out: String): Dox =
    parse_dox(Dox2Parser.Config.literateModel, in, out)
}
