package org.smartdox.transformer

import scalaz._
import Scalaz._
import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.tree._
import org.smartdox._
import Dox._

/*
 * @since   Jan. 12, 2012
 * @version Apr. 26, 2025
 * @author  ASAMI, Tomoharu
 */
trait Dox2StringTransformer extends DoxTreeVisitor {
  private val _newline = "\n"
  private val _buffer = new StringBuilder()
  private var _indent_depth: Int = 0
  private def _indent_size: Int = 2

  private def _indent_length = _indent_depth * _indent_size
  private def _indent: String = " " * _indent_length

  def transform(dox: Dox): Consequence[String] = Consequence {
    val tree = Dox.toTree(dox)
    tree.traverse(this)
    _buffer.toString()
  }

  protected final def print_text(p: String): Unit = {
    _buffer.append(p)
  }

  protected final def print_line(): Unit = {
    _buffer.append(_newline)
  }

  protected final def print_line(p: String): Unit = {
    _buffer.append(_indent)
    _buffer.append(p)
    _buffer.append(_newline)
  }

  protected final def print_line_start(p: String): Unit = {
    _buffer.append(_indent)
    _buffer.append(p)
  }

  protected def to_text(ps: Seq[Dox]): String = Dox.toText(ps)
}
