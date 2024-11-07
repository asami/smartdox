package org.smartdox.util

import org.goldenport.util.StringUtils

/*
 * @since   Sep.  4, 2024
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
object DoxUtils {
  def concatLines(ps: Seq[String]): String = {
    ps.toList match {
      case Nil => ""
      case x :: xs => concatLines(x, xs)
    }
  }

  def concatLines(p: String, ps: Seq[String]): String = {
    @annotation.tailrec
    def _go_(x: String, xs: List[String]): String = xs match {
      case Nil => x
      case y :: ys => _go_(concatLines(x, y), ys)
    }

    _go_(p, ps.toList)
  }

  def concatLines(p: String, q: String): String = {
    val a = p.trim
    val b = q.trim
    if (a.isEmpty)
      b
    else if (b.isEmpty)
      a
    else if (StringUtils.isSeperationLanguageChar(a.last) && StringUtils.isSeperationLanguageChar(b.head))
      a + b
    else
      a + ' ' + b
  }
}
