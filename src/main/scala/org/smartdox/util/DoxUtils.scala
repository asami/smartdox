package org.smartdox.util

import org.goldenport.util.StringUtils

/*
 * @since   Sep.  4, 2024
 *  version Sep.  5, 2024
 * @version Jul. 27, 2025
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
    else if (StringUtils.isSeparationLanguageChar(a.last) && StringUtils.isSeparationLanguageChar(b.head))
      a + b
    else
      a + ' ' + b
  }

  def trimSingleLine(p: String): String = {
    val a = p.trim
    case class Z(
      xs: Vector[Char] = Vector.empty
    ) {
      def r = xs.mkString

      def +(c: Char): Z = c match {
        case ' ' => _add_if_available(c)
        case '\n' => _add_if_available(c)
        case '\r' => _add_if_available(c)
        case m =>
          if (xs.last == ' ') {
            if (StringUtils.isSeparationLanguageChar(xs(xs.length - 2), c))
              _add(c)
            else
              copy(xs = xs.updated(xs.length - 1, c))
          } else {
            _add(c)
          }
      }

      private def _add(c: Char) = copy(xs = xs :+ c)

      private def _add_if_available(c: Char) = xs.last match {
        case ' ' => this
        case _ => _add(c)
      }
    }
    a.headOption match {
      case Some(s: Char) => a.tail.foldLeft(Z(Vector(s)))(_+_).r
      case None => a
    }
  }
}
