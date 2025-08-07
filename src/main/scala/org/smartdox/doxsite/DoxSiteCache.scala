package org.smartdox.doxsite

import java.io.File
import java.time.Instant
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.io.InputSource
import org.goldenport.io.IoUtils
import org.goldenport.util.StringUtils
import org.smartdox.Dox
import org.smartdox.Document
import org.smartdox.generator.Context
import org.smartdox.doxsite.DoxSite.Strategy
import org.smartdox.parser.PureParser
import org.smartdox.converters.Dox2XmlConverter

/*
 * @since   Jul. 23, 2025
 *  version Jul. 27, 2025
 * @version Aug.  7, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSiteCache(config: Option[DoxSite.Config], context: Context) {
  import DoxSiteCache._

  private val _is_test_compare = true
  private val _is_test_only = false
  private val _context_name =
    config.map(_.strategy).getOrElse(Strategy.Overview).name

  private val _base = new File(s"doxsite-cache-${_context_name}.d")

  private def _to_xml(dox: Dox): Consequence[String] = {
    val converter = new Dox2XmlConverter(context)
    converter.convert(dox)
  }

  def get(
    pathname: String,
    lastmodified: Option[Instant]
  ): Option[Dox] = Consequence {
    val file = new File(_base, pathname)
    if (file.exists)
      lastmodified match {
        case Some(s) =>
          if (s.toEpochMilli < file.lastModified)
            _get(file, s)
          else
            None
        case None => None
      }
    else
      None
  }.toOption.flatten

  private def _get(file: File, lastmodified: Instant): Option[Dox] =
    _parse(InputSource(file)).flatMap(x =>
      Dox.getHead(x).flatMap(h =>
        if (h.doxCacheControl.fold(false)(_.isAvailable(lastmodified)))
          if (_is_test_only)
            None
          else
            Some(x)
        else
          None
      )
    )

  private def _get(pathname: String): Option[Dox] = Consequence {
    _parse(InputSource(new File(_base, pathname)))
  }.unsafeOnError(e =>
    context.log.error(s"Can't load cache: $pathname")
  ).toOption.flatten

  private def _parse(in: InputSource): Option[Dox] =
    PureParser.parseC(in).toOption

  def set(pathname: String, dox: Dox): Unit = dox match {
    case m: Document => set(pathname, m)
    case _ => Unit
  }

  def set(pathname: String, dox: Document): Unit = {
    for {
      s <- _to_xml(dox)
    } yield {
      val file = new File(_base, pathname)
      IoUtils.save(file, s)
      if (_is_test_compare)
        _compare(pathname, dox, s) match {
          case CompareResult.Success => Unit
          case CompareResult.Mismatch(diff) => 
            context.log.error(s"Mismatch cache: $pathname")
            _move_error(file)
            _make_error_file(file, diff)
          case CompareResult.NotFound =>  
            RAISE.noReachDefect(s"Cache not found: $pathname")
        }
    }
  }

  private def _load(pathname: String): Option[String] = Consequence {
    InputSource(new File(_base, pathname)).asText
  }.toOption

  private def _compare(pathname: String, dox: Document, expected: String): CompareResult =
    _load(pathname) match {
      case Some(actual) =>
        StringUtils.compareAt(expected, actual) match {
          case Some(msg) => CompareResult.Mismatch(msg)
          case None => _validate(actual)
        }
      case None => CompareResult.NotFound
    }

  private def _validate(expected: String): CompareResult = {
    PureParser.parseC(InputSource.string(expected)) match {
      case Consequence.Success(dox, _) => _to_xml(dox) match {
        case Consequence.Success(actual, _) =>
          StringUtils.compareAt(expected, actual) match {
            case Some(msg) => CompareResult.Mismatch(msg)
            case None => CompareResult.Success
          }
        case Consequence.Error(cc) => CompareResult.Mismatch(cc.message)
      }
      case Consequence.Error(c) => CompareResult.Mismatch(c.message)
    }
  }

  private def _compare0(pathname: String, dox: Document, expected: String): CompareResult = {
    _get(pathname) match {
      case Some(s) =>
        if (!Dox.compareWithoutDoxCacheControl(dox, s)) {
          val a = for (actual <- _to_xml(s)) yield {
            StringUtils.compareAt(expected, actual) getOrElse "Same"
          }
          val diff = a.fold(e => e.message, identity)
          CompareResult.Mismatch(diff)
        } else {
          CompareResult.Success
        }
      case None => CompareResult.NotFound
    }
  }

  private def _move_error(p: File): Unit =
    IoUtils.moveFileWithErrorSuffix(p)

  private def _make_error_file(p: File, s: String): Unit = {
    val f = IoUtils.addSuffix(p, "error_msg")
    IoUtils.save(f, s)
  }
}

object DoxSiteCache {
  sealed trait CompareResult
  object CompareResult {
    case object Success extends CompareResult
    case class Mismatch(diff: String) extends CompareResult
    case object NotFound extends CompareResult
  }
}
