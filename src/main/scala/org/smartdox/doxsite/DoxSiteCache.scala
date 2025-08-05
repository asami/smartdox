package org.smartdox.doxsite

import java.io.File
import java.time.Instant
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.io.InputSource
import org.goldenport.io.IoUtils
import org.smartdox.Dox
import org.smartdox.Document
import org.smartdox.generator.Context
import org.smartdox.doxsite.DoxSite.Strategy
import org.smartdox.parser.PureParser
import org.smartdox.converters.Dox2XmlConverter

/*
 * @since   Jul. 23, 2025
 *  version Jul. 27, 2025
 * @version Aug.  6, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSiteCache(config: Option[DoxSite.Config], context: Context) {
  import DoxSiteCache._

  private val _is_test_compare = true
  private val _is_test_only = false
  private val _context_name =
    config.map(_.strategy).getOrElse(Strategy.Overview).name

  private val _base = new File(s"doxsite-cache-${_context_name}.d")

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
    val converter = new Dox2XmlConverter(context)
    for {
      s <- converter.convert(dox)
    } yield {
      val file = new File(_base, pathname)
      IoUtils.save(file, s)
      if (_is_test_compare)
        _compare(pathname, dox) match {
          case CompareResult.Success => Unit
          case CompareResult.Mismatch => 
            context.log.error(s"Mismatch cache: $pathname")
            _move_error(file)
          case CompareResult.NotFound =>  
            RAISE.noReachDefect(s"Cache not found: $pathname")
        }
    }
  }

  private def _compare(pathname: String, dox: Document): CompareResult = {
    _get(pathname) match {
      case Some(s) =>
        if (!Dox.compareWithoutDoxCacheControl(dox, s))
          CompareResult.Mismatch
        else
          CompareResult.Success
      case None => CompareResult.NotFound
    }
  }

  private def _move_error(p: File): Unit =
    IoUtils.moveFileWithErrorSuffix(p)
}

object DoxSiteCache {
  sealed trait CompareResult
  object CompareResult {
    case object Success extends CompareResult
    case object Mismatch extends CompareResult
    case object NotFound extends CompareResult
  }
}
