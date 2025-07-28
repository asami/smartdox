package org.smartdox.doxsite

import java.io.File
import java.time.Instant
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
 * @version Jul. 27, 2025
 * @author  ASAMI, Tomoharu
 */
class DoxSiteCache(config: Option[DoxSite.Config], context: Context) {
  private val _is_test_compare = true
  private val _is_test_only = true
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
    case m: Document => set(pathname, m.markCache)
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
        _compare(pathname, dox)
    }
  }

  private def _compare(pathname: String, dox: Document): Unit = {
    _get(pathname) match {
      case Some(s) =>
        if (!Dox.compareWithoutDoxCacheControl(dox, s))
          context.log.error(s"Mismatch cache: $pathname")
      case None => context.log.error(s"Cache not found: $pathname")
    }
  }
}

object DoxSiteCache {
}
