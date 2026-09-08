package org.smartdox.service.operations

import java.util.Locale
import org.goldenport.cli.Request
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NContext
import org.goldenport.tree.TreeTransformer
import org.smartdox.{Dox, Hyperlink}
import org.smartdox.diagnostics.{StructuredRenderingDiagnostic, StructuredRenderingDiagnosticException}
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.transformers.LanguageFilterTransformer
import org.smartdox.doxsite.SitePublicationContext

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[operations] object PdfOperationSiteProjection {
  def selectLocale(dox: Dox, selector: Option[String]): Consequence[Dox] =
    selector match {
      case None => Consequence.success(dox)
      case Some(value) =>
        _locale(value).flatMap { locale =>
          if (LanguageFilterTransformer._has_exact_locale(dox, locale)) {
            val context = TreeTransformer.Context.default[Dox].
              withI18NContext(I18NContext.default.withLocale(locale))
            Consequence.execute {
              Dox.transform(dox, LanguageFilterTransformer._strict(context))
            }
          } else {
            Consequence.resourceNotFound[Dox](s"pdf.locale.unavailable: $value")
          }
        }
    }

  def localeSelector(req: Request): Consequence[Option[String]] =
    req.getPropertyString("locale") match {
      case None => Consequence.success(None)
      case Some(value) => _locale(value).map(_ => Some(value))
    }

  def localeSelectorOrThrow(req: Request): Option[String] = {
    val result = localeSelector(req)
    if (result.isError) {
      val tokencontext = req.getPropertyString("locale").getOrElse("")
      throw new StructuredRenderingDiagnosticException(
        _locale_failure_diagnostic(tokencontext, None, result.message)
      )
    }
    result.take
  }

  def selectLocaleOrThrow(
    dox: Dox,
    selector: Option[String],
    sourceIdentity: String
  ): Dox = {
    val result = selectLocale(dox, selector)
    if (result.isError) {
      val tokencontext = selector.getOrElse("")
      throw new StructuredRenderingDiagnosticException(
        _locale_failure_diagnostic(tokencontext, Some(sourceIdentity), result.message)
      )
    }
    result.take
  }

  def resolveSiteLinks(
    context: GeneratorContext,
    cmd: PdfOperationClass.PdfCommand,
    dox: Dox,
    selector: Option[String]
  ): Consequence[Dox] =
    if (_site_links(dox).isEmpty)
      Consequence.success(dox)
    else {
      val publication = (cmd.siteRoot, cmd.siteConfig) match {
        case (Some(root), Some(config)) =>
          SitePublicationContext.create(root, config, cmd.in, context)
        case _ =>
          Consequence.invalidArgumentFault[SitePublicationContext](
            "pdf.site-context.missing: --site-root and --site-config are required for site:[...] links"
          )
      }
      publication.flatMap { sitecontext =>
        val locale = selector match {
          case Some(value) => SitePublicationContext.locale(value)
          case None => sitecontext.defaultLocale
        }
        locale.flatMap { selectedlocale =>
          sitecontext.resolveDox(dox, selectedlocale)
        }
      }
    }

  private def _locale(value: String): Consequence[Locale] = {
    val locale = Locale.forLanguageTag(value)
    if (value.isEmpty || value.trim != value || locale.toLanguageTag != value)
      Consequence.invalidArgumentFault(s"pdf.locale.invalid: $value")
    else if (value == "ja" || value == "en")
      Consequence.success(locale)
    else
      Consequence.unsupportedOperation(s"pdf.locale.unsupported: $value")
  }

  private def _locale_failure_diagnostic(
    tokencontext: String,
    sourceidentity: Option[String],
    message: String
  ): StructuredRenderingDiagnostic =
    if (message.startsWith("pdf.locale.invalid:"))
      StructuredRenderingDiagnostic.pdfLocaleInvalid(tokencontext, sourceidentity)
    else if (message.startsWith("pdf.locale.unsupported:"))
      StructuredRenderingDiagnostic.pdfLocaleUnsupported(tokencontext, sourceidentity)
    else if (message.startsWith("pdf.locale.unavailable:"))
      StructuredRenderingDiagnostic.pdfLocaleUnavailable(tokencontext, sourceidentity)
    else
      throw new IllegalStateException(s"Unexpected PDF locale-selection failure: $message")

  private def _site_links(dox: Dox): Vector[Hyperlink] = {
    val here = dox match {
      case link: Hyperlink if link.isSite => Vector(link)
      case _ => Vector.empty
    }
    here ++ dox.elements.toVector.flatMap(_site_links)
  }
}
