package org.smartdox.doxsite

import java.io.File
import java.net.URI
import java.nio.file.{Files, LinkOption, Path}
import java.util.Locale
import org.goldenport.context.Consequence
import org.goldenport.tree.{TreeNode, TreeTransformer}
import org.goldenport.util.StringUtils
import org.smartdox._
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.metadata.DocumentMetaData
import org.smartdox.transformer.DoxHomoTreeTransformer

/*
 * @since   Sep.  7, 2026
 * @version Sep.  7, 2026
 * @author  ASAMI, Tomoharu
 */
final case class SitePublicationContext private (
  siteRoot: Path,
  siteConfig: Path,
  sourceDocument: Path,
  sourcePath: String,
  private[doxsite] site: DoxSite,
  private[doxsite] baseUri: URI
) {
  import SitePublicationContext._

  def defaultLocale: Consequence[Locale] =
    locale(site.config.siteOutput.defaultLocale)

  def resolve(target: URI, locale: Locale): Consequence[ResolvedLink] =
    _target_path(sourcePath, target).flatMap(_verify_target).flatMap { _ =>
      resolveSiteLink(sourcePath, target, _site_document_metadata).flatMap { link =>
        link.localizedTitle(locale).flatMap { title =>
          publicUri(baseUri, site.config, locale, link.publicPath).map { uri =>
            ResolvedLink(uri, title, link)
          }
        }
      }
    }

  def resolveDox(dox: Dox, locale: Locale): Consequence[Dox] =
    Consequence.execute {
      Dox.transform(dox, new SiteLinkProjection(this, locale))
    }

  private def _verify_target(targetPath: String): Consequence[Unit] = {
    val candidate = siteRoot.resolve(targetPath).normalize
    if (!candidate.startsWith(siteRoot))
      Consequence.invalidArgumentFault(s"pdf.site-link.target.outside-root: $targetPath")
    else if (Files.isSymbolicLink(candidate))
      Consequence.invalidArgumentFault(s"pdf.site-link.target.outside-root: $targetPath")
    else if (!Files.isRegularFile(candidate, LinkOption.NOFOLLOW_LINKS))
      Consequence.resourceNotFound(s"pdf.site-link.target.unresolved: source=$sourcePath target=$targetPath")
    else {
      Consequence.execute {
        val canonical = candidate.toRealPath(LinkOption.NOFOLLOW_LINKS)
        if (!canonical.startsWith(siteRoot))
          throw new IllegalArgumentException(s"pdf.site-link.target.outside-root: $targetPath")
      }
    }
  }

  private def _site_document_metadata(path: String): Option[DocumentMetaData] =
    site.space.getContent(path).flatMap {
      case page: Page if Dox.getMetadata(page.dox).exists(_.status == DocumentMetaData.Status.Published) =>
        Dox.getMetadata(page.dox)
      case _ => None
    }
}

object SitePublicationContext {
  case class ResolvedSiteLink(
    sourcePath: String,
    targetPath: String,
    relativeUri: URI,
    publicPath: String,
    title: I18NFragment,
    tooltip: Option[org.goldenport.i18n.I18NString]
  ) {
    def localizedTitle(locale: Locale): Consequence[String] =
      if (!title.hasExactSource(locale))
        Consequence.resourceNotFound(
          s"pdf.site-link.target.localized-title-missing: path=$targetPath locale=${locale.toLanguageTag}"
        )
      else {
        val value = title.distillString(locale).trim
        if (value.isEmpty)
          Consequence.resourceNotFound(
            s"pdf.site-link.target.localized-title-missing: path=$targetPath locale=${locale.toLanguageTag}"
          )
        else
          Consequence.success(value)
      }
  }

  case class ResolvedLink(
    uri: URI,
    title: String,
    document: ResolvedSiteLink
  )

  private case class Admitted(
    root: Path,
    config: Path,
    source: Path,
    sourcepath: String
  )

  def create(
    siteRoot: File,
    siteConfig: File,
    sourceDocument: File,
    context: GeneratorContext
  ): Consequence[SitePublicationContext] =
    _admit(siteRoot.toPath, siteConfig.toPath, sourceDocument.toPath).flatMap { admitted =>
      Consequence.execute {
        DoxSite.create(
          context,
          admitted.root.toFile,
          None,
          DoxSiteConfigLoader.config(
            DoxSite.Config.default.copy(strategy = DoxSite.Strategy.Full),
            admitted.config.toFile
          )(context.i18NContext)
        )
      }.flatMap { site =>
        _site_base_uri(site.config).map { base =>
          SitePublicationContext(
            admitted.root,
            admitted.config,
            admitted.source,
            admitted.sourcepath,
            site,
            base
          )
        }
      }
    }

  def resolveSiteLink(
    sourcePath: String,
    target: URI,
    lookup: String => Option[DocumentMetaData]
  ): Consequence[ResolvedSiteLink] =
    _target_path(sourcePath, target).flatMap { targetpath =>
      lookup(targetpath).flatMap(_.title) match {
        case Some(title) =>
          val relative = _relative_uri(sourcePath, targetpath, target.getRawFragment)
          Consequence.success(
            ResolvedSiteLink(
              sourcePath,
              targetpath,
              relative,
              StringUtils.changeSuffix(targetpath, "html"),
              title,
              lookup(targetpath).flatMap(_.getEffectiveTooltip)
            )
          )
        case None =>
          Consequence.resourceNotFound(
            s"pdf.site-link.target.unresolved: source=$sourcePath target=${target.toString}"
          )
      }
    }

  private[doxsite] def publicUri(
    base: URI,
    config: DoxSite.Config,
    language: Locale,
    publicPath: String
  ): Consequence[URI] =
    locale(language.toLanguageTag).flatMap { routeLocale =>
      val route = config.siteOutput.localeMode match {
        case DoxSite.Config.SiteOutput.LocaleMode.MultiLocaleSubdirs =>
          s"${routeLocale.toLanguageTag}/$publicPath"
        case DoxSite.Config.SiteOutput.LocaleMode.SingleLocaleRoot =>
          publicPath
      }
      Consequence.execute {
        val root = base.toASCIIString.stripSuffix("/") + "/"
        new URI(root).resolve(new URI(route))
      }.flatMap { uri =>
        if (Option(uri.getScheme).exists(_.equalsIgnoreCase("https")) && Option(uri.getHost).exists(_.nonEmpty))
          Consequence.success(uri)
        else
          Consequence.invalidArgumentFault(s"pdf.site-context.base-url.invalid: $base")
      }
    }

  private[smartdox] def locale(value: String): Consequence[Locale] = {
    val locale = Locale.forLanguageTag(value)
    if (value.isEmpty || value.trim != value || locale.toLanguageTag != value || (value != "ja" && value != "en"))
      Consequence.invalidArgumentFault(s"pdf.site-context.locale.invalid: $value")
    else
      Consequence.success(locale)
  }

  private def _admit(root: Path, config: Path, source: Path): Consequence[Admitted] = {
    if (!root.isAbsolute)
      Consequence.invalidArgumentFault(s"pdf.site-context.root.absolute-required: $root")
    else if (!config.isAbsolute)
      Consequence.invalidArgumentFault(s"pdf.site-context.config.absolute-required: $config")
    else {
      val rootpath = root.normalize
      val configpath = config.normalize
      val sourcepath = source.toAbsolutePath.normalize
      if (!Files.isDirectory(rootpath, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(rootpath))
      Consequence.invalidArgumentFault(s"pdf.site-context.root.invalid: $rootpath")
      else if (!Files.isRegularFile(configpath, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(configpath))
      Consequence.invalidArgumentFault(s"pdf.site-context.config.invalid: $configpath")
      else if (!Files.isRegularFile(sourcepath, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(sourcepath))
      Consequence.invalidArgumentFault(s"pdf.site-context.source.invalid: $sourcepath")
      else {
        Consequence.execute {
          val canonicalroot = rootpath.toRealPath(LinkOption.NOFOLLOW_LINKS)
          val canonicalconfig = configpath.toRealPath(LinkOption.NOFOLLOW_LINKS)
          val canonicalsource = sourcepath.toRealPath(LinkOption.NOFOLLOW_LINKS)
          if (!canonicalconfig.startsWith(canonicalroot))
            throw new IllegalArgumentException(s"pdf.site-context.config.outside-root: $canonicalconfig")
          if (!canonicalsource.startsWith(canonicalroot))
            throw new IllegalArgumentException(s"pdf.site-context.source.outside-root: $canonicalsource")
          val filename = canonicalconfig.getFileName.toString
          if (!filename.endsWith(".conf") || filename.length == ".conf".length)
            throw new IllegalArgumentException(s"pdf.site-context.config.invalid: $canonicalconfig")
          Admitted(
            canonicalroot,
            canonicalconfig,
            canonicalsource,
            canonicalroot.relativize(canonicalsource).toString.replace(File.separatorChar, '/')
          )
        }
      }
    }
  }

  private def _site_base_uri(config: DoxSite.Config): Consequence[URI] =
    config.siteUrl match {
      case Some(url) =>
        Consequence.execute {
          new URI(url.toString)
        }.flatMap { uri =>
          if (Option(uri.getScheme).exists(_.equalsIgnoreCase("https")) && Option(uri.getHost).exists(_.nonEmpty))
            Consequence.success(uri)
          else
            Consequence.invalidArgumentFault(s"pdf.site-context.base-url.invalid: $url")
        }
      case None => Consequence.invalidArgumentFault("pdf.site-context.base-url.missing")
    }

  private def _target_path(sourcePath: String, target: URI): Consequence[String] =
    if (
      target.isAbsolute ||
      target.getRawAuthority != null ||
      target.getRawQuery != null ||
      Option(target.getPath).exists(_.startsWith("/"))
    )
      Consequence.invalidArgumentFault(s"pdf.site-link.target.invalid: ${target.toString}")
    else {
      Option(target.getPath).filter(_.nonEmpty) match {
        case Some(path) =>
          _resolve_segments(_source_parent_segments(sourcePath), path).flatMap { segments =>
            if (segments.isEmpty)
              Consequence.invalidArgumentFault(s"pdf.site-link.target.invalid: ${target.toString}")
            else
              Consequence.success(segments.mkString("/"))
          }
        case None => Consequence.invalidArgumentFault(s"pdf.site-link.target.invalid: ${target.toString}")
      }
    }

  private def _source_parent_segments(sourcePath: String): Vector[String] = {
    val segments = sourcePath.split('/').toVector.filter(_.nonEmpty)
    if (segments.length >= 2 && segments.last == "index.dox" && segments(segments.length - 2).endsWith(".dox"))
      segments.dropRight(2)
    else
      segments.dropRight(1)
  }

  private def _resolve_segments(base: Vector[String], path: String): Consequence[Vector[String]] =
    path.split("/", -1).foldLeft(Consequence.success(base)) { (state, segment) =>
      state.flatMap { current =>
        segment match {
          case "" | "." => Consequence.success(current)
          case ".." =>
            if (current.nonEmpty)
              Consequence.success(current.dropRight(1))
            else
              Consequence.invalidArgumentFault(s"pdf.site-link.target.outside-root: $path")
          case value if value.indexOf('\\') >= 0 || value.indexOf('\u0000') >= 0 =>
            Consequence.invalidArgumentFault(s"pdf.site-link.target.invalid: $path")
          case value => Consequence.success(current :+ value)
        }
      }
    }

  private def _relative_uri(sourcePath: String, targetPath: String, fragment: String): URI = {
    val sourceparent = sourcePath.split('/').toVector.filter(_.nonEmpty).dropRight(1)
    val target = targetPath.split('/').toVector.filter(_.nonEmpty)
    val shared = sourceparent.zip(target).takeWhile { case (lhs, rhs) => lhs == rhs }.length
    val path = (Vector.fill(sourceparent.length - shared)("..") ++ target.drop(shared)).mkString("/")
    new URI(null, null, StringUtils.changeSuffix(path, "html"), null, fragment)
  }

  private class SiteLinkProjection(context: SitePublicationContext, locale: Locale) extends DoxHomoTreeTransformer {
    val treeTransformerContext: TreeTransformer.Context[Dox] = TreeTransformer.Context.default[Dox]

    override protected def make_Node(
      node: TreeNode[Dox],
      content: Dox
    ): TreeTransformer.Directive[Dox] = content match {
      case link: Hyperlink if link.linkKind == Hyperlink.LinkKind.Site =>
        val resolved = context.resolve(link.href, locale).take
        directive_node(link.copy(contents = List(Text(resolved.title)), href = resolved.uri))
      case _ => directive_default
    }
  }
}
