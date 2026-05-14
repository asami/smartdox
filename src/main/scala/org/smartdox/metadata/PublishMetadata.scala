package org.smartdox.metadata

import java.io.File
import java.net.URI
import io.circe.Json
import io.circe.parser
import org.goldenport.i18n.I18NString
import org.goldenport.realm.Realm
import org.goldenport.config.ConfigLoader
import org.goldenport.io.InputSource
import org.goldenport.value.DescriptiveAttributes
import org.smartdox._
import org.smartdox.doxsite.Page

/*
 * @since   May. 13, 2026
 * @version May. 14, 2026
 * @author  ASAMI, Tomoharu
 */
case class PublishMetadata(
  entries: Vector[PublishMetadata.Entry]
) {
  import PublishMetadata._

  def generatedPages: Vector[(String, Page)] =
    _catalog_page +: (_tutorial_catalog_pages ++ _groups.flatMap(_.pages))

  private def _catalog_page: (String, Page) = {
    val table = Table.Builder.headerString("Publication", "Summary", "Version", "Public Page", "Diagnostics")
    _groups.foreach { g =>
      table.append(Vector(
        Text(g.title),
        Text(g.summary),
        Text(g.version),
        _link("Open", s"../${g.userBasePath}/index.html"),
        _link("Inspect", s"${g.adminPath}/index.html")
      ))
    }
    "catalog/index.dox" -> _page("index.dox", "Publication Catalog", List(
      Paragraph.text("This catalog lists resources published from Cozy-generated publish.d metadata. Public pages are intended for site users. Diagnostics pages are intended for maintainers who need to inspect publication inputs and generated artifact references."),
      table()
    ))
  }

  private lazy val _groups: Vector[Group] =
    entries.groupBy(_.identity).toVector.sortBy(_._1).map {
      case (name, xs) => Group(name, xs.sortBy(_.path))
    }

  private def _tutorial_catalog_pages: Vector[(String, Page)] = {
    val tutorials = _groups.filter(_.userBasePath.startsWith("textus/tutorial/"))
    if (tutorials.isEmpty)
      Vector.empty
    else {
      val page = _get_page_definition("textus/tutorial")
      val tb = Table.Builder.headerString("Tutorial", "Summary", "Version")
      tutorials.foreach { g =>
        tb.append(
          _link(g.userTitleText, g.userTitleString, s"${g.userBasePath.stripPrefix("textus/tutorial/")}/index.html"),
          _inline(g.summaryText),
          Text(g.version)
        )
      }
      Vector("textus/tutorial/index.dox" -> _page("index.dox", page.flatMap(_.title.toI18NString).getOrElse(I18NString("Textus Tutorials")), List(
        _paragraph(page.map(_.descriptive.summary).getOrElse(DescriptiveAttributes.Text.empty), "This page lists tutorial collections for Textus. Open a tutorial to read its overview, download its package, or choose individual sample packages."),
        page.flatMap(_.descriptive.description.toI18NString).map(x => Paragraph(List(Dox.toDox(x)))).getOrElse(Fragment.empty),
        tb()
      )))
    }
  }

  private lazy val _page_definition_map: Map[String, PageDefinition] =
    _groups.flatMap(_.pageDefinitions).map(x => x.path -> x).toMap

  private def _get_page_definition(path: String): Option[PageDefinition] =
    _page_definition_map.get(path)

  case class Group(
    name: String,
    entries: Vector[Entry]
  ) {
    lazy val title: String = entries.flatMap(_.titleOption).headOption.getOrElse(name)
    lazy val pageDefinition: Option[PageDefinition] = pageDefinitions.find(_.path == userBasePath)
    lazy val userTitleText: DescriptiveAttributes.Text =
      pageDefinition.map(_.title).filter(_.nonEmpty).getOrElse(DescriptiveAttributes.Text(Some(if (name == "textus-tutorial") "Textus Tutorial" else title)))
    lazy val userTitleString: String =
      userTitleText.default.getOrElse(if (name == "textus-tutorial") "Textus Tutorial" else title)
    lazy val types: Vector[String] = entries.flatMap(_.typeOption).distinct
    lazy val basePath: String =
      entries.flatMap(_.publicationPath).headOption.getOrElse(s"repository/${_path_segment(name)}")
    lazy val userBasePath: String =
      basePath
    lazy val adminPath: String =
      s"catalog/${_path_segment(name)}"
    lazy val summaryText: DescriptiveAttributes.Text =
      pageDefinition.map(_.descriptive.summary).filter(_.nonEmpty).
        getOrElse(DescriptiveAttributes.Text(entries.flatMap(_.summaryOption).headOption.orElse(Some("Published resource."))))
    lazy val descriptionText: DescriptiveAttributes.Text =
      pageDefinition.map(_.descriptive.description).filter(_.nonEmpty).
        getOrElse(DescriptiveAttributes.Text(entries.flatMap(_.descriptionOption).headOption))
    lazy val summary: String = summaryText.default.getOrElse("Published resource.")
    lazy val description: Option[String] = descriptionText.default
    lazy val version: String =
      entries.flatMap(_.versionOption).headOption.getOrElse("")
    lazy val kind: String =
      entries.flatMap(_.kindOption).headOption.getOrElse("")
    lazy val pageDefinitions: Vector[PageDefinition] =
      entries.flatMap(_.pageDefinitions)
    lazy val sampleRefs: Vector[SampleRef] =
      _distinct_samples(entries.flatMap(_.sampleRefs))

    def pages: Vector[(String, Page)] = {
      val index = s"$userBasePath/index.dox" -> _page("index.dox", _to_i18n(userTitleText, userTitleString), _index_contents)
      val downloads = _downloads_page(entries.filter(_.isArtifact))
      val admin = s"$adminPath/index.dox" -> _page("index.dox", s"$title Publication Diagnostics", _admin_contents)
      val artifacts = _artifact_reference_page(entries.filter(_.isArtifact))
      val source = _source_manifest_page(entries.filter(_.isSourceManifest))
      val releases = _release_page(entries.filter(_.isRelease))
      val metadata = s"$adminPath/metadata.dox" -> _page("metadata.dox", s"$title Metadata Reference", _metadata_contents)
      Vector(Some(index), downloads, Some(admin), artifacts, source, releases, Some(metadata)).flatten
    }

    private def _index_contents: List[Dox] =
      if (_is_tutorial_collection)
        _tutorial_index_contents
      else
        _publication_index_contents

    private def _is_tutorial_collection: Boolean =
      userBasePath.startsWith("textus/tutorial/") && sampleRefs.nonEmpty

    private def _publication_index_contents: List[Dox] = {
      val tb = Table.Builder.headerString("Property", "Value")
      if (kind.nonEmpty)
        tb.append("Kind", kind)
      if (version.nonEmpty)
        tb.append("Version", version)
      tb.append("Resource path", s"/$userBasePath/")
      val actiontable = _actions
      List(_paragraph(pageDefinition.map(_.descriptive.lead).getOrElse(DescriptiveAttributes.Text.empty), _user_lead)) ++
        List(Paragraph(List(_inline(summaryText)))) ++
        descriptionText.toI18NString.map(x => Paragraph(List(Dox.toDox(x)))).toList ++
        List(tb()) ++
        List(Section("Related Pages", List(actiontable)))
    }

    private def _tutorial_index_contents: List[Dox] =
      _overview_contents ++ _sample_overview_table.toList ++ _sample_sections

    private def _overview_contents: List[Dox] =
      List(Paragraph(List(_inline(summaryText)))) ++
        descriptionText.toI18NString(description.getOrElse("")).map(x => Paragraph(List(Dox.toDox(x)))).toList ++
        List(Paragraph(List(_link(_label("Downloads", "ダウンロード"), "downloads.html"))))

    private def _sample_overview_table: Option[Table] =
      if (sampleRefs.isEmpty)
        None
      else {
        val tb = Table.Builder.header(Vector(
          _label("Tutorial", "チュートリアル"),
          _label("What you learn", "内容")
        ))
        sampleRefs.foreach { sample =>
          tb.append(Vector(
            Text(sample.displayTitle),
            _inline(sample.summaryText)
          ))
        }
        Some(tb())
      }

    private def _sample_sections: List[Dox] =
      if (sampleRefs.isEmpty)
        Nil
      else
        List(Section.create(I18NString.enja("Tutorials", "個々のチュートリアル"), sampleRefs.map(_.section)))

    private def _user_lead: String =
      if (name == "textus-tutorial")
        "This page is the entry point for the Textus Tutorial collection. Use the downloads page to obtain the complete tutorial package or an individual sample package."
      else
        "This page is the entry point for this published resource."

    private def _actions: Table = {
      val tb = Table.Builder.headerString("Page", "Purpose")
      if (entries.exists(_.isArtifact))
        tb.append(_link("Downloads", "downloads.html"), Text("Download the complete package or selected sample packages."))
      tb()
    }

    private def _admin_contents: List[Dox] = {
      val tb = Table.Builder.headerString("Page", "Purpose")
      if (entries.exists(_.isArtifact))
        tb.append(_link("Artifact Reference", "artifacts.html"), Text("Inspect generated artifact metadata, public paths, versions, and package records."))
      if (entries.exists(_.isSourceManifest))
        tb.append(_link("Source Manifest", "source-manifest.html"), Text("Inspect the source file inventory and checksums used to describe this publication."))
      if (entries.exists(_.isRelease))
        tb.append(_link("Release History", "releases.html"), Text("Inspect release metadata for this publication."))
      tb.append(_link("Metadata Reference", "metadata.html"), Text("Inspect the publish.d metadata records consumed by SmartDox."))
      List(
        Paragraph.text("These pages are for maintainers and release verification. They expose the metadata used to generate public publication pages and to link published artifacts."),
        tb()
      )
    }

    private def _metadata_contents: List[Dox] =
      List(
        Paragraph.text("Technical metadata records used by the publication bridge."),
        _entries_table("Metadata Files", entries)
      )

    private def _downloads_page(xs: Vector[Entry]): Option[(String, Page)] = {
      val files = xs.flatMap(_.artifactFiles)
      if (files.isEmpty)
        None
      else {
        val collectionfiles = files.filter(_artifact_type(_) == "sample-collection-zip")
        val samplefiles = files.filter(_artifact_type(_) == "sample-zip")
        val otherfiles = files.filterNot(f => Set("sample-collection-zip", "sample-zip").contains(_artifact_type(f)))
        val contents =
          List(_downloads_lead) ++
            _download_section(
              I18NString.enja("Complete Tutorial Package", "全体ダウンロード"),
              I18NString.enja(
                "Download this package when you want all tutorial materials in one archive.",
                "チュートリアル全体をまとめて入手する場合はこちらを使います。"
              ),
              collectionfiles,
              usesamplename = false
            ).toList ++
            _download_section(
              I18NString.enja("Individual Sample Packages", "個々の項目のダウンロード"),
              I18NString.enja(
                "Download an individual package when you only need one runnable sample.",
                "必要な実行サンプルだけを入手する場合はこちらを使います。"
              ),
              samplefiles,
              usesamplename = true
            ).toList ++
            _download_section(
              I18NString.enja("Other Packages", "その他のダウンロード"),
              I18NString.enja(
                "Additional downloadable packages for this publication.",
                "この公開項目に関連するその他のダウンロードです。"
              ),
              otherfiles,
              usesamplename = false
            ).toList
        Some(s"$userBasePath/downloads.dox" -> _page("downloads.dox", _downloads_title, contents))
      }
    }

    private def _downloads_lead: Paragraph =
      Paragraph(List(Dox.toDox(I18NString.enja(
        "Choose the complete tutorial package for the full set, or choose an individual sample package for a focused exercise.",
        "全体をまとめて使う場合は全体ダウンロードを、特定の演習だけを使う場合は個々の項目のダウンロードを選びます。"
      ))))

    private def _download_section(
      title: I18NString,
      description: I18NString,
      files: Vector[Json],
      usesamplename: Boolean
    ): Option[Section] =
      if (files.isEmpty)
        None
      else
        Some(Section.create(title, List(
          Paragraph(List(Dox.toDox(description))),
          _download_table(files, usesamplename)
        )))

    private def _download_table(files: Vector[Json], usesamplename: Boolean): Table = {
      val tb = Table.Builder.header(Vector(
        _label(if (usesamplename) "Sample" else "Package", if (usesamplename) "項目" else "パッケージ"),
        _label("Version", "バージョン"),
        _label("Download", "ダウンロード")
      ))
      files.foreach { f =>
        val publicpath = _public_path(f)
        val label =
          if (usesamplename)
            _json_string(f, "sample").filter(_.nonEmpty).
              orElse(_sample_name_from_public_path(publicpath)).
              orElse(_json_string(f, "name")).
              getOrElse("Download")
          else
            _json_string(f, "name").orElse(publicpath.split("/").lastOption).getOrElse("Download")
        val link =
          if (publicpath.isEmpty) Text("")
          else _link(_label("Download", "ダウンロード"), _root_relative(publicpath))
        tb.append(Vector(
          Text(label),
          Text(_json_string(f, "version").getOrElse("")),
          link
        ))
      }
      tb()
    }

    private def _artifact_type(json: Json): String =
      _json_string(json, "type").getOrElse("artifact")

    private def _public_path(json: Json): String =
      _json_string(json, "publicPath").orElse(_json_string(json, "public_path")).orElse(_json_string(json, "path")).getOrElse("")

    private def _sample_name_from_public_path(path: String): Option[String] = {
      val segments = path.split("/").toVector.filter(_.nonEmpty)
      if (segments.size >= 2)
        Some(segments(segments.size - 2)).filter(_.nonEmpty)
      else
        None
    }

    private def _downloads_title: I18NString =
      if (_is_tutorial_collection)
        I18NString.enja("Textus Tutorial Downloads", "Textus チュートリアル ダウンロード")
      else
        I18NString(s"$title Downloads")

    private def _artifact_reference_page(xs: Vector[Entry]): Option[(String, Page)] =
      if (xs.isEmpty)
        None
      else
        Some(s"$adminPath/artifacts.dox" -> _page("artifacts.dox", s"$title Artifact Reference", List(
          Paragraph.text("This maintainer page shows artifact metadata generated by Cozy and consumed by SmartDox. Use it to verify public paths, artifact types, versions, and expected package records.")
        ) ++ _detail_contents(xs)))

    private def _source_manifest_page(xs: Vector[Entry]): Option[(String, Page)] = {
      val files = xs.flatMap(_.sourceFiles)
      if (files.isEmpty)
        None
      else {
        val tb = Table.Builder.headerString("Path", "Size", "Checksum")
        files.take(80).foreach { f =>
          tb.appendString(Vector(
            _json_string(f, "path").getOrElse(""),
            _number_string(f, "size").getOrElse(""),
            _json_string(f, "sha256").map(_.take(16) + "...").getOrElse("")
          ))
        }
        val more =
          if (files.size > 80)
            List(Paragraph.text(s"${files.size - 80} more files are listed in the raw metadata."))
          else
            Nil
        Some(s"$adminPath/source-manifest.dox" -> _page("source-manifest.dox", s"$title Source Manifest",
          Paragraph.text("This maintainer page lists source files and checksums recorded in publish.d. Use it to verify what source content was described at publication time.") :: tb() :: more
        ))
      }
    }

    private def _release_page(xs: Vector[Entry]): Option[(String, Page)] =
      if (xs.isEmpty)
        None
      else {
        val tb = Table.Builder.headerString("Version", "Date", "Status")
        xs.foreach { entry =>
          tb.appendString(Vector(
            entry.string("release", "version").orElse(entry.versionOption).getOrElse(""),
            entry.string("release", "date").getOrElse(""),
            entry.string("release", "status").getOrElse("")
          ))
        }
        Some(s"$adminPath/releases.dox" -> _page("releases.dox", s"$title Release History", List(
          Paragraph.text("This maintainer page shows release metadata recorded for this publication."),
          tb()
        )))
      }

    private def _detail_contents(xs: Vector[Entry]): List[Dox] =
      xs.toList.map { entry =>
        Section(entry.displayTitle, List(_field_table(entry)))
      }
  }
}

object PublishMetadata {
  case class PageDefinition(
    path: String,
    title: DescriptiveAttributes.Text,
    descriptive: DescriptiveAttributes
  )

  case class Entry(
    path: String,
    key: String,
    json: Json
  ) {
    def logicalPath: String = _logical_path(path)
    def logicalKey: String = _logical_path(key)
    def schemaOption: Option[String] = string("schema")
    def typeOption: Option[String] = string("type")
    def publicationPath: Option[String] =
      string("publication", "path").map(_.trim).filter(_.nonEmpty).map(_validate_path)
    def identity: String =
      string("project", "name").
        orElse(string("publication", "name")).
        orElse(string("name")).
        orElse(string("artifact", "name")).
        getOrElse(_path_segment(key.split("/").lastOption.getOrElse("metadata")))
    def titleOption: Option[String] =
      string("project", "title").orElse(string("sample", "title")).orElse(string("publication", "title")).orElse(string("title"))
    def summaryOption: Option[String] =
      string("project", "summary").orElse(string("sample", "summary")).orElse(string("summary")).
        map(_.trim).filter(_.nonEmpty).filterNot(_ == ">")
    def descriptionOption: Option[String] =
      string("project", "description").orElse(string("sample", "description")).orElse(string("description")).
        map(_.trim).filter(_.nonEmpty).filterNot(_ == ">")
    def versionOption: Option[String] =
      string("project", "version").orElse(string("sample", "version")).orElse(string("release", "version"))
    def kindOption: Option[String] =
      string("project", "kind").orElse(string("sample", "kind")).orElse(string("kind"))
    def artifactFiles: Vector[Json] =
      array("artifact", "files")
    def sourceFiles: Vector[Json] =
      array("files") ++ array("source_manifest")
    def sampleRefs: Vector[SampleRef] =
      array("samples").flatMap(SampleRef.fromJson)
    def displayTitle: String =
      List(typeOption, Some(path)).flatten.mkString(" - ")
    def isSourceManifest: Boolean =
      logicalKey.startsWith("source-manifest/") || typeOption.exists(_.contains("source"))
    def isArtifact: Boolean =
      logicalKey.startsWith("artifacts/") || logicalKey.startsWith("repository/") || logicalKey.startsWith("maven/") ||
        typeOption.exists(x => x.contains("artifact") || x.contains("maven"))
    def isRelease: Boolean =
      logicalKey.startsWith("releases/") || typeOption.exists(_.contains("release"))
    def isPublicationPages: Boolean =
      logicalKey.startsWith("publication-pages/") || typeOption.contains("publication-pages")
    def pageDefinitions: Vector[PageDefinition] =
      if (isPublicationPages)
        array("pages").flatMap { page =>
          _json_string(page, "path").map { p =>
            PageDefinition(
              _validate_path(p),
              DescriptiveAttributes.textFromJson(page, "title"),
              DescriptiveAttributes.fromJson(page)
            )
          }
        }
      else
        Vector.empty

    def string(path: String*): Option[String] =
      field(path: _*).flatMap(_.asString)

    def field(path: String*): Option[Json] =
      path.foldLeft(Option(json)) {
        case (Some(z), x) => z.hcursor.downField(x).focus
        case (None, _) => None
      }

    def array(path: String*): Vector[Json] =
      field(path: _*).flatMap(_.asArray).getOrElse(Vector.empty)

    def projectRows: Vector[(String, String)] = {
      val keys = Vector("kind", "version", "scala_version", "scalaVersion", "sbt_version", "sbtVersion")
      keys.flatMap { key =>
        string("project", key).map(key -> _)
      }
    }
  }

  case class SampleRef(
    name: String,
    title: DescriptiveAttributes.Text,
    summaryText: DescriptiveAttributes.Text,
    descriptionText: DescriptiveAttributes.Text,
    directory: Option[String],
    version: Option[String]
  ) {
    def displayTitle: String =
      name

    def section: Section = {
      val body =
        List(Paragraph(List(_inline(summaryText))))
      Section(List(Text(displayTitle)), body)
    }
  }

  private def _distinct_samples(samples: Vector[SampleRef]): Vector[SampleRef] = {
    case class Z(
      seen: Set[String] = Set.empty,
      samples: Vector[SampleRef] = Vector.empty
    ) {
      def +(rhs: SampleRef): Z =
        if (seen.contains(rhs.name))
          this
        else
          Z(seen + rhs.name, samples :+ rhs)
    }
    samples.foldLeft(Z())(_+_).samples
  }

  object SampleRef {
    def fromJson(json: Json): Option[SampleRef] =
      _json_string(json, "name").map { name =>
        val descriptive = DescriptiveAttributes.fromJson(json)
        SampleRef(
          name = name,
          title = DescriptiveAttributes.textFromJson(json, "title"),
          summaryText = descriptive.summary,
          descriptionText = descriptive.description,
          directory = _json_string(json, "directory"),
          version = _json_string(json, "version")
        )
      }
  }

  def load(publish: Option[File]): Option[PublishMetadata] =
    _directory(publish).flatMap(load)

  def load(publish: File): Option[PublishMetadata] = {
    val entries = _metadata_files(publish).map(_entry(publish, _))
    if (entries.isEmpty)
      None
    else
      Some(PublishMetadata(entries))
  }

  def rawRealm(publish: Option[File]): Option[Realm] =
    _directory(publish).map(Realm.create)

  private def _directory(publish: Option[File]): Option[File] =
    publish.filter(x => x.exists && x.isDirectory)

  private def _metadata_files(base: File): Vector[File] = {
    val files = _files(base).filter { x =>
      val n = x.getName.toLowerCase
      (n.endsWith(".json") || n.endsWith(".yaml") || n.endsWith(".yml")) &&
        _is_page_source_key(_logical_path(_strip_suffix(_relative_path(base, x))))
    }
    files.groupBy(x => _strip_suffix(_relative_path(base, x))).toVector.sortBy(_._1).flatMap {
      case (_, xs) => xs.sortBy(_priority).headOption
    }
  }

  private def _files(base: File): Vector[File] =
    Option(base.listFiles).toVector.flatten.toVector.flatMap { x =>
      if (x.isDirectory)
        _files(x)
      else
        Vector(x)
    }

  private def _entry(base: File, file: File): Entry = {
    val json = _parse_metadata(file)
    val path = _relative_path(base, file)
    Entry(path, _strip_suffix(path), json)
  }

  private def _parse_metadata(file: File): Json = {
    val in = InputSource(file)
    if (file.getName.toLowerCase.endsWith(".json"))
      parser.parse(in.asText) match {
        case Right(r) => r
        case Left(l) => throw new IllegalArgumentException(s"Invalid publish metadata JSON: ${file.getPath}: ${l.message}", l)
      }
    else
      ConfigLoader.loadConfigFromYaml[Json](in).fold(
        e => throw new IllegalArgumentException(s"Invalid publish metadata YAML: ${file.getPath}: ${e.message}"),
        identity
      )
  }

  private def _priority(file: File): Int =
    if (file.getName.toLowerCase.endsWith(".json")) 0 else 1

  private def _relative_path(base: File, file: File): String =
    base.toPath.relativize(file.toPath).toString.replace(File.separatorChar, '/')

  private def _strip_suffix(path: String): String =
    path.replaceFirst("""\.[^.]+$""", "")

  private def _logical_path(path: String): String =
    if (path.startsWith("metadata/"))
      path.substring("metadata/".length)
    else
      path

  private def _is_page_source_key(key: String): Boolean =
    key.startsWith("catalog/") ||
      key.matches("""projects/[^/]+/metadata""") ||
      key.matches("""samples/[^/]+/metadata""") ||
      key.startsWith("publication-pages/") ||
      key.startsWith("source-manifest/") ||
      key.startsWith("artifacts/") ||
      key.startsWith("repository/") ||
      key.startsWith("maven/") ||
      key.startsWith("releases/")

  private def _validate_path(path: String): String = {
    val segments = path.split("/").toVector.filter(_.nonEmpty)
    if (segments.isEmpty)
      throw new IllegalArgumentException("publication.path must not be empty")
    segments.foreach { x =>
      if (!_is_valid_path_segment(x))
        throw new IllegalArgumentException(s"Invalid publication.path segment: $path")
    }
    segments.mkString("/")
  }

  private def _is_valid_path_segment(s: String): Boolean =
    s.matches("""[A-Za-z0-9._-]+""") && s != "." && s != ".."

  private def _path_segment(s: String): String = {
    val a = s.trim.replaceAll("""[^A-Za-z0-9._-]+""", "-")
    if (a.isEmpty) "metadata" else a
  }

  private def _to_i18n(text: DescriptiveAttributes.Text, fallback: String): I18NString =
    text.toI18NString(fallback).getOrElse(I18NString(fallback))

  private def _label(en: String, ja: String): Inline =
    Dox.toDox(I18NString.enja(en, ja))

  private def _inline(text: DescriptiveAttributes.Text): Inline =
    Dox.toDox(_to_i18n(text, text.default.getOrElse("")))

  private def _paragraph(text: DescriptiveAttributes.Text, fallback: String): Paragraph =
    Paragraph(List(Dox.toDox(_to_i18n(text, fallback))))

  private def _page(name: String, title: I18NString, contents: List[Dox]): Page = {
    val meta = DocumentMetaData.create(Dox.toInlineContents(title), Explanation.empty)
    val dox = Document(Head(metadata = meta), Body(contents.filterNot(_ == Fragment.empty)))
    Page(name, dox)
  }

  private def _page(name: String, title: String, contents: List[Dox]): Page = {
    val meta = DocumentMetaData.create(title, Explanation.empty)
    val dox = Document(Head(metadata = meta), Body(contents))
    Page(name, dox)
  }

  private def _link(label: String, href: String): Hyperlink =
    Hyperlink(Text(label), new URI(href))

  private def _link(label: Inline, href: String): Hyperlink =
    Hyperlink(label, new URI(href))

  private def _link(label: DescriptiveAttributes.Text, fallback: String, href: String): Hyperlink =
    Hyperlink(List(Dox.toDox(_to_i18n(label, fallback))), new URI(href))

  private def _root_relative(path: String): String =
    "/" + path.stripPrefix("/")

  private def _json_string(json: Json, path: String*): Option[String] =
    path.foldLeft(Option(json)) {
      case (Some(z), x) => z.hcursor.downField(x).focus
      case (None, _) => None
    }.flatMap(_.asString)

  private def _number_string(json: Json, path: String*): Option[String] =
    path.foldLeft(Option(json)) {
      case (Some(z), x) => z.hcursor.downField(x).focus
      case (None, _) => None
    }.flatMap { x =>
      x.asNumber.map(_.toString).orElse(x.asString)
    }

  private def _entries_table(caption: String, xs: Vector[Entry]): Table = {
    val tb = Table.Builder.captionHeaderString(caption, Vector("File", "Type", "Schema"))
    xs.foreach { x =>
      tb.appendString(Vector(
        x.path,
        x.typeOption.getOrElse(""),
        x.schemaOption.getOrElse("")
      ))
    }
    tb()
  }

  private def _field_table(entry: Entry): Table = {
    val tb = Table.Builder.headerString("Field", "Value")
    _flatten(entry.json).take(120).foreach {
      case (k, v) => tb.append(k, v)
    }
    tb()
  }

  private def _flatten(json: Json): Vector[(String, String)] =
    _flatten("", json)

  private def _flatten(prefix: String, json: Json): Vector[(String, String)] =
    json.asObject match {
      case Some(obj) =>
        obj.toVector.flatMap {
          case (k, v) => _flatten(_join_path(prefix, k), v)
        }
      case None =>
        json.asArray match {
          case Some(xs) =>
            if (xs.forall(x => x.asObject.isEmpty && x.asArray.isEmpty))
              Vector(prefix -> xs.map(_scalar).mkString(", "))
            else
              xs.zipWithIndex.toVector.flatMap {
                case (x, i) => _flatten(s"$prefix[$i]", x)
              }
          case None => Vector(prefix -> _scalar(json))
        }
    }

  private def _join_path(prefix: String, key: String): String =
    if (prefix.isEmpty) key else s"$prefix.$key"

  private def _scalar(json: Json): String =
    json.asString.
      orElse(json.asNumber.map(_.toString)).
      orElse(json.asBoolean.map(_.toString)).
      getOrElse(if (json.isNull) "" else json.noSpaces)
}
