package org.smartdox.metadata

import java.net.URI

import io.circe.Json
import org.goldenport.i18n.I18NString
import org.goldenport.value.DescriptiveAttributes
import org.smartdox._
import org.smartdox.doxsite.Page

import PublishMetadata._

/*
 * @since   Sep.  8, 2026
 * @version Sep.  8, 2026
 * @author  ASAMI, Tomoharu
 */
private[metadata] object PublishMetadataCatalogPageSupport {
  def generatedPages(publishMetadata: PublishMetadata): Vector[(String, Page)] = {
    val groups = _groups(publishMetadata)
    val pagedefinitionmap = groups.flatMap(_.pageDefinitions).map(x => x.path -> x).toMap
    _catalog_page(groups) +: (_tutorial_catalog_pages(groups, pagedefinitionmap) ++ groups.flatMap(_.pages))
  }

  def pages(group: PublishMetadata#Group): Vector[(String, Page)] = {
    val index = s"${group.userBasePath}/index.dox" -> _page("index.dox", _to_i18n(group.userTitleText, group.userTitleString), _index_contents(group))
    val downloads = _downloads_page(group, group.entries.filter(_.isArtifact))
    val admin = s"${group.adminPath}/index.dox" -> _page("index.dox", s"${group.title} Publication Diagnostics", _admin_contents(group))
    val artifacts = _artifact_reference_page(group, group.entries.filter(_.isArtifact))
    val source = _source_manifest_page(group, group.entries.filter(_.isSourceManifest))
    val releases = _release_page(group, group.entries.filter(_.isRelease))
    val metadata = s"${group.adminPath}/metadata.dox" -> _page("metadata.dox", s"${group.title} Metadata Reference", _metadata_contents(group))
    Vector(Some(index), downloads, Some(admin), artifacts, source, releases, Some(metadata)).flatten
  }

  def sampleSection(sample: SampleRef): Section = {
    val body =
      List(Paragraph(List(_inline(sample.summaryText))))
    Section(List(Text(sample.displayTitle)), body)
  }

  private def _groups(publishmetadata: PublishMetadata): Vector[PublishMetadata#Group] =
    publishmetadata.entries.groupBy(_.identity).toVector.sortBy(_._1).map {
      case (name, xs) => publishmetadata.Group(name, xs.sortBy(_.path))
    }

  private def _catalog_page(groups: Vector[PublishMetadata#Group]): (String, Page) = {
    val table = Table.Builder.headerString("Publication", "Summary", "Version", "Public Page", "Diagnostics")
    groups.foreach { group =>
      table.append(Vector(
        Text(group.title),
        Text(group.summary),
        Text(group.version),
        _link("Open", s"../${group.userBasePath}/index.html"),
        _link("Inspect", s"${group.adminPath}/index.html")
      ))
    }
    "catalog/index.dox" -> _page("index.dox", "Publication Catalog", List(
      Paragraph.text("This catalog lists resources published from the Cozy-generated publication registry. Public pages are intended for site users. Diagnostics pages are intended for maintainers who need to inspect publication inputs and generated artifact references."),
      table()
    ))
  }

  private def _tutorial_catalog_pages(
    groups: Vector[PublishMetadata#Group],
    pagedefinitionmap: Map[String, PageDefinition]
  ): Vector[(String, Page)] = {
    val tutorials = groups.filter(_.userBasePath.startsWith("textus/tutorial/"))
    if (tutorials.isEmpty)
      Vector.empty
    else {
      val page = pagedefinitionmap.get("textus/tutorial")
      val tb = Table.Builder.headerString("Tutorial", "Summary", "Version")
      tutorials.foreach { group =>
        tb.append(
          _link(group.userTitleText, group.userTitleString, s"${group.userBasePath.stripPrefix("textus/tutorial/")}/index.html"),
          _inline(group.summaryText),
          Text(group.version)
        )
      }
      Vector("textus/tutorial/index.dox" -> _page("index.dox", page.flatMap(_.title.toI18NString).getOrElse(I18NString("Textus Tutorials")), List(
        _paragraph(page.map(_.descriptive.summary).getOrElse(DescriptiveAttributes.Text.empty), "This page lists tutorial collections for Textus. Open a tutorial to read its overview, download its package, or choose individual sample packages."),
        page.flatMap(_.descriptive.description.toI18NString).map(x => Paragraph(List(Dox.toDox(x)))).getOrElse(Fragment.empty),
        tb()
      )))
    }
  }

  private def _index_contents(group: PublishMetadata#Group): List[Dox] =
    if (_is_tutorial_collection(group))
      _tutorial_index_contents(group)
    else
      _publication_index_contents(group)

  private def _is_tutorial_collection(group: PublishMetadata#Group): Boolean =
    group.userBasePath.startsWith("textus/tutorial/") && group.sampleRefs.nonEmpty

  private def _publication_index_contents(group: PublishMetadata#Group): List[Dox] = {
    val tb = Table.Builder.headerString("Property", "Value")
    if (group.kind.nonEmpty)
      tb.append("Kind", group.kind)
    if (group.version.nonEmpty)
      tb.append("Version", group.version)
    tb.append("Resource path", s"/${group.userBasePath}/")
    val actiontable = _actions(group)
    List(_paragraph(group.pageDefinition.map(_.descriptive.lead).getOrElse(DescriptiveAttributes.Text.empty), _user_lead(group))) ++
      List(Paragraph(List(_inline(group.summaryText)))) ++
      group.descriptionText.toI18NString.map(x => Paragraph(List(Dox.toDox(x)))).toList ++
      List(tb()) ++
      List(Section("Related Pages", List(actiontable)))
  }

  private def _tutorial_index_contents(group: PublishMetadata#Group): List[Dox] =
    _overview_contents(group) ++ _sample_overview_table(group).toList ++ _sample_sections(group)

  private def _overview_contents(group: PublishMetadata#Group): List[Dox] =
    List(Paragraph(List(_inline(group.summaryText)))) ++
      group.descriptionText.toI18NString(group.description.getOrElse("")).map(x => Paragraph(List(Dox.toDox(x)))).toList ++
      List(Paragraph(List(_link(_label("Downloads", "ダウンロード"), "downloads.html"))))

  private def _sample_overview_table(group: PublishMetadata#Group): Option[Table] =
    if (group.sampleRefs.isEmpty)
      None
    else {
      val tb = Table.Builder.header(Vector(
        _label("Tutorial", "チュートリアル"),
        _label("What you learn", "内容")
      ))
      group.sampleRefs.foreach { sample =>
        tb.append(Vector(
          Text(sample.displayTitle),
          _inline(sample.summaryText)
        ))
      }
      Some(tb())
    }

  private def _sample_sections(group: PublishMetadata#Group): List[Dox] =
    if (group.sampleRefs.isEmpty)
      Nil
    else
      List(Section.create(I18NString.enja("Tutorials", "個々のチュートリアル"), group.sampleRefs.map(_.section)))

  private def _user_lead(group: PublishMetadata#Group): String =
    if (group.name == "textus-tutorial")
      "This page is the entry point for the Textus Tutorial collection. Use the downloads page to obtain the complete tutorial package or an individual sample package."
    else
      "This page is the entry point for this published resource."

  private def _actions(group: PublishMetadata#Group): Table = {
    val tb = Table.Builder.headerString("Page", "Purpose")
    if (group.entries.exists(_.isArtifact))
      tb.append(_link("Downloads", "downloads.html"), Text("Download the complete package or selected sample packages."))
    tb()
  }

  private def _admin_contents(group: PublishMetadata#Group): List[Dox] = {
    val tb = Table.Builder.headerString("Page", "Purpose")
    if (group.entries.exists(_.isArtifact))
      tb.append(_link("Artifact Reference", "artifacts.html"), Text("Inspect generated artifact metadata, public paths, versions, and package records."))
    if (group.entries.exists(_.isSourceManifest))
      tb.append(_link("Source Manifest", "source-manifest.html"), Text("Inspect the source file inventory and checksums used to describe this publication."))
    if (group.entries.exists(_.isRelease))
      tb.append(_link("Release History", "releases.html"), Text("Inspect release metadata for this publication."))
    tb.append(_link("Metadata Reference", "metadata.html"), Text("Inspect the publication registry metadata records consumed by SmartDox."))
    List(
      Paragraph.text("These pages are for maintainers and release verification. They expose the metadata used to generate public publication pages and to link published artifacts."),
      tb()
    )
  }

  private def _metadata_contents(group: PublishMetadata#Group): List[Dox] =
    List(
      Paragraph.text("Technical metadata records used by the publication bridge."),
      _entries_table("Metadata Files", group.entries)
    )

  private def _downloads_page(group: PublishMetadata#Group, xs: Vector[Entry]): Option[(String, Page)] = {
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
      Some(s"${group.userBasePath}/downloads.dox" -> _page("downloads.dox", _downloads_title(group), contents))
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
    files.foreach { file =>
      val publicpath = _public_path(file)
      val label =
        if (usesamplename)
          _json_string(file, "sample").filter(_.nonEmpty).
            orElse(_sample_name_from_public_path(publicpath)).
            orElse(_json_string(file, "name")).
            getOrElse("Download")
        else
          _json_string(file, "name").orElse(publicpath.split("/").lastOption).getOrElse("Download")
      val link =
        if (publicpath.isEmpty) Text("")
        else _link(_label("Download", "ダウンロード"), _root_relative(publicpath))
      tb.append(Vector(
        Text(label),
        Text(_json_string(file, "version").getOrElse("")),
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

  private def _downloads_title(group: PublishMetadata#Group): I18NString =
    if (_is_tutorial_collection(group))
      I18NString.enja("Textus Tutorial Downloads", "Textus チュートリアル ダウンロード")
    else
      I18NString(s"${group.title} Downloads")

  private def _artifact_reference_page(group: PublishMetadata#Group, xs: Vector[Entry]): Option[(String, Page)] =
    if (xs.isEmpty)
      None
    else
      Some(s"${group.adminPath}/artifacts.dox" -> _page("artifacts.dox", s"${group.title} Artifact Reference", List(
        Paragraph.text("This maintainer page shows artifact metadata generated by Cozy and consumed by SmartDox. Use it to verify public paths, artifact types, versions, and expected package records.")
      ) ++ _detail_contents(xs)))

  private def _source_manifest_page(group: PublishMetadata#Group, xs: Vector[Entry]): Option[(String, Page)] = {
    val files = xs.flatMap(_.sourceFiles)
    if (files.isEmpty)
      None
    else {
      val tb = Table.Builder.headerString("Path", "Size", "Checksum")
      files.take(80).foreach { file =>
        tb.appendString(Vector(
          _json_string(file, "path").getOrElse(""),
          _number_string(file, "size").getOrElse(""),
          _json_string(file, "sha256").map(_.take(16) + "...").getOrElse("")
        ))
      }
      val more =
        if (files.size > 80)
          List(Paragraph.text(s"${files.size - 80} more files are listed in the raw metadata."))
        else
          Nil
      Some(s"${group.adminPath}/source-manifest.dox" -> _page("source-manifest.dox", s"${group.title} Source Manifest",
        Paragraph.text("This maintainer page lists source files and checksums recorded in the publication registry. Use it to verify what source content was described at publication time.") :: tb() :: more
      ))
    }
  }

  private def _release_page(group: PublishMetadata#Group, xs: Vector[Entry]): Option[(String, Page)] =
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
      Some(s"${group.adminPath}/releases.dox" -> _page("releases.dox", s"${group.title} Release History", List(
        Paragraph.text("This maintainer page shows release metadata recorded for this publication."),
        tb()
      )))
    }

  private def _detail_contents(xs: Vector[Entry]): List[Dox] =
    xs.toList.map { entry =>
      Section(entry.displayTitle, List(_field_table(entry)))
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
    }.flatMap { value =>
      value.asNumber.map(_.toString).orElse(value.asString)
    }

  private def _entries_table(caption: String, xs: Vector[Entry]): Table = {
    val tb = Table.Builder.captionHeaderString(caption, Vector("File", "Type", "Schema"))
    xs.foreach { entry =>
      tb.appendString(Vector(
        entry.path,
        entry.typeOption.getOrElse(""),
        entry.schemaOption.getOrElse("")
      ))
    }
    tb()
  }

  private def _field_table(entry: Entry): Table = {
    val tb = Table.Builder.headerString("Field", "Value")
    _flatten(entry.json).take(120).foreach {
      case (key, value) => tb.append(key, value)
    }
    tb()
  }

  private def _flatten(json: Json): Vector[(String, String)] =
    _flatten("", json)

  private def _flatten(prefix: String, json: Json): Vector[(String, String)] =
    json.asObject match {
      case Some(obj) =>
        obj.toVector.flatMap {
          case (key, value) => _flatten(_join_path(prefix, key), value)
        }
      case None =>
        json.asArray match {
          case Some(xs) =>
            if (xs.forall(x => x.asObject.isEmpty && x.asArray.isEmpty))
              Vector(prefix -> xs.map(_scalar).mkString(", "))
            else
              xs.zipWithIndex.toVector.flatMap {
                case (value, index) => _flatten(s"$prefix[$index]", value)
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
