package org.smartdox.converters

import scalaz._, Scalaz._
import java.net.URI
import java.io.File
import org.goldenport.RAISE
import org.goldenport.tree._
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.LocaleUtils
import org.goldenport.util.ListUtils
import org.smartdox._
import org.smartdox.generators.AntoraGenerator
import org.smartdox.generators.KrokiGenerator
import org.smartdox.converter._

/*
 * @since   Apr. 18, 2025
 *  version Apr. 29, 2025
 *  version Jun. 20, 2025
 *  version Jul. 28, 2025
 *  version Aug. 31, 2025
 *  version Sep. 15, 2025
 *  version Oct. 26, 2025
 *  version Nov. 30, 2025
 * @version May. 14, 2026
 * @author  ASAMI, Tomoharu
 */
class Dox2AsciidocConverter(
  val context: Dox2AsciidocConverter.Context,
) extends Dox2TextDocConverter {
  import Dox2AsciidocConverter._

  protected override def is_ignore_img_in_figure = true
  protected def section_Mark = "="
  protected def unorderd_List_Mark = "*"
  protected def orderd_List_Mark = "."
  protected def definition_List_Term_Mark = ""
  protected def definition_List_Definition_Mark = "::"
  protected def horizontal_Rule_Mark: String = "pass:[<hr/>]"
  protected def bold_open = "*"
  protected def bold_close = "*"
  protected def italic_open = "_"
  protected def italic_close = "_"
  protected def bolditalic_open = "*_"
  protected def bolditalic_close = "_*"
  protected def code_open = "`"
  protected def code_close = "`"
  protected def target_locale = context.targetI18NContext.locale

  private def _is_ja = target_locale == LocaleUtils.ja

  private def _is_diagram_generation: Boolean = context.isDiagramGeneration

  private var _is_in_pass_count: Int = 0
  private def _is_in_pass = _is_in_pass_count > 0
  private var _document_depth: Int = 0
  private var _has_document_title: Boolean = false
  private var _body_section_depth: Int = 0
  private var _promoted_title_section_depth: Option[Int] = None
  private var _current_asciidoc_section_level: Int = 0
  private var _asciidoc_section_level_stack: List[Int] = Nil

  private lazy val _kroki_generator = KrokiGenerator(
    context.context.context,
    new File("kroki-cache.d"),
    false
  )

  override protected def normalize_Text_Dt(p: String) =
    if (is_autowire_dt)
      p
    else
      s"++$p++"

  override protected def is_space_required_in_stay(p: Dox): Boolean = p match {
    case _: Code => true
//    case _: Verbatim => true
    case _ => false
  }

  override protected def enter_Document(p: Document): Unit = {
    _document_depth = _document_depth + 1
  }

  override protected def leave_Document(p: Document): Unit = {
    _document_depth = _document_depth - 1
  }

  override protected def enter_Head(p: Head): Unit = {
    p.titleDefault match {
      case Nil => // do nothing
      case xs if _document_depth <= 1 && _current_asciidoc_section_level == 0 =>
        _has_document_title = true
        enter_asciidoc_section(p, to_text(xs))
      case xs =>
        val level = math.max(_current_asciidoc_section_level + 1, 2)
        _sb_asciidoc_section_title(level, to_text(xs))
    }
  }

  protected final def enter_asciidoc_section(head: Head, title: String): Unit = {
    val attachment = _make_title_attachment(head)
    _sb_asciidoc_section_title(1, title, attachment)
    _author(head)
  }

  private def _author(head: Head): Unit = {
    val (author, created, updated) = if (_is_ja) {
      val author = head.getAuthorString(LocaleUtils.ja)
      val created = head.getPublisedAtString(LocaleUtils.ja)
      val updated = head.getModefinedAtString(LocaleUtils.ja)
      (author, created, updated)
    } else {
      val author = head.getAuthorString(LocaleUtils.en)
      val created = head.getPublisedAtString(LocaleUtils.en)
      val updated = head.getModefinedAtString(LocaleUtils.en)
      (author, created, updated)
    }
    if (author.nonEmpty || created.nonEmpty || updated.nonEmpty) {
      sb_println("++++")
      sb_println("""<div class="doc-meta">""")
      sb_println("""<span class="doc-meta-bg">""")
      for (x <- author) {
        sb_println(s"""<span class="author meta-item">${x}</span>""")
      }
      for (x <- created) {
        sb_println(s"""<span class="created meta-item">Created: ${x}</span>""")
      }
      for (x <- updated) {
        sb_println(s"""<span class="updated meta-item">Updated: ${x}</span>""")
      }
      sb_println("""</span>""")
      sb_println("""</div>""")
      sb_println("++++")
      sb_println()
    }
  }

  protected final def leave_asciidoc_section(): Unit = {
  }

  override protected def enter_section(node: TreeNode[Dox], p: Section): Unit = {
    _body_section_depth = _body_section_depth + 1
    section_up()
    p.getClassName.foreach(x => sb_println(s"[.$x]"))
    val level = _asciidoc_section_level()
    _asciidoc_section_level_stack = _current_asciidoc_section_level :: _asciidoc_section_level_stack
    _current_asciidoc_section_level = level
    _sb_asciidoc_section_title(level, to_text(p.title))
  }

  override protected def leave_section(node: TreeNode[Dox], p: Section): Unit = {
    leave_Section(p)
    if (_promoted_title_section_depth.contains(_body_section_depth))
      _promoted_title_section_depth = None
    section_down()
    _body_section_depth = _body_section_depth - 1
    _current_asciidoc_section_level = _asciidoc_section_level_stack.headOption.getOrElse(0)
    _asciidoc_section_level_stack = _asciidoc_section_level_stack.drop(1)
  }

  private def _asciidoc_section_level(): Int =
    if (!_has_document_title && _body_section_depth == 1) {
      _has_document_title = true
      _promoted_title_section_depth = Some(_body_section_depth)
      1
    } else {
      _promoted_title_section_depth match {
        case Some(d) if _body_section_depth > d => _body_section_depth
        case _ => _body_section_depth + 1
      }
    }

  private def _sb_asciidoc_section_title(level: Int, title: String): Unit =
    _sb_asciidoc_section_title(level, title, Nil)

  private def _sb_asciidoc_section_title(level: Int, title: String, attachment: Seq[String]): Unit = {
    sb_println(s"${section_Mark * level} $title")
    for (s <- attachment)
      sb_println(s)
    sb_println()
  }

  private def _make_title_attachment(head: Head): Vector[String] = {
    val a = _make_title_attachment_locale(head)
    val b = _make_title_attachment_directive(head)
    a ++ b
  }

  private def _make_title_attachment_locale(head: Head): Vector[String] = {
    if (_is_ja) {
      val author = head.getAuthorString(LocaleUtils.ja)
      val created = head.getPublisedAtString(LocaleUtils.ja)
      val updated = head.getModefinedAtString(LocaleUtils.ja)
      Vector(
        ":lang: ja",
        ":table-caption: 表",
        ":figure-caption: 図",
        ":example-caption: 例",
        ":listing-caption: リスト"
      ) ++ Vector(
        author.map(x => s":author: $x"),
        created.map(x => s":created: $x"),
        updated.map(x => s":updated: $x")
      ).flatten
    } else {
      val author = head.getAuthorString(LocaleUtils.en)
      val created = head.getPublisedAtString(LocaleUtils.en)
      val updated = head.getModefinedAtString(LocaleUtils.en)
      Vector(
        ":lang: en"
      ) ++ Vector(
        author.map(x => s":author: $x"),
        created.map(x => s":created: $x"),
        updated.map(x => s":updated: $x")
      ).flatten
    }
  }

  private def _make_title_attachment_directive(head: Head): Vector[String] =
    Vector(
      head.metadata.isAutoWire.map { // currently not supported
        case true => s":autolink: on"
        case false => s":autolink: off"
      }
    ).flatten

  private def _make_title_attachment0(head: Head): Seq[String] = {
    Vector(_json_ld(head))
  }

  private def _json_ld(head: Head): String = {
    Vector(
      ":jsonld: {",
      """  "@context": "https://schema.org",""",
      """  "@type": "Article",""",
      """  "headline": "AI協調のためのBoK生成アーキテクチャ"""",
      "}"
    ).mkString
  }

  override protected def enter_Bold(p: Bold) = enter_Html_Element(p)
  override protected def leave_Bold(p: Bold) = leave_Html_Element(p)
  override protected def enter_Italic(p: Italic) = enter_Html_Element(p)
  override protected def leave_Italic(p: Italic) = leave_Html_Element(p)

  override protected def enter_Hyperlink(p: Hyperlink) =
    if (_is_in_pass)
      enter_Html_Element(p)
    else
      _enter_hyperlink(p)

  private def _enter_hyperlink(p: Hyperlink) = {
    sb_print(s"""link:${p.href.toString}[""")
    if (_use_quotation(p))
      sb_print("\"")
  }

  private def _use_quotation(p: Hyperlink) =
    p.title.isDefined || p.getHtmlClass.isDefined

  override protected def leave_Hyperlink(p: Hyperlink) =
    if (_is_in_pass)
      leave_Html_Element(p)
    else
      _leave_hyperlink(p)

    private def _leave_hyperlink(p: Hyperlink) = {
    if (_use_quotation(p))
      sb_print("\"")
    p.title foreach { x =>
      sb_print(""", title="""")
      sb_print(x.as(target_locale))
      sb_print("\"")
    }
    p.getHtmlClass foreach { x =>
      sb_print(""", role="""")
      sb_print(x)
      sb_print("\"")
    }
    sb_print("]")
  }

  override protected def enter_Figure(p: Figure): Unit = {
    val attrs = ListUtils.buildTupleList(
      List("role" -> "img-figure"),
      List(
        "alt" -> get_text(p.caption.contents),
        "title" -> get_text(p.caption.contents)
      )
    )
    sb_print(".")
    sb_print(p.caption.contents)
    sb_println("")
    sb_print("image::")
    if (false)
      sb_print("_")
    sb_print(_normalize_src(p.img.src))
    sb_print("[")
    sb_print(_build_attrs(attrs))
    sb_println("]")
    sb_println()
  }

  private def _normalize_src(p: URI): String = {
    val s = p.toString
    if (s.startsWith("images/"))
      s.substring("images/".length)
    else
      s
  }

  private def _build_attrs(ps: Seq[(String, String)]): String = 
    ps.map {
      case (k, v) => s"$k=$v" // TODO escape
    }.mkString(",")

  override protected def leave_Figure(p: Figure): Unit = {
  }

  override protected def enter_Table(p: Table): Unit = {
    val header = p.head.map { s =>
      val d = s"""[%autowidth, options="header"]"""
      sb_println(d)
      s.records
    } orElse {
      val d = s"""[%autowidth]"""
      sb_println(d)
      None
    }
    sb_println("|===")
    header.foreach(_print_records)
    _print_records(p.body.records)
    sb_println("|===")
  }

  private def _print_records(ps: List[TRecord]): Unit =
    ps.foreach(_print_record)

  private def _print_record(p: TRecord): Unit = {
//    val s = p.fields.map(_.text).mkString("|", "|", "")
//    sb_println(s)
    for (x <- p.fields) {
      sb_print("|")
      x.traverse(this)
    }
    sb_println()
  }

  override protected def enter_I18NFragment(p: I18NFragment): Unit = {
    val a = p.makeInlines
    if (a.nonEmpty) {
      for (x <- a)
        x.traverse(this)
//      sb_println()
//      sb_println()
    }
  }

  override protected def enter_program(node: TreeNode[Dox], p: Program): Unit =
    p.kind match {
      case Some(s) if _is_diagram_generation && use_kroki(s) =>
        done_traverse(node)
        _embed_diagram(s, p)
      case _ => _enter_program(p)
    }

  private def _enter_program(p: Program): Unit = {
    val caption = p.caption
    val kind = p.kind getOrElse "text"
    val directive = s"[source,$kind]" // _directive(kind)
    caption.foreach { x =>
      sb_print(".")
      sb_println(x)
    }
    sb_println(directive)
    sb_println("----")
  }

  // private def _directive(kind: String): String =
  //   if (_is_diagram_generation) {
  //     kind match {
  //       case m if (use_kroki(kind)) => s"[$kind,svg]"
  //       case _ => s"[source,$kind]"
  //     }
  //   } else {
  //     s"[source,text]"
  //   }

  protected def use_kroki(kind: String): Boolean = KrokiSource.contains(kind)

  /**
   * Embeds a Kroki diagram as inline SVG HTML.
   *
   * This version uses KrokiGenerator.generateSvgString to obtain the SVG
   * directly and embeds it into the AsciiDoc output. This avoids
   * filesystem path dependency and works inside Antora without asset copying.
   */
  private def _embed_diagram(kind: String, p: Program): Unit = {
    val caption = p.caption
    val source = p.contents

    try {
      // Generate SVG string from Kroki
      val svg = _kroki_generator.generateSvgString(kind, source).take

      // Print embedded HTML block
      sb_println("++++")
      sb_println("<div class=\"diagram\" role=\"diagram\">")
      caption.foreach { x =>
        sb_println(s"""<div class="diagram-caption">${x}</div>""")
      }
      sb_println(svg.trim)
      sb_println("</div>")
      sb_println("++++")
      sb_println()

    } catch {
      case e: Throwable =>
        context.context.context.log.error(s"Kroki embedding failed: ${e.getMessage}")
        sb_println("[WARNING.error]")
        sb_println("====")
        sb_println(e.toString)
        sb_println("====")
        sb_println()
        // Fallback to text source
        caption.foreach(x => sb_println("." + x))
        sb_println(s"[source,$kind]")
        sb_println("----")
        sb_print(p.contents)
        sb_println("----")
        sb_println()
    }
  }

  override protected def leave_Program(p: Program): Unit = {
    sb_println("----")
    for (c <- p.callouts.slots) {
      val s = c.content.distillString(target_locale)
      sb_println(s"<${c.num}> ${s}")
    }
  }

  override protected def enter_Code(p: Code): Unit = {
    p.kind match {
      case Some(s) =>
        val role = s match {
          case Code.Kind.Console => "filename"
        }
        sb_print("[.")
        sb_print(role)
        sb_print("]#")
      case None => super.enter_Code(p)
    }
  }

  override protected def leave_Code(p: Code): Unit = {
    p.kind match {
      case Some(s) => sb_print("#")
      case None => super.leave_Code(p)
    }
  }

  override protected def enter_Foot(p: Foot): Unit = {
  }

  override protected def enter_Html_Element(p: Dox): Unit = {
    if (_is_in_pass_count == 0)
      sb_print("pass:[")
    _is_in_pass_count = _is_in_pass_count + 1
    sb_print(p.showOpenText)
  }

  override protected def leave_Html_Element(p: Dox): Unit = {
    sb_print(p.showCloseText)
    _is_in_pass_count = _is_in_pass_count - 1
    if (_is_in_pass_count == 0)
      sb_print("]")
  }

  override protected def enter_Quotation_SimpleQuote(p: Quotation.SimpleQuote) = {
    sb_println("[quote]")
    sb_println("____")
  }

  override protected def leave_Quotation_SimpleQuote(p: Quotation.SimpleQuote) = {
    sb_println_if_required()
    sb_println("____")
  }
}

object Dox2AsciidocConverter {
  val KrokiSource = Vector("plantuml")

  case class Context(
    context: AntoraGenerator.Context,
    isDiagramGeneration: Boolean
  ) {
    def targetI18NContext = context.targetI18NContext
    def getDefaultAuthor: Option[I18NString] = context.getDefaultAuthor
  }
}
