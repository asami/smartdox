package org.smartdox.converters

import scalaz._
import java.io.File
import java.nio.file.{Files, StandardCopyOption}
import scala.util.control.NonFatal
import org.goldenport.context.Consequence
import org.goldenport.tree.TreeNode
import org.smartdox._
import org.smartdox.converter.Dox2TextDocConverter
import org.smartdox.generator.{Context => GeneratorContext}
import org.smartdox.generators.KrokiCache
import org.smartdox.generators.KrokiGenerator

/*
 * @since   Jun.  2, 2026
 * @version Jun.  3, 2026
 * @author  ASAMI, Tomoharu
 */
class Dox2LatexConverter(
  engine: Dox2LatexConverter.Engine = Dox2LatexConverter.Engine.LuaLatex,
  format: Dox2LatexConverter.Format = Dox2LatexConverter.Format.Standard,
  documentDate: Option[String] = None,
  affiliation: Option[String] = None,
  author: Option[String] = None,
  generatorContext: Option[GeneratorContext] = None,
  diagramDir: Option[File] = None,
  isDiagramGeneration: Boolean = false,
  diagramRenderer: Option[Dox2LatexConverter.DiagramRenderer] = None
) extends Dox2TextDocConverter {
  import Dox2LatexConverter._

  private var _listStack: List[String] = Nil
  private lazy val _krokiGenerator: Option[KrokiGenerator] =
    generatorContext.map { ctx =>
      val dir = diagramDir.getOrElse(new File("kroki-cache.d"))
      new KrokiGenerator(ctx, KrokiCache(dir, false, ctx))
    }

  override def convert(dox: Dox): Consequence[String] = Consequence {
    _preamble()
    Dox.toTree(dox).traverse(this)
    _postamble()
    sb_to_string()
  }

  protected def section_Mark: String = ""
  protected def unorderd_List_Mark: String = ""
  protected def orderd_List_Mark: String = ""
  protected def definition_List_Term_Mark: String = ""
  protected def definition_List_Definition_Mark: String = ""
  protected def horizontal_Rule_Mark: String = "\\par\\hrule\\par"
  protected def bold_open: String = "\\textbf{"
  protected def bold_close: String = "}"
  protected def italic_open: String = "\\emph{"
  protected def italic_close: String = "}"
  protected def bolditalic_open: String = "\\textbf{\\emph{"
  protected def bolditalic_close: String = "}}"
  protected def code_open: String = "\\texttt{"
  protected def code_close: String = "}"

  override protected def normalize_Text(p: String): String =
    _escape_latex(p)

  override protected def enter_head(node: TreeNode[Dox], p: Head): Unit = {
    p.titleDefault match {
      case Nil => // do nothing
      case xs =>
        val title = to_text(xs)
        format match {
          case Format.Standard => _standard_title(title)
          case Format.Business => _business_title(title, p)
        }
    }
    done_traverse(node)
  }

  override protected def enter_Section(p: Section): Unit = {
    val command = _section_command(section_bar("x").length)
    sb_println(s"\\$command{${_escape_latex(to_text(p.title))}}")
    sb_println()
  }

  override protected def enter_Ul(p: Ul): Unit =
    _begin_list("itemize")

  override protected def leave_Ul(p: Ul): Unit =
    _end_list()

  override protected def enter_Ol(p: Ol): Unit =
    _begin_list("enumerate")

  override protected def leave_Ol(p: Ol): Unit =
    _end_list()

  override protected def enter_Dl(p: Dl): Unit =
    _begin_list("description")

  override protected def leave_Dl(p: Dl): Unit =
    _end_list()

  override protected def enter_Li(p: Li): Unit =
    sb_print("\\item ")

  override protected def enter_Dt(p: Dt): Unit =
    sb_print("\\item[")

  override protected def leave_Dt(p: Dt): Unit =
    sb_print("] ")

  override protected def enter_Dd(p: Dd): Unit = {}

  override def leave_Paragraph(p: Paragraph): Unit = {
    sb_println()
    sb_println()
  }

  override protected def enter_Hyperlink(p: Hyperlink): Unit = {}

  override protected def leave_Hyperlink(p: Hyperlink): Unit = {}

  override protected def enter_I18NFragment(p: I18NFragment): Unit = {}

  override protected def leave_I18NFragment(p: I18NFragment): Unit = {}

  override protected def enter_program(node: TreeNode[Dox], p: Program): Unit = {
    p.kind match {
      case Some(kind) if isDiagramGeneration && use_kroki(kind) =>
        _enter_diagram(kind, p)
      case _ =>
        _enter_verbatim_program(p)
    }
    done_traverse(node)
  }

  private def _enter_verbatim_program(p: Program): Unit = {
    sb_println("\\begin{verbatim}")
    sb_print(p.contents)
    if (!p.contents.endsWith("\n"))
      sb_println()
    sb_println("\\end{verbatim}")
    sb_println()
  }

  private def _enter_diagram(kind: String, p: Program): Unit =
    try {
      _diagram_file(kind, p.contents) match {
        case Some(file) =>
          val includeFile = _diagram_include_file(file)
          p.caption.foreach(x => sb_println(s"\\noindent\\textbf{${_escape_latex(x)}}\\\\"))
          sb_println("\\begin{center}")
          sb_println(s"\\includegraphics[width=\\linewidth,keepaspectratio]{\\detokenize{${_diagram_path(includeFile)}}}")
          sb_println("\\end{center}")
          sb_println()
        case None =>
          _enter_verbatim_program(p)
      }
    } catch {
      case NonFatal(e) =>
        _enter_diagram_error(kind, p.contents, e)
    }

  private def _enter_diagram_error(kind: String, source: String, error: Throwable): Unit = {
    _enter_diagnostic(DiagnosticBlock.error(
      s"Diagram render error ($kind)",
      _compact_error_message(error),
      "Diagram source",
      source
    ))
  }

  override protected def enter_DiagnosticBlock(p: DiagnosticBlock): Unit = {
    _enter_diagnostic(p)
  }

  private def _enter_diagnostic(p: DiagnosticBlock): Unit = {
    sb_println(s"\\noindent\\textbf{${_escape_latex(p.title)}}")
    sb_println()
    sb_println("\\begin{verbatim}")
    sb_println(p.message)
    for (label <- p.sourceLabel; source <- p.source) {
      sb_println()
      sb_println(s"$label:")
      sb_print(source)
      if (!source.endsWith("\n"))
        sb_println()
    }
    sb_println("\\end{verbatim}")
    sb_println()
  }

  private def _compact_error_message(error: Throwable): String =
    _compact_repeated_lines(Option(error.getMessage).getOrElse(error.toString))

  private def _compact_repeated_lines(message: String): String = {
    val lines = message.linesIterator.toVector
    val compacted = lines.foldLeft(Vector.empty[(String, Int)]) {
      case (Vector(), line) => Vector(line -> 1)
      case (acc, line) =>
        val (last, count) = acc.last
        if (last == line)
          acc.init :+ (last -> (count + 1))
        else
          acc :+ (line -> 1)
    }
    compacted.map {
      case (line, 1) => line
      case (line, n) => s"$line (repeated $n times)"
    }.mkString("\n")
  }

  private def _diagram_include_file(file: File): File =
    diagramDir.map { dir =>
      if (!dir.exists)
        dir.mkdirs()
      val target = new File(dir, file.getName)
      val sourcePath = file.toPath.toAbsolutePath.normalize
      val targetPath = target.toPath.toAbsolutePath.normalize
      if (sourcePath != targetPath)
        Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING)
      target
    }.getOrElse(file)

  private def _diagram_path(file: File): String =
    diagramDir.flatMap(_relative_path(_, file)).getOrElse(file.getAbsolutePath)

  private def _relative_path(base: File, file: File): Option[String] = {
    val basePath = base.toPath.toAbsolutePath.normalize
    val filePath = file.toPath.toAbsolutePath.normalize
    if (filePath.startsWith(basePath))
      Some(basePath.relativize(filePath).toString)
    else
      None
  }

  private def _diagram_file(kind: String, source: String): Option[File] =
    diagramRenderer.map(_.render(kind, source, "png")) orElse {
      _krokiGenerator.map(_.generate(kind, source, "png").take)
    }

  protected def use_kroki(kind: String): Boolean =
    KrokiSource.contains(kind)

  override protected def enter_Table(p: Table): Unit = {
    val width = math.max(1, p.width)
    val columns = (1 to width).map(_ => "l").mkString("|", "|", "|")
    sb_println(s"\\begin{longtable}{$columns}")
    p.caption.foreach(c => sb_println(s"\\caption{${_escape_latex(c.toText)}}\\\\"))
    p.head.foreach(_rows(_, isHeader = true))
    _rows(p.body, isHeader = false)
    p.foot.foreach(_rows(_, isHeader = false))
    sb_println("\\end{longtable}")
    sb_println()
  }

  private def _preamble(): Unit = {
    engine match {
      case Engine.LuaLatex =>
        sb_println("\\documentclass[a4paper,11pt]{ltjsarticle}")
        sb_println("\\usepackage{luatexja}")
        _preamble_common(None)
      case Engine.UpLatex =>
        sb_println("\\documentclass[uplatex,a4j,11pt]{jsarticle}")
        sb_println("\\AtBeginDvi{\\special{papersize=210truemm,297truemm}}")
        _preamble_common(Some("dvipdfmx"))
    }
  }

  private def _preamble_common(driver: Option[String]): Unit = {
    val driverOption = driver.map(_ + ",").getOrElse("")
    sb_println(s"\\usepackage[${driverOption}a4paper,portrait,margin=25mm]{geometry}")
    sb_println("\\usepackage{longtable}")
    sb_println("\\usepackage{graphicx}")
    sb_println("\\begin{document}")
    sb_println()
  }

  private def _postamble(): Unit =
    sb_println("\\end{document}")

  private def _standard_title(title: String): Unit = {
    sb_println(s"\\title{${_escape_latex(title)}}")
    sb_println("\\date{}")
    sb_println("\\maketitle")
    sb_println()
  }

  private def _business_title(title: String, head: Head): Unit = {
    sb_println("\\begin{center}")
    sb_println(s"{\\Large\\bfseries ${_escape_latex(title)}}")
    sb_println("\\end{center}")
    sb_println("\\vspace{\\baselineskip}")
    val lines = Vector(
      documentDate.orElse(_metadata_date(head)).orElse(_non_empty(to_text(head.date))),
      _business_identity_line(head)
    ).flatten
    if (lines.nonEmpty) {
      sb_println("\\begin{flushright}")
      lines.init.foreach(x => sb_println(s"${_escape_latex(x)}\\\\"))
      sb_println(_escape_latex(lines.last))
      sb_println("\\end{flushright}")
      sb_println("\\vspace{\\baselineskip}")
    } else {
      sb_println("\\vspace{\\baselineskip}")
    }
    sb_println()
  }

  private def _business_identity_line(head: Head): Option[String] = {
    val xs = Vector(
      affiliation.orElse(_metadata_organization(head)),
      author.orElse(_metadata_author(head)).orElse(_non_empty(to_text(head.author)))
    ).flatten.map(_.trim).filter(_.nonEmpty)
    if (xs.isEmpty)
      None
    else
      Some(xs.mkString(" "))
  }

  private def _non_empty(p: String): Option[String] = {
    val s = p.trim
    if (s.isEmpty)
      None
    else
      Some(s)
  }

  private def _metadata_date(head: Head): Option[String] =
    head.metadata.getPublishedString(java.util.Locale.getDefault)

  private def _metadata_organization(head: Head): Option[String] =
    head.metadata.getOrganizationString(java.util.Locale.getDefault)

  private def _metadata_author(head: Head): Option[String] =
    head.metadata.getAuthorString(java.util.Locale.getDefault)

  private def _section_command(depth: Int): String =
    depth match {
      case n if n <= 1 => "section"
      case 2 => "subsection"
      case 3 => "subsubsection"
      case 4 => "paragraph"
      case _ => "subparagraph"
    }

  private def _begin_list(name: String): Unit = {
    _listStack = name :: _listStack
    sb_println(s"\\begin{$name}")
  }

  private def _end_list(): Unit = {
    val name = _listStack.headOption.getOrElse("itemize")
    sb_println(s"\\end{$name}")
    sb_println()
    _listStack = _listStack.drop(1)
  }

  private def _rows(p: TableCompartment, isHeader: Boolean): Unit =
    p.records.foreach(_row(_, isHeader))

  private def _row(p: TRecord, isHeader: Boolean): Unit = {
    val values = p.fields.map(x => _escape_latex(x.text))
    val rendered =
      if (isHeader)
        values.map(x => s"\\textbf{$x}")
      else
        values
    sb_println(rendered.mkString(" & ") + " \\\\")
    if (isHeader)
      sb_println("\\hline")
  }

  private def _escape_latex(p: String): String =
    p.flatMap {
      case '\\' => "\\textbackslash{}"
      case '{' => "\\{"
      case '}' => "\\}"
      case '$' => "\\$"
      case '&' => "\\&"
      case '%' => "\\%"
      case '#' => "\\#"
      case '_' => "\\_"
      case '^' => "\\textasciicircum{}"
      case '~' => "\\textasciitilde{}"
      case c => c.toString
    }
}

object Dox2LatexConverter {
  val KrokiSource = Vector("plantuml")

  trait DiagramRenderer {
    def render(kind: String, source: String, format: String): File
  }

  sealed trait Format {
    def name: String
  }

  object Format {
    case object Standard extends Format {
      val name = "standard"
    }

    case object Business extends Format {
      val name = "business"
    }

    def create(p: Option[String]): Format =
      p.map(_.trim.toLowerCase).filter(_.nonEmpty) match {
        case None | Some("standard") | Some("default") => Standard
        case Some("business") | Some("business-document") | Some("business-doc") => Business
        case Some(s) => sys.error(s"Unsupported LaTeX format: $s")
      }
  }

  sealed trait Engine {
    def name: String
  }

  object Engine {
    case object LuaLatex extends Engine {
      val name = "lualatex"
    }

    case object UpLatex extends Engine {
      val name = "uplatex"
    }

    def create(p: Option[String]): Engine =
      p.map(_.trim.toLowerCase).filter(_.nonEmpty) match {
        case None | Some("lua") | Some("lualatex") | Some("lua-latex") => LuaLatex
        case Some("up") | Some("uplatex") | Some("up-latex") => UpLatex
        case Some(s) => sys.error(s"Unsupported LaTeX engine: $s")
      }
  }
}
