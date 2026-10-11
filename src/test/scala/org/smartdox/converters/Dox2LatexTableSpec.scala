package org.smartdox.converters

import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import org.smartdox._
import org.smartdox.parser.UseDox2Parser

/*
 * @since   Oct. 11, 2026
 * @version Oct. 11, 2026
 * @author  ASAMI, Tomoharu
 */
class Dox2LatexTableSpec extends AnyWordSpec with Matchers with GivenWhenThen
    with ScalaCheckPropertyChecks with UseDox2Parser {
  private def _table(rows: List[List[String]]): Table =
    org.smartdox.Table(None, TBody(rows.map(row => TR(row.map(cell => TD(cell))))),
      None, None, None, None, None)

  private def _latex(dox: Dox): String = new Dox2LatexConverter().convert(dox).take

  private def _layout(columns: Int): String = {
    val dimension = s"p{\\dimexpr(\\linewidth-${2 * columns}\\tabcolsep-${columns + 1}\\arrayrulewidth)/$columns\\relax}"
    List.fill(columns)(dimension).mkString("|", "|", "|")
  }

  private def _occurrences(text: String, needle: String): Int =
    text.sliding(needle.length).count(_ == needle)

  "LaTeX table projection" should {
    "wrap four columns of authored Japanese and English prose without losing cells" in {
      Given("a parser-owned pipe table with complete breakable Japanese and English sentences")
      val cells = List(
        "JA1 長い説明を省略せずに読みやすく折り返し、表全体を本文の幅に収めます。",
        "EN2 Long descriptions wrap across lines while every complete cell remains within the document text area.",
        "JA3 すべての列に文章を入れ、元の説明と順序を保ちます。",
        "EN4 Each paragraph retains its full meaning and every word in the final column."
      )
      val input = "# Table prose\n\n#+CAPTION: Bilingual caption\n|----\n| A | B | C | D |\n|----\n" +
        cells.mkString("| ", " | ", " |\n") + "|----\n| EndA | EndB | EndC | EndD |\n|----\n"
      val dox = parse_dox(input)

      When("the authored table is projected to LaTeX")
      val latex = _latex(dox)

      Then("four bounded paragraph columns and every complete cell appear")
      latex should include (s"\\begin{longtable}{${_layout(4)}}")
      cells.foreach(cell => _occurrences(latex, cell) shouldBe 1)
      latex should include ("\\caption{Bilingual caption}")
      latex should include ("EndA & EndB & EndC & EndD")
    }

    "preserve escaped caption header body and footer once and in order" in {
      Given("distinct LaTeX-sensitive text in every table compartment")
      val table = _table(List(List("Body#", "Body%", "Body&", "Body_"))).copy(
        caption = Some(Caption("Caption#%&_")),
        head = Some(THead.create(List("Head#", "Head%", "Head&", "Head_"))),
        foot = Some(TFoot(List(TR(List(TD("Foot#"), TD("Foot%"), TD("Foot&"), TD("Foot_"))))))
      )

      When("the compartment-bearing table is projected")
      val latex = _latex(table)

      Then("escaping is preserved once with bold headers and original compartment order")
      val expected = List("Caption\\#\\%\\&\\_", "\\textbf{Head\\#}", "\\textbf{Head\\%}",
        "\\textbf{Head\\&}", "\\textbf{Head\\_}", "Body\\#", "Body\\%", "Body\\&", "Body\\_",
        "Foot\\#", "Foot\\%", "Foot\\&", "Foot\\_")
      expected.foreach(text => _occurrences(latex, text) shouldBe 1)
      val positions = expected.map(latex.indexOf)
      positions shouldBe positions.sorted
      latex should include ("\\hline")
      latex should include (s"\\begin{longtable}{${_layout(4)}}")
    }

    "retain both engine preambles and both title formats with wrapping tables" in {
      Given("a titled document containing a two-column table and all engine-format selections")
      val dox = parse_dox("Table title\n========\n\n|----\n| Left | Right |\n|----\n| Complete left prose | Complete right prose |\n|----\n")
      val selections = for {
        engine <- List(Dox2LatexConverter.Engine.LuaLatex, Dox2LatexConverter.Engine.UpLatex)
        format <- List(Dox2LatexConverter.Format.Standard, Dox2LatexConverter.Format.Business)
      } yield (engine, format)

      When("each selected engine and format projects the same document")
      val projections = selections.map { case (engine, format) =>
        (engine, format, new Dox2LatexConverter(engine = engine, format = format).convert(dox).take)
      }

      Then("each established preamble and title format accompanies bounded columns")
      projections.foreach { case (engine, format, latex) =>
        latex should include (s"\\begin{longtable}{${_layout(2)}}")
        latex should include ("\\usepackage{longtable}")
        latex should include ("Complete left prose & Complete right prose")
        engine match {
          case Dox2LatexConverter.Engine.LuaLatex =>
            latex should include ("\\documentclass[a4paper,11pt]{ltjsarticle}")
            latex should include ("\\usepackage{luatexja}")
            latex should include ("\\usepackage[a4paper,portrait,margin=25mm]{geometry}")
          case Dox2LatexConverter.Engine.UpLatex =>
            latex should include ("\\documentclass[uplatex,a4j,11pt]{jsarticle}")
            latex should include ("\\usepackage[dvipdfmx,a4paper,portrait,margin=25mm]{geometry}")
            latex should not include ("\\usepackage{luatexja}")
        }
        format match {
          case Dox2LatexConverter.Format.Standard => latex should include ("\\maketitle")
          case Dox2LatexConverter.Format.Business =>
            latex should include ("{\\Large\\bfseries Table title}")
            latex should not include ("\\maketitle")
        }
      }
    }

    "declare one usable column for an empty table" in {
      Given("a table with no header body rows or footer")
      val table = _table(Nil)

      When("the zero-width table is projected")
      val latex = _latex(table)

      Then("the longtable declares one paragraph column")
      table.width shouldBe 0
      latex should include (s"\\begin{longtable}{${_layout(1)}}")
      latex should include ("\\end{longtable}")
    }

    "take the column count from the widest header or footer" in {
      Given("tables whose header or footer is wider than the body")
      val tables = List(
        _table(List(List("Body"))).copy(head = Some(THead.create(List("H1", "H2", "H3", "H4")))),
        _table(List(List("Body"))).copy(foot = Some(TFoot(List(TR(List(TD("F1"), TD("F2"), TD("F3"), TD("F4"), TD("F5")))))))
      )

      When("both widest-compartment tables are projected")
      val projections = tables.map(_latex)

      Then("each layout accommodates its widest compartment without dropping body content")
      projections.head should include (s"\\begin{longtable}{${_layout(4)}}")
      projections.last should include (s"\\begin{longtable}{${_layout(5)}}")
      projections.foreach(latex => _occurrences(latex, "Body") shouldBe 1)
      projections.head should include ("\\textbf{H4}")
      projections.last should include ("F1 & F2 & F3 & F4 & F5")
    }

    "retain every cell and longtable row for a many-row table" in {
      Given("64 rows of four complete uniquely identified cell sentences")
      val rows = (1 to 64).map(row => (1 to 4).map(column =>
        s"R${row}C${column} Complete prose remains available for paragraph wrapping.").toList).toList
      val table = _table(rows)

      When("the many-row table is projected")
      val latex = _latex(table)

      Then("all cells and row terminators survive within longtable without claiming native pagination")
      latex should include (s"\\begin{longtable}{${_layout(4)}}")
      latex should include ("\\end{longtable}")
      rows.flatten.foreach(cell => _occurrences(latex, cell) shouldBe 1)
      latex.linesIterator.count(_.endsWith(" \\\\")) shouldBe 64
    }

    "preserve variable column and row counts with stable padding-aware dimensions" in {
      Given("ScalaCheck column counts from one to eight and row counts from one to twelve")
      val columncounts = Gen.choose(1, 8)
      val rowcounts = Gen.choose(1, 12)
      forAll(columncounts, rowcounts) { (columns, rowcount) =>
        Given("a generated rectangular table of complete uniquely identified cell prose")
        val rows = (1 to rowcount).map(row => (1 to columns).map(column =>
          s"R${row}C${column} Every generated word remains in its cell.").toList).toList
        val table = _table(rows)

        When("the generated table is projected to LaTeX")
        val latex = _latex(table)

        Then("the declared columns include padding and rules while all cells and rows remain complete")
        latex should include (s"\\begin{longtable}{${_layout(columns)}}")
        _occurrences(latex, "p{\\dimexpr") shouldBe columns
        rows.flatten.foreach(cell => _occurrences(latex, cell) shouldBe 1)
        latex.linesIterator.count(_.endsWith(" \\\\")) shouldBe rowcount
        latex should include ("\\end{longtable}")
      }
    }
  }
}
