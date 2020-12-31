package org.smartdox.builder

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox._

/*
 * @since   Nov. 23, 2020
 * @version Nov. 24, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxBuilderSpec extends WordSpec with Matchers with ScalazMatchers {
  "DoxBuilder" should {
    "empty" in {
      val b = DoxBuilder.create()
      val r = b.build()
      r should be(EmptyDox)
    }

    "text" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.addContent("OK")
      val r = b.build()
      r should be(Dox.text("OK"))
    }

    "section" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.enterDivision("Section")
      c.addContent("Content in section.")
      val r = b.build()
      r should be(Section("Section", Dox.text("Content in section.")))
    }

    "sections" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.enterDivision("Section")
      c.addContent("Content in section.")
      c.leaveDivision()
      c.enterDivision("Section 2")
      c.addContent("Content in section 2.")
      c.leaveDivision()
      val r = b.build()
      r should be(Fragment(
        Section("Section", Dox.text("Content in section.")),
        Section("Section 2", Dox.text("Content in section 2."))))
    }

    "section - subsection" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.enterDivision("Section")
      c.addContent("Content in section.")
      c.enterDivision("SubSection")
      c.addContent("Content in sub section 2.")
      c.leaveDivision()
      c.leaveDivision()
      val r = b.build()
      r should be(Section("Section", Dox.text("Content in section."), Section("SubSection", Dox.text("Content in sub section 2."))))
    }

    "section - subsections" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.enterDivision("Section")
      c.addContent("Content in section.")
      c.enterDivision("SubSection")
      c.addContent("Content in sub section.")
      c.leaveDivision()
      c.enterDivision("SubSection 2")
      c.addContent("Content in sub section 2.")
      c.leaveDivision()
      c.leaveDivision()
      val r = b.build()
      r should be(Section(
        "Section",
        Dox.text("Content in section."),
        Section("SubSection", Dox.text("Content in sub section.")),
        Section("SubSection 2", Dox.text("Content in sub section 2."))))
    }

    "sections - subsections" in {
      val b = DoxBuilder.create()
      val c = b.createCursor()
      c.enterDivision("Section")
      c.addContent("Content in section.")
      c.enterDivision("SubSection")
      c.addContent("Content in sub section.")
      c.leaveDivision()
      c.enterDivision("SubSection 2")
      c.addContent("Content in sub section 2.")
      c.leaveDivision()
      c.leaveDivision()
      c.enterDivision("Section 2")
      c.addContent("Content in section 2.")
      c.enterDivision("SubSection")
      c.addContent("Content in sub section.")
      c.leaveDivision()
      c.enterDivision("SubSection 2")
      c.addContent("Content in sub section 2.")
      c.leaveDivision()
      c.leaveDivision()
      val r = b.build()
      r should be(
        Fragment(
          Section(
            "Section",
            Dox.text("Content in section."),
            Section("SubSection", Dox.text("Content in sub section.")),
            Section("SubSection 2", Dox.text("Content in sub section 2."))),
          Section(
            "Section 2",
            Dox.text("Content in section 2."),
            Section("SubSection", Dox.text("Content in sub section.")),
            Section("SubSection 2", Dox.text("Content in sub section 2.")))))
    }
  }
}
