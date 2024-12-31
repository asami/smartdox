package org.smartdox.structure

import org.goldenport.context.Conclusion
import org.smartdox._

/*
 * @since   Nov. 21, 2024
 * @version Nov. 24, 2024
 * @author  ASAMI, Tomoharu
 */
case class Statement(
  section: Section,
  description: Option[Description],
  statement: Definition
) extends Structure {
  def print = s"Statement(${statement.print})"
}

object Statement {
  case class Builder() extends Structure.BuilderBase {
    type S = Statement

    protected def build_Section(p: Section) = {
      val xs = p.sections
      if (xs.isEmpty) {
        val d = make_definition(p)
        Statement(p, None, d)
      } else {
        val (ds, df) = make_description_definition(p, "statement")
        Statement(p, Some(ds), df)
      }
    }
  }
}
