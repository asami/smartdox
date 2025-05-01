package org.smartdox.structure

import org.goldenport.context.Conclusion
import org.smartdox._

/*
 * @since   Nov.  9, 2024
 *  version Nov. 22, 2024
 * @version Dec. 21, 2024
 * @author  ASAMI, Tomoharu
 */
case class DescriptionDefinitions(
  section: Section,
  description: Description,
  definitions: Vector[Definition]
) extends Structure {
  def print = "DescriptionDefinitions"
}

object DescriptionDefinitions {
  case class Builder() extends Structure.BuilderBase {
    type S = DescriptionDefinitions

    protected def build_Section(p: Section) = {
      val desc = Structure.makeDescription(p)
      val xs = p.sections
      val ds = Definition.makeDefinitions(xs)
      DescriptionDefinitions(p, desc, ds)
    }
  }
}
