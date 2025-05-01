package org.smartdox.structure

import scalaz._, Scalaz._
import org.goldenport.context.Conclusion
import org.goldenport.context.Showable
import org.smartdox._

/*
 * @since   Nov.  8, 2024
 * @version Nov. 22, 2024
 * @author  ASAMI, Tomoharu
 */
trait Structure extends Showable {
}

object Structure {
  def makeDescription(p: Dox): Description = Description.parse(p)

  trait BuilderBase extends Doxes {
    type S

    def build(dox: Dox): S = dox match {
      case m: Section => build_Section(m)
      case m: Document => build(m.body)
      case m: Body => m.sections.headOption.map(build) getOrElse {
        Conclusion.unsupportedOperationFault(s"No section in document: $m").RAISE
      }
      case m => Conclusion.unsupportedOperationFault(s"Not section: $m").RAISE
    }

    protected def build_Section(p: Section): S

    protected final def make_definition(p: Section): Definition = 
      Definition.makeDefinition(p)

    protected final def make_description_definition(p: Section, key: String): (Description, Definition) = {
      val ds = make_description(p)
      val xs = p.sections
      val df = xs.find(_.keyForModel == key).fold(Definition.empty)(x =>
        make_definition(x)
      )
      (ds, df)
    }
  }
}
