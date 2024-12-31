package org.smartdox.structure

import com.typesafe.config.Config
import org.goldenport.context.Showable
import org.goldenport.context.Consequence
import org.goldenport.context.Conclusion
import org.goldenport.hocon.HoconUtils
import org.goldenport.values.Designation
import org.goldenport.xsv.Lxsv
import org.goldenport.xsv.LxsvSequence
import org.smartdox._

/*
 * @since   Nov.  9, 2024
 * @version Nov. 24, 2024
 * @author  ASAMI, Tomoharu
 */
trait Definition extends Showable {
}

object Definition {
  sealed trait DefinitionKind
  object DefinitionKind {
    case object TableKind extends DefinitionKind
    case object UlKind extends DefinitionKind
    case object OlKind extends DefinitionKind
    case object DlKind extends DefinitionKind
    case object PropertiesKind extends DefinitionKind
    case object DoxKind extends DefinitionKind
    case object TextKind extends DefinitionKind
  }

  case class Policy(
    kinds: Set[DefinitionKind] = Set(
      DefinitionKind.TableKind,
      DefinitionKind.DlKind,
      DefinitionKind.PropertiesKind,
      DefinitionKind.TextKind
    )
  )

  case class EmptyDefinition() extends Definition {
    def print = "EmptyDefinition"
  }

  case class TableDefinition(
    designation: Designation,
    table: Table
  ) extends Definition with Designation.Holder {
    def print = "TableDefinition"
  }
  object TableDefinition {
    def apply(name: String, p: Table): TableDefinition =
      TableDefinition(Designation(name), p)

    def apply(name: String, p: LxsvSequence): TableDefinition = {
      TableDefinition(Designation(name), Table.create(p))
    }

    def createLxsv(name: String, p: String): TableDefinition =
      apply(name, LxsvSequence.make(p))
  }

  case class UnorderedListDefinition(
    designation: Designation,
    ul: Ul
  ) extends Definition {
    def print = "UnorderedListDefinition"
  }
  object UnorderedListDefinition {
    def apply(name: String, p: Ul): UnorderedListDefinition =
      UnorderedListDefinition(Designation(name), p)
  }

  case class OrderedListDefinition(
    designation: Designation,
    ol: Ol
  ) extends Definition {
    def print = "OrderedListDefinition"
  }
  object OrderedListDefinition {
    def apply(name: String, p: Ol): OrderedListDefinition =
      OrderedListDefinition(Designation(name), p)
  }

  case class DescriptionListDefinition(
    designation: Designation,
    ul: Dl
  ) extends Definition with Designation.Holder {
    def print = "DescriptionListDefinition"
  }
  object DescriptionListDefinition {
    def apply(name: String, p: Dl): DescriptionListDefinition =
      DescriptionListDefinition(Designation(name), p)

    def apply(name: String, p: Seq[(String, String)]): DescriptionListDefinition =
      DescriptionListDefinition(Designation(name), Dl.create(p))
  }

  case class PropertiesDefinition(
    designation: Designation,
    config: Config
  ) extends Definition with Designation.Holder {
    def print = "PropertiesDefinition"
  }
  object PropertiesDefinition {
    def apply(name: String, p: Config): PropertiesDefinition =
      PropertiesDefinition(Designation(name), p)

    def createLxsv(name: String, p: String): PropertiesDefinition = {
      val lxsv = Lxsv.create(p)
      val c = HoconUtils.createHocon(lxsv.toStringStringVectorMap)
      PropertiesDefinition(Designation(name), c)
    }
  }

  case class DoxDefinition(
    designation: Designation,
    dox: Dox
  ) extends Definition with Designation.Holder {
    def print = "DoxDefinition"
  }
  object DoxDefinition {
    def apply(name: String, p: Dox): DoxDefinition =
      DoxDefinition(Designation(name), p)
  }

  case class TextDefinition(
    designation: Designation,
    text: String
  ) extends Definition with Designation.Holder {
    def print = "TextDefinition"
  }
  object TextDefinition {
    def apply(name: String, p: String): TextDefinition =
      TextDefinition(Designation(name), p)
  }

  case class ErrorDefinition(
    designation: Designation,
    conclusion: Conclusion
  ) extends Definition with Designation.Holder {
    def print = "Error"
  }
  object ErrorDefinition {
    def apply(name: String, p: Conclusion): ErrorDefinition =
      ErrorDefinition(Designation(name), p)
  }

  case class Builder(policy: Policy = Policy()) {
    protected def is_properties(p: String) =
      p.contains('=') || p.contains('{')

    protected def is_dox(p: Dox) = false

    protected def is_text(p: Dox) = false

    def makeDefinition(p: Section): Definition = getDefinition(p) getOrElse Definition.empty

    def makeDefinitions(ps: Seq[Section]): Vector[Definition] =
      ps.flatMap(getDefinition).toVector

    def getDefinition(p: Section): Option[Definition] = {
      getDefinitionByTable(p) orElse
      getDefinitionByUl(p) orElse
      getDefinitionByOl(p) orElse
      getDefinitionByDl(p) orElse
      getDefinitionByProperties(p) orElse
      getDefinitionByDox(p) orElse
      getDefinitionByText(p)
    }

    def getDefinitionByTable(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.TableKind)) {
        p.tables match {
          case Nil => None
          case x :: Nil => Some(TableDefinition(p.titleName, x))
          case x :: xs => Some(TableDefinition(p.titleName, x))
        }
      } else {
        None
      }

    def getDefinitionByUl(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.UlKind)) {
        p.uls match {
          case Nil => None
          case x :: Nil => Some(UnorderedListDefinition(p.titleName, x))
          case x :: xs => Some(UnorderedListDefinition(p.titleName, x))
        }
      } else {
        None
      }

    def getDefinitionByOl(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.OlKind)) {
        p.ols match {
          case Nil => None
          case x :: Nil => Some(OrderedListDefinition(p.titleName, x))
          case x :: xs => Some(OrderedListDefinition(p.titleName, x))
        }
      } else {
        None
      }

    def getDefinitionByDl(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.DlKind)) {
        p.dls match {
          case Nil => None
          case x :: Nil => Some(DescriptionListDefinition(p.titleName, x))
          case x :: xs => Some(DescriptionListDefinition(p.titleName, x))
        }
      } else {
        None
      }

    def getDefinitionByProperties(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.PropertiesKind)) {
        val r = Consequence.run {
          val s = p.toData
          if (is_properties(s))
            for {
              c <- HoconUtils.parseConfig(s)
            } yield Some(PropertiesDefinition(p.titleName, c))
          else
            Consequence.success(None)
        }
        r match {
          case Consequence.Success(s, _) => s
          case Consequence.Error(c) => Some(ErrorDefinition(p.titleName, c))
        }
      } else {
        None
      }

    def getDefinitionByDox(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.DoxKind)) {
        if (is_dox(p))
          Some(DoxDefinition(p.titleName, p))
        else
          None
      } else {
        None
      }

    def getDefinitionByText(p: Section): Option[Definition] =
      if (policy.kinds.contains(DefinitionKind.TextKind)) {
        val s = p.getStringIfOnlyText
        s.map(TextDefinition(p.titleName, _))
      } else {
        None
      }
  }

  val empty: Definition = EmptyDefinition()

  def makeDefinitions(ps: Seq[Section]): Vector[Definition] =
    Builder().makeDefinitions(ps)

  def makeDefinition(p: Section): Definition =
    Builder().makeDefinition(p)
}
