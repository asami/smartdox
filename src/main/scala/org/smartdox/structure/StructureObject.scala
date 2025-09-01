package org.smartdox.structure

import scalaz.{Value => _, _}, Scalaz._
import org.smartdox._
import org.smartdox.metadata.Explanation

/*
 * @since   Aug. 29, 2025
 *  version Aug. 31, 2025
 * @version Sep.  1, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class StructureObject() extends Structure {
  def title: StructureObject.Title
  def getAsI18NFragment(key: String): Option[I18NFragment]
  def getAsI18NValue(key: String): Option[Value]
  def contents: List[Dox]
  def +(rhs: StructureObject): StructureObject
}

case class DocumentStructureObject(
  doc: Document,
  ingredients: StructureObject.Ingredients
) extends StructureObject with StructureObject.Ingredients.Holder {
  def +(rhs: StructureObject): StructureObject =
    CompoundStructureObject(Vector(this, rhs))

  def print = "DocumentStructureObject"
}

case class SectionStructureObject(
  secion: Section,
  ingredients: StructureObject.Ingredients
) extends StructureObject with StructureObject.Ingredients.Holder {
  def +(rhs: StructureObject): StructureObject =
    CompoundStructureObject(Vector(this, rhs))

  def print = "SectionStructureObject"
}

case class CompoundStructureObject(
  objects: Vector[StructureObject]
) extends StructureObject {
  def title: StructureObject.Title = objects.head.title

  def contents = objects.foldMap(_.contents)

  def getAsI18NFragment(key: String): Option[I18NFragment] =
    objects.toStream.flatMap(_.getAsI18NFragment(key)).headOption

  def getAsI18NValue(key: String): Option[Value] =
    objects.toStream.flatMap(_.getAsI18NValue(key)).headOption

  def +(rhs: StructureObject): StructureObject = rhs match {
    case m: CompoundStructureObject => copy(objects = objects ++ m.objects)
    case m => copy(objects = objects :+ rhs)
  }

  def print = "ComoundStructureObject"
}

object StructureObject {
  case class Ingredients(
    title: Title,
    explanation: Explanation,
    properties: StructureProperties,
    children: List[StructureObject],
    contents: List[Dox]
  )
  object Ingredients {
    trait Holder {
      def ingredients: Ingredients
      def title: Title = ingredients.title
      def properties: StructureProperties = ingredients.properties
      def contents = ingredients.contents

      def getAsI18NFragment(key: String): Option[I18NFragment] =
        properties.getAsI18NFragment(key)

      def getAsI18NValue(key: String): Option[Value] =
        properties.getAsI18NValue(key)
    }
  }

  case class Title(contents: I18NFragment)

  class Builder(config: Builder.Config) {
    def build(p: Document): StructureObject = {
      val title = Title(p.head.title getOrElse I18NFragment.create("No title"))
      val explanation = p.head.metadata.explanation
      val cursor = Cursor(p.body.contents)
      val prog = for {
        properties <- _properties
        children <- _children
        contents <- Cursor.pop
      } yield {
        val a = Ingredients(title, explanation, properties, children, contents)
        DocumentStructureObject(p, a)
      }
      prog.eval(cursor)
    }

    def build(p: Section): StructureObject = {
      val title = Title(p.titleI18NFragment)
      val explanation = Explanation.empty
      val cursor = Cursor(p.contents)
      val prog = for {
        properties <- _properties
        children <- _children
        contents <- Cursor.pop
      } yield {
        val a = Ingredients(title, explanation, properties, children, contents)
        SectionStructureObject(p, a)
      }
      prog.eval(cursor)
    }

    private def _properties: Cursor.CS[StructureProperties] = State { c =>
      val prog = for {
        a <- _properties_table_dl
        b <- _properties_section_ul_text
      } yield a + b
      prog.run(c)
    }

    private def _properties_table_dl: Cursor.CS[StructureProperties] = State { c =>
      case class Z(
        isDone: Boolean = false,
        target: Option[Either[Table, Dl]] = None,
        init: Vector[Dox] = Vector.empty,
        tail: Vector[Dox] = Vector.empty
      ) {
        def r = {
          val r = target match {
            case Some(s) => s match {
              case Right(r) => _create(r)
              case Left(l) => _create(l)
            }
            case None => StructureProperties.empty
          }
          (Cursor(tail.toList), r)
        }

        private def _create(p: Table) = {
          val records = p.body.records
          records.foldLeft(StructureProperties.Builder())(_+_).build
        }

        private def _create(p: Dl) = {
          StructureProperties.empty
        }

        def +(rhs: Dox) =
          if (isDone)
            copy(tail = tail :+ rhs)
          else
            rhs match {
              case m: Section => copy(isDone = true, tail = tail:+ rhs)
              case m: Table => copy(isDone = true, target = Some(Left(m)))
              case m: Dl => copy(isDone = true, target = Some(Right(m)))
              case m => copy(init = init :+ m)
            }
      }
      c.xs.foldLeft(Z())(_+_).r
    }

    private def _properties_section_ul_text: Cursor.CS[StructureProperties] = State { c =>
      case class Z(
        isDone: Boolean = false,
        init: Vector[Dox] = Vector.empty,
        values: Vector[Section] = Vector.empty,
        descriptions: Vector[Section] = Vector.empty,
        tail: Vector[Dox] = Vector.empty
      ) {
        def r = {
          val props = _create_values + _create_descriptions
          (Cursor(tail.toList), props)
        }

        private def _create_values = {
          val xs = values.map(Section.toKeyValueOrValues)
          StructureProperties.createValue(xs)
        }

        private def _create_descriptions = {
          val xs = descriptions.map(Section.toKeyDescription)
          StructureProperties.createI18NFragment(xs)
        }

        def +(rhs: Dox) =
          if (isDone)
            copy(tail = tail :+ rhs)
          else
            rhs match {
              case m: Section =>
                if (config.isSectionValue(m))
                  copy(values = values :+ m)
                else if (config.isSectionDescription(m))
                  copy(descriptions = descriptions :+ m)
                else
                  copy(isDone = true, tail = tail :+ rhs)
              case m => copy(init = init :+ rhs)
            }
      }
      c.xs.foldLeft(Z())(_+_).r
    }

    private def _children: Cursor.CS[List[StructureObject]] = State { c =>
      if (config.isChildren) {
        val prog = for {
          a <- _children_table
          b <- _children_section
          r <- _merge(a, b)
        } yield r
        prog.run(c)
      } else {
        (c, Nil)
      }
    }

    private def _children_table: Cursor.CS[List[StructureObject]] = State { c =>
      (c, Nil)
    }

    private def _children_section: Cursor.CS[List[StructureObject]] = State { c =>
      case class Z(
        init: Vector[Dox] = Vector.empty,
        sections: Vector[Section] = Vector.empty,
        tail: Vector[Dox] = Vector.empty
      ) {
        def r = {
          val a = sections.map(build)
          (Cursor(tail.toList), a.toList)
        }

        def +(rhs: Dox) =
          rhs match {
            case m: Section => copy(sections = sections :+ m)
            case m =>
              if (sections.isEmpty)
                copy(init = init :+ rhs)
              else
                copy(tail = tail :+ rhs)
          }
      }
      c.xs.foldLeft(Z())(_+_).r
    }

    private def _merge(a: List[StructureObject], b: List[StructureObject]): Cursor.CS[List[StructureObject]] = State { c =>
      (c, a ++ b)
    }
  }
  object Builder {
    case class Config(
      schema: Config.Schema
    ) {
      // def isSectionProperty(p: Section) =
      //   schema.attributes.exists(_.name == p.keyForModel)

      def isSectionValue(p: Section) =
        schema.values.exists(_.name == p.keyForModel)

      def isSectionDescription(p: Section) =
        schema.descriptions.exists(_.name == p.keyForModel)

      def isChildren = false
    }
    object Config {
      case class Schema(attributes: List[Schema.Attribute]) {
        import Schema._
        lazy val values = attributes.filter(_.kind == Attribute.Kind.Value)
        lazy val descriptions = attributes.filter(_.kind == Attribute.Kind.Description)

        def addDescription(name: String) = copy(attributes = attributes :+ Attribute(name, Attribute.Kind.Description))
      }
      object Schema {
        case class Attribute(name: String, kind: Attribute.Kind)
        object Attribute {
          sealed trait Kind
          object Kind {
            case object Value extends Kind
            case object Description extends Kind
          }

          def value(p: String): Attribute = Attribute(p, Attribute.Kind.Value)
          def description(p: String): Attribute = Attribute(p, Attribute.Kind.Description)
        }

        def create(p: String, ps: String*): Schema =
          Schema((p +: ps).toList.map(Attribute.value))

        def create(values: Seq[String], descs: Seq[String]): Schema = {
          val vs = values.toList.map(Attribute.value)
          val ds = descs.toList.map(Attribute.description)
          Schema(vs ++ ds)
        }
      }
    }
  }

  case class Cursor(xs: List[Dox]) {
  }
  object Cursor {
    type CS[A] = State[Cursor, A]

    val peek: CS[Option[Dox]] =
      State { s => (s, s.xs.headOption) }

    val next: CS[Option[Dox]] =
      State { s =>
        s.xs match {
          case h :: t => (s.copy(xs = t), Some(h))
          case Nil    => (s, None)
        }
      }

    val pop: CS[List[Dox]] = State { c => (c.copy(Nil), c.xs) }

    def take(n: Int): CS[List[Dox]] =
      State { s =>
        val (ys, zs) = s.xs.splitAt(n)
        (s.copy(xs = zs), ys)
      }

    def takeWhile(p: Dox => Boolean): CS[List[Dox]] =
      State { s =>
        val (ys, zs) = s.xs.span(p)
        (s.copy(xs = zs), ys)
      }

    def dropWhile(p: Dox => Boolean): CS[Unit] =
      State.modify[Cursor](s => s.copy(xs = s.xs.dropWhile(p)))

    def unshift(ds: List[Dox]): CS[Unit] =
      State.modify[Cursor](s => s.copy(xs = ds ::: s.xs))
  }

  def create(config: Builder.Config, p: Document) =
    new Builder(config).build(p)
}
