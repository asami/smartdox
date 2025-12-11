package org.smartdox.structure

import scalaz.{Value => _, _}, Scalaz._
import org.goldenport.context.Consequence
import org.smartdox._
import org.smartdox.metadata.Explanation

/*
 * @since   Aug. 29, 2025
 *  version Aug. 31, 2025
 *  version Sep.  1, 2025
 *  version Nov. 17, 2025
 * @version Dec. 11, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class StructureObject() extends Structure {
  def title: StructureObject.Title
  def getAsI18NFragment(key: String): Option[I18NFragment]
  def getAsI18NValue(key: String): Option[Value]
  def contents: List[Dox]
  def +(rhs: StructureObject): StructureObject

  def distillI18NFragmentPropertyList: List[I18NFragmentProperty] =
    distillI18NFragmentPropertyList(_ => true)
  def distillI18NFragmentPropertyList(key: String): List[I18NFragmentProperty] =
    distillI18NFragmentPropertyList(_.key.isMatch(key))
  def distillI18NFragmentPropertyList(matcher: ListI18NFragmentPropertyProperty => Boolean): List[I18NFragmentProperty] = Nil

  def distillValueListPropertyListProperty: List[ValueListPropertyListProperty] =
    distillValueListPropertyListProperty(_ => true)
  def distillValueListPropertyListProperty(key: String): List[ValueListPropertyListProperty] =
    distillValueListPropertyListProperty(_.key.isMatch(key))
  def distillValueListPropertyListProperty(
    matcher: ValueListPropertyListProperty => Boolean
  ): List[ValueListPropertyListProperty] = Nil
}

case class DocumentStructureObject(
  doc: Document,
  ingredients: StructureObject.Ingredients
) extends StructureObject with StructureObject.Ingredients.Holder {
  def +(rhs: StructureObject): StructureObject =
    CompoundStructureObject(Vector(this, rhs))

  def print = "DocumentStructureObject"

  override def distillI18NFragmentPropertyList(matcher: ListI18NFragmentPropertyProperty => Boolean): List[I18NFragmentProperty] =
    distill_I18NFragmentProperty_list(matcher)

  override def distillValueListPropertyListProperty(
    matcher: ValueListPropertyListProperty => Boolean
  ): List[ValueListPropertyListProperty] =
    distill_ValueListPropertyListProperty_list(matcher)
}

case class SectionStructureObject(
  secion: Section,
  ingredients: StructureObject.Ingredients
) extends StructureObject with StructureObject.Ingredients.Holder {
  def +(rhs: StructureObject): StructureObject =
    CompoundStructureObject(Vector(this, rhs))

  def print = "SectionStructureObject"

  override def distillI18NFragmentPropertyList(
    matcher: ListI18NFragmentPropertyProperty => Boolean
  ): List[I18NFragmentProperty] =
    distill_I18NFragmentProperty_list(matcher)

  override def distillValueListPropertyListProperty(
    matcher: ValueListPropertyListProperty => Boolean
  ): List[ValueListPropertyListProperty] =
    distill_ValueListPropertyListProperty_list(matcher)
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

      protected def distill_I18NFragmentProperty_list: List[I18NFragmentProperty] =
        distill_I18NFragmentProperty_list(_ => true)

      protected def distill_I18NFragmentProperty_list(key: String): List[I18NFragmentProperty] =
        distill_I18NFragmentProperty_list(_.key.isMatch(key))

      protected def distill_I18NFragmentProperty_list(matcher: ListI18NFragmentPropertyProperty => Boolean): List[I18NFragmentProperty] =
        properties.distillI18NFragmentPropertyList(matcher)

      protected def distill_ValueListPropertyListProperty_list: List[ValueListPropertyListProperty] =
        distill_ValueListPropertyListProperty_list(_ => true)

      protected def distill_ValueListPropertyListProperty_list(key: String): List[ValueListPropertyListProperty] =
        distill_ValueListPropertyListProperty_list(_.key.isMatch(key))

      protected def distill_ValueListPropertyListProperty_list(
        matcher: ValueListPropertyListProperty => Boolean
      ): List[ValueListPropertyListProperty] =
        properties.distillValueListPropertyListProperty(matcher)
    }
  }

  case class Title(contents: I18NFragment)

  case class KeyContent[T](
    key: Key,
    content: T,
    description: Vector[Dox] = Vector.empty
  ) {
    def toTuple: (String, T) = (key.value, content)
  }
  object KeyContent {
    def apply[T](key: String, content: T): KeyContent[T] = KeyContent(Key(key), content)
  }

  class Builder(config: Builder.Config) {
    case class Progress(
      isDone: Boolean = false,
      init: Vector[Dox] = Vector.empty,
      target: Option[Either[Table, Dl]] = None,
      values: Vector[Section] = Vector.empty,
      descriptions: Vector[Section] = Vector.empty,
      lists: Vector[Section] = Vector.empty,
      valuePropertyListProperty: Vector[Section] = Vector.empty,
      valueListPropertyListProperty: Vector[Section] = Vector.empty,
      tail: Vector[Dox] = Vector.empty
    ) {
      def +(rhs: Dox) = if (isDone)
        copy(tail = tail :+ rhs)
      else
        rhs match {
          case m: Section =>
            if (config.isSectionValue(m))
              copy(values = values :+ m)
            else if (config.isSectionDescription(m))
              copy(descriptions = descriptions :+ m)
            else if (config.isSectionList(m))
              copy(lists = lists :+ m)
            else if (config.isListValuePropertyProperty(m))
              copy(valuePropertyListProperty = valuePropertyListProperty :+ m)
            else if (config.isListValueListPropertyProperty(m))
              copy(valueListPropertyListProperty = valueListPropertyListProperty :+ m)
            else
              copy(isDone = true, tail = tail :+ rhs)
          case m: Table => copy(isDone = true, target = Some(Left(m)))
          case m: Dl => copy(isDone = true, target = Some(Right(m)))
          case m => copy(init = init :+ m)
        }
    }
    object Progress {
      lazy val empty = Progress()

      trait Holder {
        def progress: Progress

        def isDone = progress.isDone
        def init = progress.init
        def values = progress.values
        def descriptions = progress.descriptions
        def lists = progress.lists
        def tail = progress.tail
      }
    }

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

    private def _properties: Cursor.CS[StructureProperties] = State { s =>
      val prog = for {
        a <- _properties_table_dl
        b <- _properties_section_ul_text
        c <- _properties_section_sections
        d <- _properties_progress
      } yield a + b + c + d
      prog.run(s)
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

    private def _properties_section_sections: Cursor.CS[StructureProperties] = State { s =>
      case class Z(
        progress: Progress = Progress.empty
      ) extends Progress.Holder {
        def r: (Cursor, StructureProperties) = {
          val props = _create_sections
          (Cursor(tail.toList), props)
        }

        private def _create_sections = {
          val xs = lists.map(Section.toKeySectionList)
          StructureProperties.createLists(xs)
        }

        def +(rhs: Dox) = copy(progress = progress + rhs)
      }
      s.xs.foldLeft(Z())(_+_).r
    }

    private def _properties_progress: Cursor.CS[StructureProperties] = State { s =>
      case class Z(
        progress: Progress = Progress.empty
      ) extends Progress.Holder {
        def r: (Cursor, StructureProperties) = {
          val a = _create_sections
          val b = _create_ValuePropertyListProperty
          val c = _create_ValueListPropertyListProperty
          val props = a + b + c
          (Cursor(tail.toList), props)
        }

        private def _create_sections = {
          val xs = progress.lists.map(Section.toKeySectionList)
          StructureProperties.createLists(xs)
        }

        private def _create_ValuePropertyListProperty = {
          val xs = progress.valuePropertyListProperty.map(ValuePropertyListProperty.create)
          StructureProperties.createStructureProperty(xs)
        }

        private def _create_ValueListPropertyListProperty = {
          val xs = progress.valueListPropertyListProperty.map(ValueListPropertyListProperty.create)
          StructureProperties.createStructureProperty(xs)
        }

        def +(rhs: Dox) = copy(progress = progress + rhs)
      }
      s.xs.foldLeft(Z())(_+_).r
    }

    // private def _properties_section_ValueListPropertyListProperty: Cursor.CS[StructureProperties] = State { s =>
    //   val prog = for {
    //     a <- _properties_section_sections_ul_value
    //   } yield a
    //   prog.run(s)
    // }

    // private def _properties_section_sections_ul_value: Cursor.CS[StructureProperties] = State { s =>
    // }

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
      (c, Nil) // TODO
    }

    private def _children_section: Cursor.CS[List[StructureObject]] = State { c =>
      case class Z(
        init: Vector[Dox] = Vector.empty,
        sections: Vector[Section] = Vector.empty,
        tail: Vector[Dox] = Vector.empty
      ) {
        def r = {
          val a = sections.map(build) // TODO use new Builder
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

      def isSectionList(p: Section) =
        schema.lists.exists(_.name == p.keyForModel)

      def isListValuePropertyProperty(p: Section) =
        schema.listValuePropertyProperty.exists(_.name == p.keyForModel)

      def isListValueListPropertyProperty(p: Section) =
        schema.listValueListPropertyProperty.exists(_.name == p.keyForModel)

      def isChildren = schema.nodes.exists(_.isChildren)
    }
    object Config {
      case class Schema(nodes: List[Schema.Node] = Nil) {
        import Schema._
        lazy val values = nodes.filter(_.isValue)
        lazy val descriptions = nodes.filter(_.isDescription)
        lazy val lists = nodes.filter(_.isSectionList)
        lazy val listValuePropertyProperty = nodes.filter(_.isListValuePropertyProperty)
        lazy val listValueListPropertyProperty = nodes.filter(_.isListValueListPropertyProperty)

        def addDescription(name: String) = copy(nodes = nodes :+ Node.Description(name))
      }
      object Schema {
        val empty = Schema()

        sealed trait Node {
          def name: String
          def isValue: Boolean = false
          def isDescription: Boolean = false
          def isSectionList: Boolean = false
          def isListValuePropertyProperty: Boolean = false
          def isListValueListPropertyProperty: Boolean = false
          def isChildren: Boolean = false
        }
        sealed trait Property extends Node
        sealed trait SectionListProperty extends Property {
          override def isSectionList: Boolean = true
        }
        sealed trait Children extends Node {
          override def isChildren: Boolean = true
        }
        object Node {
          case class Value(name: String) extends Property {
            override def isValue = true
          }
          case class Description(name: String) extends Property {
            override def isDescription = true
          }
          case class SectionLocalDateOrDateTimeSectionList(name: String) extends SectionListProperty
          case class List(name: String, childSchema: Schema) extends SectionListProperty

          case class SectionSectionListValueList(name: String) extends SectionListProperty

          def value(p: String): Node = Node.Value(p)
          def description(p: String): Node = Node.Description(p)
        }

        def apply(p: Node, ps: Node*): Schema =
          Schema((p +: ps).toList)

        def create(p: String, ps: String*): Schema =
          Schema((p +: ps).toList.map(Node.value))

        def create(values: Seq[String], descs: Seq[String]): Schema = {
          val vs = values.toList.map(Node.value)
          val ds = descs.toList.map(Node.description)
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
