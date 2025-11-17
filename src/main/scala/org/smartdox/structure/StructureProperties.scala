package org.smartdox.structure

import org.goldenport.RAISE
import org.smartdox._
import org.smartdox.structure.StructureObject.KeyContent

/*
 * @since   Aug. 29, 2025
 *  version Aug. 31, 2025
 *  version Sep.  1, 2025
 * @version Nov. 17, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class StructureProperties() {
  def +(rhs: StructureProperties): StructureProperties
  def getAsI18NFragment(key: String): Option[I18NFragment]
  def getAsI18NValue(key: String): Option[Value]
}

object StructureProperties {
  val empty: StructureProperties = Empty

  case object Empty extends StructureProperties {
    def +(rhs: StructureProperties): StructureProperties = rhs

    def getAsI18NFragment(key: String): Option[I18NFragment] = None

    def getAsI18NValue(key: String): Option[Value] = None
  }

  case class Plain(xs: Map[String, List[Dox]] = Map.empty) extends StructureProperties {
    def +(rhs: StructureProperties): StructureProperties = rhs match {
      case Empty => this
      case m: Plain => copy(xs = xs ++ m.xs)
      case m: Compound => RAISE.notImplementedYetDefect
      case m => Compound(Vector(this, rhs))
    }

    def getAsI18NFragment(key: String): Option[I18NFragment] =
      xs.get(key).map(I18NFragment.create)

    def getAsI18NValue(key: String): Option[Value] =
      xs.get(key).map(Value.create)
  }

  case class Multi(xs: Map[String, List[List[Dox]]] = Map.empty) extends StructureProperties {
    def +(rhs: StructureProperties): StructureProperties = rhs match {
      case Empty => this
      case m: Multi => copy(xs = xs ++ m.xs)
      case m: Compound => RAISE.notImplementedYetDefect
      case m => Compound(Vector(this, rhs))
    }

    def getAsI18NFragment(key: String): Option[I18NFragment] =
      xs.get(key).map(I18NFragment.createList)

    def getAsI18NValue(key: String): Option[Value] =
      xs.get(key).map(Value.createMulti)
  }

  case class Compound(props: Vector[StructureProperties] = Vector.empty) extends StructureProperties {
    def +(rhs: StructureProperties): StructureProperties = rhs match {
      case Empty => this
      case m: Compound => copy(props = props ++ m.props)
      case m => copy(props = props :+ m)
    }

    def getAsI18NFragment(key: String): Option[I18NFragment] =
      props.toStream.flatMap(_.getAsI18NFragment(key)).headOption

    def getAsI18NValue(key: String): Option[Value] =
      props.toStream.flatMap(_.getAsI18NValue(key)).headOption
  }

  case class StructurePropertyProperties(
    props: Vector[StructureProperty] = Vector.empty
  ) extends StructureProperties {
    def +(rhs: StructureProperties): StructureProperties = rhs match {
      case Empty => this
      case m: StructurePropertyProperties => RAISE.notImplementedYetDefect
      case m: Compound => RAISE.notImplementedYetDefect
      case m => Compound(Vector(this, rhs))
    }

    def getAsI18NFragment(key: String): Option[I18NFragment] =
      props.find(_.isKey(key)).flatMap(_.getI18NFragment)

    def getAsI18NValue(key: String): Option[Value] =
      props.find(_.isKey(key)).flatMap(_.getI18NValue)
  }

  case class Builder(
    xs: Vector[(String, List[Dox])] = Vector.empty
  ) {
    def build(): StructureProperties = Plain(xs.toMap)

    def +(rhs: TRecord): Builder = {
      val fs = rhs.fields
      val key: String = fs(0).text
      val value: List[Dox] = fs(1).contents
      copy(xs = xs :+ (key -> value))
    }
  }

  def create(ps: Seq[(String, List[Dox])]): StructureProperties =
    if (ps.isEmpty)
      Empty
    else
      Plain(ps.toMap)

  def createI18NFragment(ps: Vector[KeyContent[I18NFragment]]): StructureProperties =
    createI18NFragment(ps.map(_.toTuple))

  def createI18NFragment(ps: Seq[(String, I18NFragment)]): StructureProperties =
    create(ps map {
      case (k, v) => k -> List(v)
    })

  def createValues(ps: Seq[(String, Seq[List[Dox]])]): StructureProperties =
    if (ps.isEmpty) {
      Empty
    } else {
      val a = ps.toVector.map {
        case (k, v) => k -> v.toList
      }
      Multi(a.toMap)
    }

  def createValue(ps: Vector[KeyContent[Value]]): StructureProperties =
    createValue(ps.map(_.toTuple))

  def createValue(ps: Seq[(String, Value)]): StructureProperties =
    if (ps.isEmpty) {
      Empty
    } else {
      val a = ps.toVector.map {
        case (k, v) => k -> List(v)
      }
      Plain(a.toMap)
    // val a = ps.toVector.map {
    //   case (k, v) => k -> v.toInlineContentsList
    // }
    // Multi(a.toMap)
    }

  def createLists(ps: Vector[KeyContent[List[KeyContent[Section]]]]): StructureProperties = {
    val a = ps.map(ListI18NFragmentPropertyProperty.create)
    StructurePropertyProperties(a)
  }
}
