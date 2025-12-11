package org.smartdox.structure

import org.smartdox._
import org.smartdox.structure.StructureObject.KeyContent

/*
 * @since   Nov. 16, 2025
 *  version Nov. 17, 2025
 * @version Dec. 11, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait StructureProperty {
  type T

  def key: Key
  def content: T

  def isKey(p: String) = key.value == p

  def getI18NFragment: Option[I18NFragment] = None
  def getI18NValue: Option[Value] = None
}

case class Key(value: String) {
  def isMatch(key: String): Boolean = value == key
}

case class ValueProperty(
  key: Key,
  content: Value
) extends StructureProperty {
  type T = Value

  override def getI18NValue: Option[Value] = Some(content)
}
object ValueProperty {
  def create(p: KeyContent[Value]): ValueProperty = ValueProperty(p.key, p.content)
}

case class ValueListProperty(
  key: Key,
  content: List[Value],
  description: Vector[Dox]
) extends StructureProperty {
  type T = List[Value]
}
object ValueListProperty {
  def create(p: KeyContent[List[Value]]): ValueListProperty = ValueListProperty(p.key, p.content, p.description)
}

case class I18NFragmentProperty(
  key: Key,
  content: I18NFragment
) extends StructureProperty {
  type T = I18NFragment

  override def getI18NFragment: Option[I18NFragment] = Some(content)
}
object I18NFragmentProperty {
  def create(p: KeyContent[Section]): I18NFragmentProperty = {
    val a = I18NFragment.create(p.content.contents)
    I18NFragmentProperty(p.key, a)
  }
}

// case class ListDoxProperty(
//   key: Key,
//   content: List[Dox]
// ) extends StructureProperty {
//   type T = List[Dox]
// }

case class ValuePropertyListProperty (
  key: Key,
  content: List[ValueProperty]
) extends StructureProperty {
  type T = List[ValueProperty]
}
object ValuePropertyListProperty {
  def create(p: Section): ValuePropertyListProperty = {
    val a = _to_keycontent(p)
    create(a)
  }

  private def _to_keycontent(p: Section): KeyContent[List[KeyContent[Value]]] = {
    val key = p.keyForModel
    val xs = p.sections.map(_.makeKeyContentValue)
    KeyContent(key, xs)
  }

  def create(p: KeyContent[List[KeyContent[Value]]]): ValuePropertyListProperty =
    ValuePropertyListProperty(p.key, p.content.map(ValueProperty.create))
}

case class ValueListPropertyListProperty (
  key: Key,
  content: List[ValueListProperty]
) extends StructureProperty {
  type T = List[ValueListProperty]
}
object ValueListPropertyListProperty {
  def create(p: Section): ValueListPropertyListProperty = {
    val a = _to_keycontent(p)
    create(a)
  }

  private def _to_keycontent(p: Section): KeyContent[List[KeyContent[List[Value]]]] = {
    val key = p.keyForModel
    val xs = p.sections.map(_.makeKeyContentValueList)
    KeyContent(key, xs)
  }

  def create(p: KeyContent[List[KeyContent[List[Value]]]]): ValueListPropertyListProperty =
    ValueListPropertyListProperty(p.key, p.content.map(ValueListProperty.create))
}

case class ListI18NFragmentPropertyProperty (
  key: Key,
  content: List[I18NFragmentProperty]
) extends StructureProperty {
  type T = List[I18NFragmentProperty]
}
object ListI18NFragmentPropertyProperty {
  def create(p: KeyContent[List[KeyContent[Section]]]): ListI18NFragmentPropertyProperty = {
    val a = p.content.map(I18NFragmentProperty.create)
    ListI18NFragmentPropertyProperty(p.key, a)
  }
}
