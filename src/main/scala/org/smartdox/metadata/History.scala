package org.smartdox.metadata

import scalaz._, Scalaz._
import org.joda.time.LocalDate
import org.goldenport.i18n.I18NString
import org.goldenport.collection.VectorMap
import org.goldenport.util.LocalDateUtils.Implicits.ordering
import Notices.Notice

/*
 * @since   Aug. 23, 2025
 * @version Aug. 27, 2025
 * @author  ASAMI, Tomoharu
 */
case class History(
  slots: Vector[History.Slot] = Vector.empty
) {
  import History._

  def yearList: VectorMap[Int, HistoryCollection] = {
    case class Z(map: VectorMap[Int, HistoryCollection] = VectorMap.empty) {
      def r = map.sortKeyDesc

      def +(rhs: History.Slot) = copy(map = map |+| VectorMap(rhs.year -> HistoryCollection(rhs)))
    }
    slots.foldLeft(Z())(_+_).r
  }
}

object History {
  val empty = History()

  sealed trait ContentKind {
    def title: I18NString
  }
  object ContentKind {
    case object Article extends ContentKind {
      val title = I18NString("Article", "記事")
    }
    case object Glossary extends ContentKind {
      val title = I18NString("Glossary", "用語集")
    }
    case object Keyword extends ContentKind {
      val title = I18NString("Keyword", "キーワード")

    }
    case object Tag extends ContentKind {
      val title = I18NString("Tag", "タグ")
    }
  }

  sealed trait EventKind {
    def title: I18NString
  }
  object EventKind {
    case object Created extends EventKind {
      val title = I18NString("New", "新規")
    }
    case object Updated extends EventKind {
      val title = I18NString("Update", "更新")
    }
  }

  case class Slot(
    eventKind: EventKind,
    date: LocalDate,
    contentKind: ContentKind,
    notice: Notice
  ) {
    def year: Int = date.getYear
    def title: I18NString = notice.title
    def summary: I18NString = notice.summary
    def uri = notice.uri
    def category = notice.category
  }

  case class HistoryCollection(
    slots: Vector[History.Slot] = Vector.empty
  ) {
    def desc: Vector[History.Slot] = slots.sortBy(_.date)(ordering.reverse)

    def +(rhs: HistoryCollection) = copy(slots = slots ++ rhs.slots)
  }
  object HistoryCollection {
    val empty = HistoryCollection()

    implicit val HistoryCollectionMonoid: Monoid[HistoryCollection] = new Monoid[HistoryCollection] {
      def zero: HistoryCollection = HistoryCollection.empty
      def append(f1: HistoryCollection, f2: => HistoryCollection): HistoryCollection = f1 + f2
    }

    def apply(p: Slot): HistoryCollection = HistoryCollection(Vector(p))
  }
}
