package org.smartdox.metadata

import scalaz.{Ordering => _, _}, Scalaz._
import org.joda.time.LocalDate
import org.goldenport.i18n.I18NString
import org.goldenport.collection.VectorMap
import org.goldenport.util.LocalDateUtils.Implicits._
import org.smartdox.{Dox, I18NFragment}
import Notices.Notice

/*
 * @since   Aug. 23, 2025
 *  version Aug. 27, 2025
 *  version Sep.  3, 2025
 * @version Nov. 21, 2025
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

  def add(p: History, ps: History*): History = add(p +: ps)

  def add(ps: Seq[History]): History = {
    val xs = slots ++ ps.toVector.flatMap(_.slots)
    val a = xs.sorted
    History(a)
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
    case object Bibliography extends ContentKind {
      val title = I18NString("Bibliography", "参考文献")
    }
    case object Keyword extends ContentKind {
      val title = I18NString("Keyword", "キーワード")
    }
    case object Tag extends ContentKind {
      val title = I18NString("Tag", "タグ")
    }

    implicit val contentKindOrdering: Ordering[ContentKind] = Ordering.by {
      case Article  => 0
      case Glossary => 1
      case Keyword  => 2
      case Tag      => 3
      case Bibliography => 4
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

    implicit val eventKindOrdering: Ordering[EventKind] = Ordering.by {
      case Created => 0
      case Updated => 1
    }
  }

  case class Slot(
    eventKind: EventKind,
    date: LocalDate,
    contentKind: ContentKind,
    notice: Notice,
    description: Option[I18NFragment]
  ) {
    def year: Int = date.getYear
    def title: I18NString = notice.title
    def effectiveBrief: I18NFragment = description getOrElse I18NFragment.create(notice.effectiveBrief)
    def uri = notice.uri
    def category = notice.category
  }
  object Slot {
    implicit val slotOrdering: Ordering[Slot] = Ordering.Tuple3(
      Ordering[LocalDate].reverse,
      Ordering[EventKind],
      Ordering[ContentKind]
    ).on(s => (s.date, s.eventKind, s.contentKind))
  }

  case class HistoryCollection(
    slots: Vector[History.Slot] = Vector.empty
  ) {
    def desc: Vector[History.Slot] = slots.sorted // slots.sortBy(_.date)(ordering.reverse)

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
