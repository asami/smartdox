package org.smartdox

import java.io.File
import org.goldenport.i18n.I18NContext
import org.goldenport.realm.Realm
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.smartdox.doxsite.DoxSite
import org.smartdox.generator.Context

/*
 * @since   Jun. 25, 2026
 * @version Jun. 25, 2026
 * @author  ASAMI, Tomoharu
 */
trait SmartDoxSpecVocabulary extends Matchers {
  protected final def create_site(
    context: Context,
    source: File,
    config: DoxSite.Config = DoxSite.Config.default
  ): DoxSite =
    DoxSite.create(context, source, None, config)

  protected final def site_realm(
    site: DoxSite,
    context: Context
  ): Realm =
    site.toRealm(context)

  protected final def html_at(
    realm: Realm,
    paths: String*
  )(implicit context: I18NContext): String =
    paths.toStream.flatMap(realm.getString).head

  protected final def metadata_at(
    realm: Realm,
    path: String
  )(implicit context: I18NContext): String =
    realm.getString(path).get

  protected final def first_program_contents(document: Dox): String =
    document.asInstanceOf[Document].
      body.contents.head.asInstanceOf[Program].
      contents

  protected final def first_section_title(document: Dox): List[Dox] =
    _first_section(document).title

  protected final def first_list_item_contents(document: Dox): List[Dox] =
    _first_section(document).
      contents.head.asInstanceOf[Ul].
      contents.head.contents

  protected final def default_head_title(document: Dox): Option[String] =
    document.asInstanceOf[Document].head.distillTitleStringDefault

  protected final def localized_head_title(document: Dox)(implicit context: I18NContext): Option[String] =
    document.asInstanceOf[Document].head.distillTitleString

  protected final def include_text(expected: String): Matcher[String] = Matcher { actual =>
    MatchResult(
      actual.contains(expected),
      s"""String did not include "$expected"""",
      s"""String included "$expected""""
    )
  }

  protected final def include_html(expected: String): Matcher[String] =
    include_text(expected)

  protected final def include_metadata(expected: String): Matcher[String] =
    include_text(expected)

  protected final def contain_program_text(expected: String): Matcher[Dox] = Matcher { actual =>
    val content = first_program_contents(actual)
    MatchResult(
      content.contains(expected),
      s"""Program content did not include "$expected": $content""",
      s"""Program content included "$expected""""
    )
  }

  protected final def have_first_section_title_alternatives(expected: Int): Matcher[Dox] = Matcher { actual =>
    val size = first_section_title(actual).size
    MatchResult(
      size == expected,
      s"First section title alternative count was $size, not $expected",
      s"First section title alternative count was $expected"
    )
  }

  protected final def have_first_section_title_span: Matcher[Dox] = Matcher { actual =>
    val isspan = first_section_title(actual).headOption.exists(_.isInstanceOf[Span])
    MatchResult(
      isspan,
      "First section title head was not a Span",
      "First section title head was a Span"
    )
  }

  protected final def have_first_list_item_span: Matcher[Dox] = Matcher { actual =>
    val isspan = first_list_item_contents(actual).headOption.exists(_.isInstanceOf[Span])
    MatchResult(
      isspan,
      "First list item head was not a Span",
      "First list item head was a Span"
    )
  }

  protected final def have_default_head_title(expected: String): Matcher[Dox] = Matcher { actual =>
    val title = default_head_title(actual)
    MatchResult(
      title.contains(expected),
      s"""Default head title was $title, not "$expected"""",
      s"""Default head title was "$expected""""
    )
  }

  protected final def have_localized_head_title(
    expected: String
  )(implicit context: I18NContext): Matcher[Dox] = Matcher { actual =>
    val title = localized_head_title(actual)
    MatchResult(
      title.contains(expected),
      s"""Localized head title was $title, not "$expected"""",
      s"""Localized head title was "$expected""""
    )
  }

  private def _first_section(document: Dox): Section =
    document.asInstanceOf[Document].body.contents.head.asInstanceOf[Section]
}
