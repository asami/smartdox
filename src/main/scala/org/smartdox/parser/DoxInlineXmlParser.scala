package org.smartdox.parser

import org.goldenport.RAISE
import org.goldenport.parser._
import org.smartdox._
import DoxInlineParser.{
  ChildDoxInlineParseState,
  Config,
  DoxInlineParseState,
  InlineState,
  RawFeature,
  SkipOneState,
  SkipSpaceStartState,
  SkipSpaceState
}

/*
 * @since   Aug. 30, 2026
 * @version Aug. 30, 2026
 * @author  ASAMI, Tomoharu
 */
object DoxInlineXmlParser {
  case class XmlState(
    config: Config,
    parent: DoxInlineParseState,
    tagName: String,
    attrs: Vector[(String, String)],
    cs: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    private val _ctx = Dox2Parser.ParseContext.now() // TODO
    implicit def dtctx = _ctx.dateTimeContext

    override def returnFrom(doxes: Seq[Dox]): DoxInlineParseState = {
      // Integrate parsed child Dox elements into this tag and return to the parent
      val element = _create_dox(doxes)
      leave_to(element)
    }

    override def returnEndResult: ParseResult[Dox] = {
      // Handle unclosed tag by finalizing current content
      val xs = parseInlineContents(cs.mkString)
      val dox = _create_dox(xs)
      ParseSuccess(dox)
    }

    override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val trimmed = s.trim
      if (trimmed.startsWith("</")) {
        val tag = trimmed.drop(2).takeWhile(_ != '>').trim
        if (tag == tagName) {
          // Matched closing tag: finalize current element and return to parent
          val xs = parseInlineContents(cs.mkString)
          val dox = _create_dox(xs)
          leave_to(dox)
        } else {
          // Not this tag: propagate to higher parent
          parent.returnCharsFrom(p)
        }
      } else {
        // Continue accumulating inner content
        copy(cs = cs ++ p)
      }
    }

    private def _return_chars_from_block(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val c = Dox2Parser.Config.smartdox.withInlineConfig(config).withoutComplementParagraph()
      val xs = Dox2Parser.parseFragment(c, s)
      val dox = _create_dox(xs.contents)
      leave_to(dox)
    }

    private def _return_chars_from_inline(p: Seq[Char]): DoxInlineParseState = {
      val s = (cs ++ p).mkString
      val dox = _create_dox(parseInlineContents(s))
      leave_to(dox)
    }

    private[parser] def _create_dox(doxes: Seq[Dox]): Dox =
      Dox.attachLocation(Dox.create(tagName, attrs, doxes), config.location)

    def parseInlineContents(s: String): List[Dox] =
      _normalize(DoxInlineParser.parse(config, s))

    private def _normalize(p: Dox): List[Dox] = p match {
      case m: Paragraph => m.contents // TODO
      case m: Fragment => m.contents
      case m: Div => m.contents
      case m => List(m)
    }

    override protected def use_angle_bracket = true

    override protected def end_Result(): ParseResult[Dox] =
      leave_end

    override protected def character_State(c: Char) = copy(cs = cs :+ c)

    override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
      if (XmlState._is_in_escape(cs))
        character_State(evt)
      else
        evt.next match {
          case Some('/') =>
            // closing tag of this element
            SkipOneState(XmlState.XmlCloseState(config, this), '/')
          case _ =>
            // open nested tag: create nested XmlState via TagOpenState
            XmlState.TagOpenState(config, this)
        }
    }

    override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
      if (XmlState._is_in_escape(cs))
        character_State(evt)
      else
        super.close_Angle_Bracket_State(evt)
  }
  object XmlState {
    case class XmlCloseState(
      config: Config,
      parent: XmlState,
      cs: Vector[Char] = Vector('<', '/')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true
      implicit val dtctx = parent.dtctx

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
        val tag = cs.drop(2).mkString.trim.stripSuffix(">")
        if (tag == parent.tagName) {
          // Proper closing tag: finalize content and return with child elements preserved
          val xs = parent.parseInlineContents(parent.cs.mkString)
          val dox = parent._create_dox(xs)
          parent.parent match {
            case gp: XmlState =>
              // Append the rendered markup (with attributes preserved) to the outer XmlState buffer
              val inner = {
                val buf = new StringBuilder
                dox.printDox(buf)
                buf.toString
              }
              gp.copy(cs = gp.cs ++ inner.toVector)
            case _ =>
              parent.parent.returnFrom(Seq(dox))
          }
        } else if (parent.isInstanceOf[XmlState] && parent.asInstanceOf[XmlState].tagName != tag) {
          parent.parent match {
            case gp: XmlState =>
              gp.returnCharsFrom(s"</$tag>".toVector)
            case _ =>
              parent.character_State('<')
          }
        } else {
          parent.character_State('<')
        }
      }
    }

    case class TagOpenState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector('<')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true

      override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState =
        parent.returnCharsFrom(cs ++ p)

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState = {
        val raw = cs.dropWhile(_ == '<').mkString
        if (raw.trim.endsWith("/") && !raw.endsWith("/"))
          RAISE.noReachDefect(this, "malformed self-closing generic tag")
        else if (raw.endsWith("/")) {
          val (tag, attrs) = XmlState._parse_tag_definition(raw)
          val dtctx = Dox2Parser.ParseContext.now().dateTimeContext
          val dox = Dox.attachLocation(
            Dox.create(tag, attrs, Vector.empty[Dox])(dtctx),
            config.location
          )
          parent match {
            case gp: XmlState =>
              val buf = new StringBuilder
              dox.printDox(buf)
              gp.copy(cs = gp.cs ++ buf.toString.toVector)
            case _ =>
              parent.returnFrom(Vector(dox))
          }
        } else {
          val (tag, attrs) = XmlState._parse_tag_definition(raw)
          XmlState(config, parent = parent, tagName = tag, attrs = attrs)
        }
      }
    }

    case class ContentState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector.empty
    ) extends ChildDoxInlineParseState with RawFeature {
      override def returnCharsFrom(p: Seq[Char]): DoxInlineParseState =
        parent.returnCharsFrom(cs ++ p)

      override protected def use_angle_bracket = true

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def open_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        evt.next match {
          case Some(s) if s == '/' => SkipOneState(TagCloseState(config, this), s)
          case _ => TagOpenState(config, this)
        }

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        if (_is_in_escape(cs))
          character_State(evt)
        else
          super.close_Angle_Bracket_State(evt)
    }

    private def _is_in_escape(cs: Vector[Char]): Boolean = {
      @annotation.tailrec
      def _loop_(xs: List[Char], triplecount: Int, singlecount: Int): Boolean = xs match {
        case '`' :: '`' :: '`' :: rest =>
          // Found a triple backtick: toggle code block mode
          _loop_(rest, triplecount + 1, singlecount)
        case '`' :: rest if triplecount % 2 == 0 =>
          // Found a single backtick outside a code block
          _loop_(rest, triplecount, singlecount + 1)
        case _ :: rest =>
          // Other characters: continue scanning
          _loop_(rest, triplecount, singlecount)
        case Nil =>
          // Escaping if inside a code block or inline code
          (triplecount % 2 == 1) || (singlecount % 2 == 1)
      }

      _loop_(cs.toList, 0, 0)
    }

    case class TagCloseState(
      config: Config,
      parent: DoxInlineParseState,
      cs: Vector[Char] = Vector('<', '/')
    ) extends ChildDoxInlineParseState with RawFeature {
      override protected def use_angle_bracket = true

      override protected def character_State(c: Char) = copy(cs = cs :+ c)

      override protected def close_Angle_Bracket_State(evt: CharEvent): DoxInlineParseState =
        leave_to_chars(cs :+ '>')
    }

    private def _parse_tag_definition(raw: String): (String, Vector[(String, String)]) = {
      val trimmed = raw.trim.stripSuffix(">")
      val normalized = {
        val r = trimmed.reverse.dropWhile(_.isWhitespace)
        val s = if (r.startsWith("/")) r.drop(1) else r
        s.reverse.trim
      }
      val len = normalized.length
      @annotation.tailrec
      def _skip_space_(idx: Int): Int =
        if (idx < len && normalized.charAt(idx).isWhitespace)
          _skip_space_(idx + 1)
        else
          idx

      val start = _skip_space_(0)
      val nameend = {
        @annotation.tailrec
        def _loop_(i: Int): Int =
          if (i < len && !normalized.charAt(i).isWhitespace) _loop_(i + 1)
          else i
        _loop_(start)
      }
      val tagname =
        if (start < nameend) normalized.substring(start, nameend) else ""

      val attrs = Vector.newBuilder[(String, String)]

      @annotation.tailrec
      def _parse_attr_(idx: Int): Unit = {
        val i = _skip_space_(idx)
        if (i >= len)
          ()
        else {
          val namestart = i
          @annotation.tailrec
          def _read_name_(j: Int): Int =
            if (j < len) {
              val ch = normalized.charAt(j)
              if (!ch.isWhitespace && ch != '=') _read_name_(j + 1)
              else j
            } else j

          val nameend = _read_name_(namestart)
          val attrname =
            if (namestart < nameend) normalized.substring(namestart, nameend) else ""
          val aftername = _skip_space_(nameend)
          if (attrname.nonEmpty) {
            if (aftername < len && normalized.charAt(aftername) == '=') {
              val valuestart = _skip_space_(aftername + 1)
              if (valuestart < len && (normalized.charAt(valuestart) == '"' || normalized.charAt(valuestart) == '\'')) {
                val quote = normalized.charAt(valuestart)
                val valuebodystart = valuestart + 1
                @annotation.tailrec
                def _read_quoted_(j: Int): Int =
                  if (j < len && normalized.charAt(j) != quote) _read_quoted_(j + 1) else j
                val valueend = _read_quoted_(valuebodystart)
                val value = normalized.substring(valuebodystart, valueend)
                attrs += attrname -> value
                val next = if (valueend < len) valueend + 1 else valueend
                _parse_attr_(next)
              } else {
                @annotation.tailrec
                def _read_unquoted_(j: Int): Int =
                  if (j < len && !normalized.charAt(j).isWhitespace) _read_unquoted_(j + 1) else j
                val valueend = _read_unquoted_(valuestart)
                val value = normalized.substring(valuestart, valueend)
                attrs += attrname -> value
                _parse_attr_(valueend)
              }
            } else {
              // boolean attribute
              attrs += attrname -> ""
              _parse_attr_(aftername)
            }
          } else {
            _parse_attr_(aftername + 1)
          }
        }
      }

      _parse_attr_(nameend)
      (tagname, attrs.result())
    }
  }

  case class OpenTagState(
    config: Config,
    parent: DoxInlineParseState,
    name: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    def resultOpenClose(ps: Vector[(String, String)]) =
      if (true)
        XmlState(config, parent, name.mkString, ps)
      else
        InlineState(CloseTagState(config, parent, name.mkString, ps), '<', '/')

    def resultOpenEnd(ps: Vector[(String, String)]): DoxInlineParseState = {
      val dtctx = Dox2Parser.ParseContext.now().dateTimeContext
      val dox = Dox.attachLocation(
        Dox.create(name.mkString, ps, Vector.empty[Dox])(dtctx),
        config.location
      )
      parent.returnFrom(Vector(dox))
    }

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' if name.isEmpty =>
          RAISE.syntaxErrorFault("Generic open tag requires a non-empty tag name")
        case '>' =>
          if (true)
            XmlState(config, parent, name.mkString, Vector.empty)
          else
            InlineState(CloseTagState(config, parent, name.mkString, Vector.empty), '<', '/')
        case '/' if name.isEmpty =>
          RAISE.syntaxErrorFault("Generic closing tag has no matching open tag")
        case '/' => SkipOneState(config, resultOpenEnd(Vector.empty), '>')
        case ' ' if name.isEmpty =>
          RAISE.syntaxErrorFault("Generic open tag requires a non-empty tag name")
        case ' ' => SkipSpaceState(config, TagAttributeListState(config, this))
        case m => copy(name = name :+ m)
      }
  }

  case class TagAttributeListState(
    config: Config,
    parent: OpenTagState,
    attrs: Vector[(String, String)] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override def returnFrom(c: Char): DoxInlineParseState =
      TagAttributeState(config, this, Vector(c))

    def resultFrom(key: String, value: String) = copy(attrs = attrs :+ (key, value))

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' => parent.resultOpenClose(attrs)
        case '/' => SkipOneState(config, parent.resultOpenEnd(attrs), '>')
        case ' ' => SkipSpaceState(config, TagAttributeState(config, this))
      }
  }

  case class TagAttributeState(
    config: Config,
    parent: TagAttributeListState,
    key: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    def resultFrom(p: String): DoxInlineParseState = parent.resultFrom(key.mkString, p)

    override def returnFrom(c: Char): DoxInlineParseState = copy(key = key :+ c)

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' => parent.parent.resultOpenClose(parent.attrs :+ (key.mkString, ""))
        case '/' =>
          val keytext = key.mkString
          val newattrs = parent.attrs :+ (keytext, "")
          SkipOneState(config, parent.parent.resultOpenEnd(newattrs), '>')
        case ' ' => this
        case '=' => SkipSpaceStartState(config, TagAttributeValueState(config, this), '"')
        case m => copy(key = key :+ m)
      }
  }

  case class TagAttributeValueState(
    config: Config,
    parent: TagAttributeState,
    value: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '"' => parent.resultFrom(value.mkString)
        case m => copy(value = value :+ m)
      }
  }

  case class CloseTagState(
    config: Config,
    parent: DoxInlineParseState,
    name: String,
    attrs: Vector[(String, String)],
    dox: Vector[Dox] = Vector.empty,
    closeName: Vector[Char] = Vector.empty
  ) extends ChildDoxInlineParseState with RawFeature {
    private val _ctx = Dox2Parser.ParseContext.now() // TODO
    implicit def dtctx = _ctx.dateTimeContext

    override def returnFrom(ps: Seq[Dox]): DoxInlineParseState = copy(dox = ps.toVector)

    override protected def character_State(evt: CharEvent): DoxInlineParseState =
      evt.c match {
        case '>' =>
          if (closeName.mkString != name)
            RAISE.syntaxErrorFault(s"Tag name unmatch: $name != ${closeName.mkString}")
          leave_to(Dox.attachLocation(Dox.create(name, attrs, dox), config.location))
        case ' ' => this
        case m => copy(closeName = closeName :+ m)
      }
  }
}
