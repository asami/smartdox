package org.smartdox

/*
 * derived from SNode.java since Sep. 17, 2006
 * derived from SDoc.scala since Sep.  1, 2008
 *
 * @since   Dec. 24, 2011
 * @version Dec. 24, 2011
 * @author  ASAMI, Tomoharu
 */
abstract class Dox {
}

trait Block extends Dox {
}

trait Inline extends Dox {
}

object Dox {
}

case class Document(head: Head, body: Body) extends Dox {
}

case class Head() extends Dox {
}

case class Body(contents: List[Dox]) extends Dox {
}

case class Section(contents: List[Dox]) extends Dox {
}

case class Div() extends Block {
}

case class P() extends Block {
}

case class Text(contents: String) extends Inline {
  
}

case class Bold(contents: List[Inline]) extends Inline {
}
