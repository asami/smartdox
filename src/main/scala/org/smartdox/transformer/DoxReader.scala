package org.smartdox.transformer

import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import org.smartdox.Dox

/*
 * @since   Jan. 11, 2012
 * @version Jan. 13, 2012
 * @author  ASAMI, Tomoharu
 */
class DoxReader(val dox: Dox) extends Reader[Dox] {
  def first = dox
  def rest = {
    if (atEnd) this
    else new DoxReader2(dox.elements, Nil)
  }
  def pos = new DoxPosition()
  def atEnd = dox.elements.isEmpty
}

// XXX suppot close element token
class DoxReader2(val elements: List[Dox], val stack: List[List[Dox]]) extends Reader[Dox] {
  val input = "DoxReader2: " + elements + ", " + stack
  print(input)
  println()
  def first = elements.head ensuring {x => println("first = " + x);true}
  def rest = {
    if (atEnd) this
    else {
      val tail = elements.tail
      val h = elements.head
      val h2 = elements.head.elements
      if (elements.head.elements.isEmpty) {
        if (tail.isEmpty) 
          if (stack.isEmpty) new DoxReader2(Nil, Nil)
          else new DoxReader2(stack.head, stack.tail)
        else 
          new DoxReader2(tail, stack)
      } else {
        if (tail.isEmpty) 
          new DoxReader2(elements.head.elements, stack)
        else 
          new DoxReader2(elements.head.elements, tail :: stack)
      }
    }
  }
  def pos = new DoxPosition()
  def atEnd = {
    def emptyelements = elements.isEmpty || (elements.head.elements.isEmpty && elements.tail.isEmpty)
//    def emptyelements = elements.isEmpty || elements.tail.isEmpty
//    emptyelements && stack.isEmpty ensuring {x => println("atEnd = " + x + "," + elements + "," + stack);true}
    elements.isEmpty && stack.isEmpty
  }
}

class DoxPosition extends Position {
  def line: Int = 0
  def column: Int = 0
  def lineContents = "XXX"
}
