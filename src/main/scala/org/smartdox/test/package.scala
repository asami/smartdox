package org.smartdox

import org.smartdox.structure.test.StructureMatchers

/*
 * @since   Dec.  3, 2024
 * @version Dec. 21, 2024
 * @author  ASAMI, Tomoharu
 */
package object test {
  trait DoxMatchers extends DoxBeMatcher.Matchers
      with DescriptionBeMatcher.Matchers
      with StructureMatchers {
  }
}
