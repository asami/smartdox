package org.smartdox.doxsite

import scalaz._, Scalaz._

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.smartdox.parser.UseDoxParser
import java.io.File

/*
 * @since   Feb. 24, 2025
 *  version Feb. 28, 2025
 * @version Mar.  1, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DoxSiteSpec extends WordSpec with Matchers with UseDoxParser {
  "DoxSite" should {
    "create" in {
      val site = DoxSite.create(new File("src/test/resources/site1"))
      println(site)
    }
  }
}
