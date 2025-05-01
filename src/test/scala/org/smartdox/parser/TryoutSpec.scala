package org.smartdox.parser

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Jan.  6, 2019
 *  version Jan. 26, 2019
 *  version May. 31, 2024
 *  version Jun.  9, 2024
 *  version May. 10, 2024
 *  version Sep.  5, 2024
 * @version Oct. 23, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser {
  "DoxParser" should {
    val imgdot = "#+begin_dot image/simple.png\nDOT\n#+end_dot\n"
    "Image" should {
      "img" which {
        "typical" ignore {
          parse_orgmode_simple("[[image/simple.png]]", """<p><img src="image/simple.png"/></p>""")
        }
        "embedded dot" ignore {
          parse_orgmode_simple_debug(imgdot, """<p><img src="image/simple.png"/></p>""")
        }
        "embedded ditaa" ignore {
          parse_orgmode_simple("#+begin_ditaa image/simple.png\nDITAA\n#+end_ditaa\n", """<p><img src="image/simple.png"/></p>""")
        }
        "first,contents/second,contents" ignore {
          parse_orgmode_simple("* First\n1st contents.\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
            """<section><h2>First</h2><p>1st contents.<img src="image/simple.png"/>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""")
        }
      }
      "figure" which {
        val figure = """#+CAPTION: Figure
#+LABEL: fig
[[image/simple.png]]"""
        val result = """<figure id="fig"><img src="image/simple.png"/><figcaption>Figure</figcaption></figure>"""
        "typical" ignore {
          parse_orgmode_simple(figure, result)
        }
        val figuredot = """#+CAPTION: Figure
#+LABEL: fig
""" + imgdot
        "typical dot" ignore {
          parse_orgmode_simple(figuredot, result)
        }
        "first,contents/second,contents" ignore {
          parse_orgmode_simple("* First\n1st contents.\n#+CAPTION: Figure\n#+LABEL: fig\n#+begin_dot image/simple.png\nDOT\n#+end_dot\n1st cont.\n** Second\n2nd *contents*.\n\ncont.\n* Next First\none\n\ntwo\n",
            """<section><h2>First</h2><p>1st contents.</p>%s<p>1st cont.</p><section><h3>Second</h3><p>2nd <b>contents</b>.</p><p>cont.</p></section></section><section><h2>Next First</h2><p>one</p><p>two</p></section>""".format(result))
        }
      }
    }
  }
  "Dox2Parser" should {
    // "continue" in {
    //   parse_orgmode_simple_debug("- This is \n a pen.\n",
    //     """<ul><li>This is a pen.</li></ul>""")
    // }
    // "continue 2 xx" in {
    //   parse_orgmode_simple_debug("abc\n\n- One\n - Two\n - Three\n\nxyz",
    //     """<p>abc</p><ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul><p>xyz</p>""")
    // }
    // "continue 2 x" in {
    //   parse_orgmode_simple_debug("- One\n - Two\n - Three\n",
    //     """<ul><li>One<ul><li>Two</li><li>Three</li></ul></li></ul>""")
    // }
    // "continue 2" in {
    //   parse_orgmode_simple_debug("- One\n - Two\n Two-One\n",
    //     """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
    // }
    // "tryout" in {
    //   parse_orgmode_simple_debug("- One\n- Two\n",
    //     """<ul><li>One</li><li>Two</li></ul>""")
    // }
    "typical" ignore {
      val s0 = """
#+title: アプリケーション・リソース

#+begin_src console
A

B
#+end_src
"""
      val s = """
#+title: アプリケーション・リソース

* 基本アクセス方式

- エンドポイント :: appresource
- 必須プロパティ :: app_resource_kind

app_resource_kindに設定する文字列は採番制とする予定ですが、
当面はappidをプレフィックスにつけるなどして運用規約で重複事故がないようにします。

#+begin_src console
curl -H "Authorization: Bearer yOvHJWoTvcZTWyNWoqFHtUlEybSXoEwI" "http://localhost:9000/2.1/PALShopApp/appresource?with=id&app_resource_kind=PALShopApp.note" -X POST -F 'title=yk0105' | jq .

curl -H "Authorization: Bearer yOvHJWoTvcZTWyNWoqFHtUlEybSXoEwI" "http://localhost:9000/2.1/PALShopApp/appresource?app_resource_kind=PALShopApp.note&limit=10" -X GET | jq .

curl "http://localhost:9000/acm/rest/api/2.1c/appresource?app_resource_kind=PALShopApp.note&limit=10" -H "Cookie: PLAY2AUTH_SESS_ID=806cb846af013dff68555a98e941a4d9513359d61657" --get | jq .
#+end_src

* リソース名追加

以下のいずれかでアプリケーション独自のリソース名を追加可能にする予定です。

- media_v2に設定
- Swaggerによる仕様記述
"""
      val result = Dox2Parser.parse(s)
      print(result)
    }
  }
}
