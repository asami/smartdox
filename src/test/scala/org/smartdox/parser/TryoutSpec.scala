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
 * @version May. 10, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with ScalazMatchers with UseDox2Parser {
  "Dox2Parser" should {
    "continue 2" in {
      parse_orgmode_simple("- One\n - Two\n Two-One\n",
        """<ul><li>One<ul><li>Two Two-One</li></ul></li></ul>""")
    }
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
