package org.smartdox.util

import scalaz._, Scalaz._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers
import org.smartdox.parser.{DoxParser, UseDoxParser}
import org.smartdox.Dox

/*
 * @since   Sep.  9, 2014
 * @version Sep.  9, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TechDocTransformerSpec extends WordSpec with Matchers with ScalazMatchers with UseDoxParser {
  "TechDocTransformer" should {
    "top" in {
      val in = "_OK_"
      val out = """<!DOCTYPE html><html><head/><body><h1 class="title"/><p>_OK_</p></body></html>"""
      val a = parse_document(in)
      val b = TechDocTransformer().transform(a)
      b.toString() should be (out)
    }
    "section" in {
      val in = "* sec\n_OK_\n"
      val out = """<!DOCTYPE html><html><head/><body><h1 class="title"/><section><h2>sec</h2><p>_OK_</p></section></body></html>"""
      val a = parse_document(in)
      val b = TechDocTransformer().transform(a)
      b.toString() should be (out)
    }
    "li" in {
      val in = "* sec\n- a_OK_b\n- a/OK/b\n"
      val out = """<!DOCTYPE html><html><head/><body><h1 class="title"/><section><h2>sec</h2><ul><li>a_OK_b</li><li>a/OK/b</li></ul></section></body></html>"""
      val a = parse_document(in)
      val b = TechDocTransformer().transform(a)
      b.toString() should be (out)
    }
    "sample" in {
      val in = """#+title: mediainfo

mediainfoオペレーションでAppidのメディア情報として以下の情報を
取得することができます。

* 使用例

#+caption: 使用例
#+begin_src console
curl -H "Authorization: Bearer XXXX" "http://localhost:9000/2.1/SnapInApp/mediainfo" | jq "."
{
  "statuscode": 200,
  "status": "OK",
  "data": {
    "scope": {
      "shop_ids": [],
      "brand_ids": [
        429
      ],
      "company_ids": []
    },
    "company_ids": [
      116
    ],
    "readOnly": false,
    "appid": "SnapInApp",
    "major": "kddi",
    "minor": "grandberrymall",
    "group_id": "system-system-0-brand-kddi-grandberrymall",
    "executionMode": 1,
    "tag_owner_ids": [
      "system-system-0-company-kddi"
    ],
    "tag_provider_ids": [
      "system-system-0-brand-kddi-grandberrymall",
      "system-system-0-company-kddi",
      "system-system-0-party-system"
    ],
    "preservedLike": false
  },
  "success": true
}
#+end_src

* 情報

mediainfoでは以下の情報を取得することができます。

- 一般情報
- タグ情報
- スコープ情報
- メンテナンス情報

** 一般情報



** タグ情報

タグ情報に関して以下の情報が示されます。

| パラメタ         | データ型 | 多重度 | 説明                 |
|------------------+----------+--------+----------------------|
| tag_owner_ids    | string   | *      | タグ空間所有者のID列 |
| tag_provider_ids | string   | *      | タグ空間提供者のID列 |

** スコープ情報

スコープ情報として以下の情報が示されます。

| パラメタ    | データ型 | 多重度 | 説明                         |
|-------------+----------+--------+------------------------------|
| company_ids | long     | *      | スコープを構成する会社のID列 |
| scope       | object   | 1      | スコープ                     |

*** company_id

company_idsは対象となるリソース(例: news一覧で表示されるnews)が所属する
可能性のある会社IDが示されます。

company_idsと後述のscopeとの関係は以下になります。
scopeは絞り込み条件を正確に記述したもので、実際のAPI内での絞込みに
使用されている情報です。
一方、company_idはscope情報から計算した
「リソースが所属する可能性のある会社ID」です。
たとえば、scopeの情報としてbrand_idsに「110」というIDが入っていた場合、
ブランド「110」の所属する会社のID「9」がcompany_idとして通知されます。

*** scope

scopeプロパティの配下には以下の情報が示されます。

| パラメタ    | データ型 | 多重度 | 説明         |
|-------------+----------+--------+--------------|
| company_ids | long     | *      | 会社ID列     |
| brand_ids   | long     | *      | ブランドID列 |
| shop_ids    | long     | *      | ショップID列 |

scopeプロパティの情報は、Appidに設定された会社、ブランド、ショップです。
対象となるリソース(例: news一覧で表示されるnews)は、以下の条件で
絞り込まれます。

- company_idsで指定された会社ID OR brand_idsで指定されたブランドID OR shop_idsで指定されたショップID

** メンテナンス情報

メンテナンス情報として以下の情報が示されます。

| パラメタ                    | データ型 | 説明                         |
|-----------------------------+----------+------------------------------|
| maintenance_undergoing      | boolean  | 現在メンテナンス中か否か     |
| maintenance_undergoing_type | string   | メンテナンスの種類           |
| maintenance_regular_start   | datetime | 通常メンテナンス開始予定日時 |
| maintenance_regular_end     | datetime | 通常メンテナンス終了予定日時 |
| maintenance_message         | string   | メンテナンスメッセージ       |
"""
      val out = """<!DOCTYPE html><html><head/><body><h1 class="title"/><section><h2>sec</h2><ul><li>a_OK_b</li><li>a/OK/b</li></ul></section></body></html>"""
      val a = parse_document(in)
      val b = TechDocTransformer().transform(a)
//      println(b.toString())
//      b.toString() should be (out)
    }
  }
}
