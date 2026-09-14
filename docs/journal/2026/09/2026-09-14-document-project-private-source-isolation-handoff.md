# SmartDox Document Project Private Source Isolation Handoff

Status: HANDOFF — NOT IMPLEMENTED
Created: 2026-09-14
Owner: `/Users/asami/src/dev2025/smartdox`
Observed consumer: `/Users/asami/src/dev2025/simplemodeling-org`
SmartDox HEAD at inspection: `34d27df6b8516951d2f664bc60e5be1f556f2901`

## Purpose and authority

SimpleModeling.org の公開準備中、記事 PDF のサイトリンク解決が
Document Project 内の私的な作業記録まで解析して停止した。
ユーザーは「application-modeling.dox の下は index.dox 以外は読んでは
いけないはず」と指摘し、SmartDox 側で修正するための journal handoff を
依頼した。

この journal は観測事実と修正引継ぎを記録するものであり、単独では
新しい仕様・設計を定義しない。実装前に既存の Document Project 契約と
照合し、必要な仕様・設計・Executable Specification を更新すること。
本依頼で実施するのはこのファイルの追加のみ。コード修正、テスト実行、
成果物更新、ローカル公開、コミット、本番生成・配備は未実施である。

## Incident evidence

実行場所:
`/Users/asami/src/dev2025/simplemodeling-org`

```text
cozy media build \
  /Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/development-process/domain-modeling.dox/publication-sync/media.json \
  --site-root /Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite \
  --site-config /Users/asami/src/dev2025/simplemodeling-org/src/main/doxsite/site.conf
```

- Request SHA-256:
  `4fa4a336fd06e117605467a6d86f10053c0a934b71d0c179aa93e475653f4230`
- Execution identity: `ac5b7a9775fd7005068be74c39c795bc`
- Terminal exit: `1`
- Evidence directory:
  `/tmp/skill.cncf.d/cozy-dox-64f51c5fb38864479ba1b82a044e480d575f68e5e97506935cee7a6f9d96f549-8cb305aa05a81c0cbe26c4fa809a811a`
- Native execution evidence: `result.json`, `stderr.log`, `stdout.log` in that
  directory. These are retained temporary evidence, not permanent repository
  artifacts.

The first structured diagnostic in stderr is:

```text
org.smartdox.diagnostics.StructuredRenderingDiagnosticException:
document.syntax.invalid stage=parse
source=.../application-modeling.dox/review/bilingual-resume-20260914.md
line=60 column=378 token=_modules ...
```

The final error is:

```text
Article PDF renderer failed for article-pdf-ja: exit=1
```

該当作業記録の現在の物理位置は
`src/main/doxsite/development-process/application-modeling.dox/review/bilingual-resume-20260914.md`
の61行目。診断が報告した行番号は60であり、物理行番号と同一と断定しない。
記録には、バックアップから外部実行環境へのリンクを除外した説明として
次の文字列があった:

```text
External bundled presentation/node_modules link explicitly excluded;
```

これは公開記事本文ではなく、バックアップ作成の私的な実行記録である。
`node_modules` の中身を走査したことを示すエラーではない。
当該記録本文のパス文字列がインライン構文として解析されたことを示す。
記事 PDF 生成の成功・新しい受理証跡は確立していない。

## Existing downstream workaround

2026-09-14 の読み取り確認では、上記作業記録のパス文字列がバッククォートで
囲まれていた。consumer の
`domain-modeling.dox/review/publication-sync-20260914.md` にも、この1か所を
コード表記にする限定修復が記録されている。

この編集は構文エラーの局所回避であり、私的な文書がサイト入力として
解析される責任境界の問題は修正していない。この handoff は回避編集後の
生成成功を主張しない。回帰 fixture はコード表記にした本文ではなく、
修正前の文字列および明示的に構文不正な私的文書を使うこと。
consumer の作業メモをさらに書き換えて通す方法を恒久修正にしない。

## Causal boundary confirmed by source inspection

- `src/main/scala/org/smartdox/doxsite/DoxSite.scala` の `create` は
  `realm.transformTree(new DoxSiteBuilder(rule, ctx))` を先に実行し、
  その後に enabler、リンク解決、メタデータ生成などを進める。
- `src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala` の
  `Rule.getTargetName` はファイル名・パスの一般的な include/exclude 判定後、
  `.md` と `.markdown` も文書として選択する。現在の `isIgnore` は
  ディレクトリ名が `.d` で終わるケースを除外するだけで、Document Project
  内の私的文書を選別していない。
- 同 builder の `_markdown_page` は選択された本文を
  `Dox2Parser.parseWithFilename` に渡す。
- `src/main/scala/org/smartdox/doxsite/SitePublicationContext.scala` の
  `create` は PDF サイトリンク解決用にも `DoxSite.create` を利用する。
  したがってサイト入力選別の不備が記事 PDF にも波及する。
- `DoxSiteConfigLoader` の現在の設定読込には、Document Project の内部文書を
  解析前に除外する専用設定は見つからなかった。

確定した不備は、Document Project の私的な子文書が公開文書として
解析されること。Markdown の受理文法を変更すべきかは別の問題であり、
この修正の前提・範囲には含めない。

## Proposed repair boundary

SmartDox のサイト文書入力選別を所有境界とする。候補:

- `src/main/scala/org/smartdox/doxsite/DoxSiteBuilder.scala`
- `src/main/scala/org/smartdox/doxsite/DoxSite.scala` / shared Document Project
  effective-content logic, only if the existing recognition contract requires it
- `src/test/scala/org/smartdox/doxsite/DoxSiteSpec.scala`
- `src/test/scala/org/smartdox/service/operations/PdfOperationClassSpec.scala`
- the corresponding existing spec/design documents, after checking their scope

これらは調査・修正候補であって、実装用の凍結済み Fix Manifest ではない。
consumer は修正対象外とし、入力・証拠の読み取りに限定する。

意図する責任境界:

1. 既存契約に従って Document Project を認識し、その直接の `index.dox` を
   記事として選択する。単なる `.dox` ファイルとプロジェクトディレクトリを
   混同しない。
2. Project 内の `review/`、`presentation/`、台本、README、作業記録などを
   独立したサイト文書として解析しない。出力段階で隠すのでは遅い。
3. これは文書解析対象の制限であり、記事が明示的に参照する画像などの
   リソース読込・staging や、契約で必要な package metadata の読込まで
   一律に禁止するものではない。私的文書の自動ページ化と区別する。
4. 認識条件、index を持たない `.dox` ディレクトリ、既存の nested-project
   fixture は既存仕様と照合する。ディレクトリ名だけで全子孫を排除し、
   正当な別プロジェクトや following sibling を失わせない。

維持する互換性:

- physical `xxx.dox/index.dox` と logical `xxx.dox` の effective-content mapping
- flattened localized public URLs、Site-link、related links、LinkCollection
- Antora module path、category/notice metadata、bibliography/media associations
- 既存公開文書の構文診断、locale/site context、renderer-start 前の失敗条件
- SmartDox / Markdown の受理文法と source-location/resource-origin 契約

参照する既存文書:

- `docs/spec/parser-pdf-decomposition-compatibility.md`
- `docs/design/parser-pdf-responsibility-decomposition.md`
- `DoxSiteSpec` の Document Project public-path compatibility specification
- `PdfOperationClassSpec` の `PDF site publication context` specifications

禁止する回避・拡大:

- `application-modeling` や `node_modules` 文字列だけの特別扱い
- 私的文書の parse exception を握り潰すだけの対応
- 全サイトの Markdown 無効化、公開文書の構文診断の弱化
- consumer のメモ削除・移動・一括コード表記化や古い成果物のコピー
- Cozy、共有 parser framework、公開API、配備の無関係な変更

## Required regression evidence for the receiving task

Given / When / Then と `should` による Executable Specifications を追加し、
少なくとも次を観測すること:

1. 有効な `sample.dox/index.dox` と、上記修正前のパス文字列を含む
   `sample.dox/review/resume.md` が共存しても、サイト構築と PDF Site-link
   projection が成功する。
2. 私的な `.md` / `.dox` が明示的に構文不正でも、parser に渡されず、
   独立したページ・notice・検索/リンク用メタデータとして漏れない。
3. Project の `index.dox` または通常の公開文書が不正な場合は、既存の
   診断契約に従って失敗または error document を生成する。私的文書の
   除外を理由に、公開入力の不正まで黙認しない。
4. 明示的に参照する画像と必要な package metadata の既存挙動を保持する。
5. 日英の flattened path、logical/physical Site-link、nested-project と
   following-sibling Antora module path の既存仕様が引き続き成立する。

まず renderer を起動しない既存 PDF site-projection test seam を利用し、
サイト構築と共有リンク解決の経路を検証する。Docker、TTS、動画再生成、
検査専用の PDF/スライド画像は不要。

受領タスクで focused `DoxSiteSpec` / `PdfOperationClassSpec` と
`git diff --check` を実行する。SBT は repository policy に従う登録 runner と
共有 serial wrapper を使用し、独立した top-level SBT を並行実行しない。
この journal 作成タスクでは SBT を実行していない。

## Acceptance and resumption

- 修正前に私的文書を解析して失敗する regression が、解析前の選別修正で
  通過する。コード表記による consumer の回避編集に依存しない。
- 公開文書の診断と既存 Document Project 互換性が維持される。
- 正式な spec/design と executable evidence に修正契約を反映する。
- 修正済み SmartDox runtime の選択後、別途認可された consumer タスクで
  第8回の同じ隔離 PDF 生成と全サイト事前検証を再開する。本番公開・配備は
  その事前検証とは別の認可境界とする。

## Concurrent work to preserve

読み取り時点で SmartDox は dirty。既存の変更:

- `docs/design/article-header-metadata-and-media-actions.md`
- `docs/spec/article-header-metadata-and-media-actions.md`
- `docs/phase/README.md`
- `docs/strategy/smartdox-development-strategy.md`
- `src/main/scala/org/smartdox/doxsite/DoxSite.scala`
- `src/test/scala/org/smartdox/doxsite/DoxSiteSpec.scala`
- untracked Phase 16 journal/checklist/phase documents

既存変更を reset、revert、stash、上書きしない。受領タスクは現行 tree の
所有境界を確認してから実装する。この handoff の追加は既存コード、
仕様、設計、Phase 状態を変更しない。
