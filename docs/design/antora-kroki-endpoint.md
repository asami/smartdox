# Antora Kroki endpoint coordination

The [endpoint specification](../spec/antora-kroki-endpoint.md) fixes the default
generated endpoint at `http://127.0.0.1:8000`.
`Antora.Playbook.Asciidoc.defaultAttributes` remains its single stable owner.
The existing playbook construction and serialization carry that value through
`_build_single_locale_root` and `_build_locale` without a resolver or new API.
The existing methods continue to select their own Antora and Kroki cache paths.

Cozy's existing Docker Antora handoff supplies `SMARTDOX_KROKI_PORT=8000` for
wrapper launch/readiness and `KROKI_LISTEN=127.0.0.1:8000` for the listener.
The literal IPv4 client address matches the listener's address family and port,
so hostname resolution cannot select IPv6 `::1` for this default. Coordinating
the generated URL with those two settings requires no external toolchain
mutation, image rebuild or new lifecycle. YAML serialization, navigation,
start pages, output layout, diagram format and fetch flag remain. Direct
SmartDox image selection and explicit image overrides stay with their current
owner.

[AntoraKrokiEndpointSpec](../../src/test/scala/org/smartdox/generators/AntoraKrokiEndpointSpec.scala)
uses existing read-only fixtures and fresh normal generator contexts to inspect
actual root and JA/EN playbooks with SnakeYAML. These generated products and
Cozy's recorded build commands establish their respective seams only. Actual
container/listener startup, native site output and browser acceptance need
separate execution; this design claims no runtime completion.

See the [Cozy coordination design](../../../cozy/docs/design/bok-site-toolchain.md).
