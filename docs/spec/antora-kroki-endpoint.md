# Antora Kroki endpoint contract

SmartDox's generated Antora playbook SHALL default
`asciidoc.attributes.kroki-server-url` to `http://127.0.0.1:8000`.
The literal IPv4 loopback address matches the consuming Cozy listener's
`KROKI_LISTEN=127.0.0.1:8000` address family and port, avoiding hostname
resolution to an IPv6 listener address.
`SingleLocaleRoot` emits `antora.d/antora-playbook.yml`; the existing JA/EN
`MultiLocaleSubdirs` layout emits `antora.d/ja/antora-playbook.yml` and
`antora.d/en/antora-playbook.yml`. Each emitted playbook has the same default
endpoint, `kroki-default-format: svg` and `kroki-fetch-diagram: 'true'`.

The existing Antora and Kroki cache paths, locale output layout, navigation,
start-page selection, serialization, public generator API and direct SmartDox
Docker image default retain their semantics. This contract adds no public
endpoint configuration.

[AntoraKrokiEndpointSpec](../../src/test/scala/org/smartdox/generators/AntoraKrokiEndpointSpec.scala)
specifies two active behaviors using actual `AntoraGenerator.generate` results
and parsed YAML attributes: the root playbook and both JA/EN locale playbooks,
including absence of the wrong layout and the obsolete `9609` endpoint.
Generated-product evidence is separate from actual container launch, listener
readiness, native site construction and browser evidence. Those runtime gates
remain the consuming site's responsibility.

See the [paired design](../design/antora-kroki-endpoint.md) and
[Cozy handoff contract](../../../cozy/docs/spec/bok-site-toolchain.md).
