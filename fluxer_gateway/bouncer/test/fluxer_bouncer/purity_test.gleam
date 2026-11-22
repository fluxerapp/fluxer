// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_bouncer/config
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/list
import gleam/string

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in: String) -> List(String)

fn read(path: String) -> String {
  let assert Ok(bytes) = read_file(path)
  let assert Ok(text) = bit_array.to_string(bytes)
  text
}

fn sources() -> List(String) {
  let bouncer =
    find_files(matching: "**/*.gleam", in: "src")
    |> list.map(fn(path) { "src/" <> path })
  let proto =
    find_files(matching: "**/*.gleam", in: "../bouncer_proto/src")
    |> list.map(fn(path) { "../bouncer_proto/src/" <> path })
  list.append(bouncer, proto)
}

pub fn bouncer_never_decodes_terms_or_loads_the_gateway_test() {
  let files = sources()
  assert list.length(files) >= 10
  let offenders =
    list.filter(files, fn(path) {
      let text = read(path)
      string.contains(text, "binary_to_term")
      || string.contains(text, "import fluxer_gateway")
      || string.contains(text, "fluxer_gateway@")
    })
  assert offenders == []
}

pub fn ordinal_comes_from_the_statefulset_hostname_test() {
  assert config.ordinal_of("gateway-bouncer-2") == Ok(2)
  assert config.ordinal_of("gateway-bouncer")
    == Error(config.Invalid("HOSTNAME", "gateway-bouncer"))
  let assert Ok(built) =
    config.build(
      hostname: "gateway-bouncer-1",
      count: "3",
      basename: "gateway-bouncer",
      dns_name: "gateway-bouncer.ns.svc.cluster.local",
    )
  assert built.ordinal == 1
  assert atom.to_string(config.peer_node(built, 0))
    == "gateway-bouncer@gateway-bouncer-0.gateway-bouncer.ns.svc.cluster.local"
  assert built.quorum == 2
  assert config.with_quorum(built, "1") == Ok(config.Config(..built, quorum: 1))
  assert config.with_quorum(built, "4")
    == Error(config.Invalid("FLUXER_BOUNCER_QUORUM", "4"))
  assert config.build(
      hostname: "gateway-bouncer-3",
      count: "3",
      basename: "b",
      dns_name: "d",
    )
    == Error(config.OrdinalOutOfRange(ordinal: 3, count: 3))
}
