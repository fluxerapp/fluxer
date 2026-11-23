// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/http
import fluxer_gateway/config/settings
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/uri
import support/fake_http
import support/gateway_api_stub
import support/guild_api
import support/session_harness
import support/wait

const profile_shards = 8

@external(erlang, "erlang", "phash2")
fn phash2(term: Pid, range: Int) -> Int

@external(erlang, "httpc", "which_sessions")
fn which_sessions(
  profile: Atom,
) -> #(List(Dynamic), List(Dynamic), List(Dynamic))

fn post(url: String) -> Result(http.Response, http.HttpFailure) {
  let body =
    json.object([#("type", json.string("probe"))])
    |> json.to_string
    |> bit_array.from_string
  http.post(url, [#("content-type", "application/json")], body, 5000)
}

fn port_of(url: String) -> Int {
  let assert Ok(uri.Uri(port: Some(port), ..)) = uri.parse(url)
  port
}

fn kept_alive(port: Int) -> Bool {
  let profile =
    atom.create(
      "fluxer_gateway_api_http_"
      <> int.to_string(phash2(process.self(), profile_shards)),
    )
  let #(good, _, _) = which_sessions(profile)
  list.any(good, fn(session) {
    decode.run(session, decode.at([1, 0, 1], decode.int)) == Ok(port)
  })
}

fn twenty_probes(api: guild_api.FakeApi) -> Nil {
  guild_api.override(api, "probe", None, guild_api.Data(json.object([])))
  list.each(list.repeat(Nil, 20), fn(_) {
    let assert Ok(http.Response(status: 200, ..)) = post(api.url)
  })
}

pub fn fake_apis_share_one_listener_and_reuse_its_connections_test() {
  let first = guild_api.start()
  twenty_probes(first)
  let first_served = fake_http.connections(first.route)
  guild_api.stop(first)
  let second = guild_api.start()
  twenty_probes(second)
  let second_served = fake_http.connections(second.route)
  guild_api.stop(second)
  assert port_of(first.url) == port_of(second.url)
  assert list.length(first_served) == 1
  assert second_served == first_served
}

pub fn a_stopped_fake_api_answers_nothing_test() {
  let api = guild_api.start()
  guild_api.stop(api)
  let assert Error(_) = post(api.url)
}

pub fn the_gateway_api_stub_keeps_its_connection_open_test() {
  let stub = gateway_api_stub.start(fn(_, _) { #(200, "{\"data\":{}}") })
  let url = "http://127.0.0.1:" <> int.to_string(stub.port) <> "/rpc"
  let assert Ok(http.Response(status: 200, ..)) = post(url)
  let assert Ok(http.Response(status: 200, ..)) = post(url)
  let kept = wait.until(2000, fn() { kept_alive(stub.port) })
  gateway_api_stub.stop(stub)
  assert kept
}

pub fn the_session_harness_api_keeps_its_connection_open_test() {
  let kept =
    session_harness.with_api(session_harness.empty_ok, fn() {
      let assert Some(url) = settings.get().api_rpc_endpoint
      let assert Ok(http.Response(status: 200, ..)) = post(url)
      let assert Ok(http.Response(status: 200, ..)) = post(url)
      wait.until(2000, fn() { kept_alive(port_of(url)) })
    })
  assert kept
}
