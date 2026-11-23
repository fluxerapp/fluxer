// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/http
import fluxer_gateway/config/settings
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleam/uri
import support/geo_probe as geo
import support/voice_api as api
import support/wait

const profile_shards = 8

@external(erlang, "erlang", "phash2")
fn phash2(term: Pid, range: Int) -> Int

@external(erlang, "inets", "stop")
fn inets_stop(service: Atom, profile: Atom) -> Dynamic

@external(erlang, "httpc", "get_options")
fn get_options(options: List(Atom), profile: Atom) -> Dynamic

@external(erlang, "httpc", "which_sessions")
fn which_sessions(
  profile: Atom,
) -> #(List(Dynamic), List(Dynamic), List(Dynamic))

fn post(url: String, kind: String) -> Result(http.Response, http.HttpFailure) {
  let body =
    json.object([#("type", json.string(kind))])
    |> json.to_string
    |> bit_array.from_string
  http.post(url, [#("content-type", "application/json")], body, 5000)
}

fn shard(pid: Pid) -> Int {
  phash2(pid, profile_shards)
}

fn kept_alive(target: Int, port: Int) -> Bool {
  let profile = atom.create("fluxer_gateway_api_http_" <> int.to_string(target))
  let #(good, _, _) = which_sessions(profile)
  list.any(good, fn(session) {
    decode.run(session, decode.at([1, 0, 1], decode.int)) == Ok(port)
  })
}

fn probe_on(
  url: String,
  target: Int,
  replies: Subject(Result(http.Response, http.HttpFailure)),
) -> Nil {
  let pid =
    process.spawn(fn() {
      case shard(process.self()) == target {
        True -> process.send(replies, post(url, "voice_state_upsert"))
        False -> Nil
      }
    })
  case shard(pid) == target {
    True -> Nil
    False -> probe_on(url, target, replies)
  }
}

pub fn a_held_request_does_not_block_the_next_one_on_its_profile_test() {
  let stub = api.start()
  api.set(stub, "voice_get_token", api.Hold)
  let assert Some(url) = settings.get().api_rpc_endpoint
  let assert Ok(uri.Uri(port: Some(port), ..)) = uri.parse(url)
  let holder =
    process.spawn(fn() {
      let target = shard(process.self())
      let assert Ok(http.Response(status: 200, ..)) =
        post(url, "voice_state_upsert")
      let assert True = wait.until(5000, fn() { kept_alive(target, port) })
      post(url, "voice_get_token")
    })
  assert api.await_held(stub, "voice_get_token", 1)
  let replies = process.new_subject()
  probe_on(url, shard(holder), replies)
  let reply = process.receive(replies, 2000)
  api.stop(stub)
  let assert Ok(Ok(http.Response(status: 200, ..))) = reply
}

pub fn every_profile_resolves_the_api_host_over_ipv4_only_test() {
  let stub = api.start()
  let assert Some(url) = settings.get().api_rpc_endpoint
  let assert Ok(http.Response(status: 200, ..)) =
    post(url, "voice_state_upsert")
  api.stop(stub)
  let families =
    list.repeat(Nil, profile_shards)
    |> list.index_map(fn(_, index) {
      let profile =
        atom.create("fluxer_gateway_api_http_" <> int.to_string(index))
      get_options([atom.create("ipfamily")], profile)
      |> decode.run(decode.at([1], decode.list(decode.at([1], atom.decoder()))))
    })
  assert list.all(families, fn(found) { found == Ok([atom.create("inet")]) })
}

fn byte_text(text: String) -> String {
  bit_array.from_string(text)
  |> string.inspect
  |> string.drop_start(2)
  |> string.drop_end(2)
}

pub fn a_crashed_request_logs_no_body_test() {
  let stub = api.start()
  let assert Some(url) = settings.get().api_rpc_endpoint
  let secret = "s3cr3t-session-token"
  let body =
    json.object([
      #("type", json.string("voice_get_token")),
      #("token", json.string(secret)),
      ..geo.fields()
    ])
    |> json.to_string
    |> bit_array.from_string
  let capture = geo.capture_logs()
  let replies = process.new_subject()
  process.spawn(fn() {
    let profile =
      atom.create(
        "fluxer_gateway_api_http_" <> int.to_string(shard(process.self())),
      )
    let _ = inets_stop(atom.create("httpc"), profile)
    process.send(
      replies,
      http.post(url, [#("content-type", "application/json")], body, 5000),
    )
  })
  let reply = process.receive(replies, 10_000)
  let lines = geo.finish(capture)
  api.stop(stub)
  let assert Ok(Error(http.Transport("request_exception"))) = reply
  let crashed =
    list.filter(lines, string.contains(_, "API HTTP request crashed"))
  assert crashed != []
  let forbidden = [
    secret,
    byte_text(secret),
    byte_text(geo.latitude),
    byte_text(geo.longitude),
  ]
  assert geo.leaking(lines) == []
  assert list.all(lines, fn(line) {
    !list.any(forbidden, string.contains(line, _))
  })
}

fn held_caller(url: String) -> Pid {
  process.spawn_unlinked(fn() {
    let _ = post(url, "voice_get_token")
    Nil
  })
}

pub fn a_killed_caller_frees_its_request_slot_test() {
  let stub = api.start()
  api.set(stub, "voice_get_token", api.Hold)
  let assert Some(url) = settings.get().api_rpc_endpoint
  let baseline = http.in_flight()
  let caller = held_caller(url)
  assert api.await_held(stub, "voice_get_token", 1)
  assert http.in_flight() == baseline + 1
  process.kill(caller)
  assert wait.until(2000, fn() { http.in_flight() == baseline })
  api.stop(stub)
}

pub fn a_linked_exit_frees_the_request_slot_test() {
  let stub = api.start()
  api.set(stub, "voice_get_token", api.Hold)
  let assert Some(url) = settings.get().api_rpc_endpoint
  let baseline = http.in_flight()
  let close = process.new_subject()
  let owner =
    process.spawn_unlinked(fn() {
      let closing = process.new_subject()
      process.send(close, closing)
      process.spawn(fn() {
        let _ = post(url, "voice_get_token")
        Nil
      })
      let _ = process.receive_forever(closing)
      process.send_abnormal_exit(process.self(), "socket_closed")
    })
  let assert Ok(closing) = process.receive(close, 1000)
  assert api.await_held(stub, "voice_get_token", 1)
  assert http.in_flight() == baseline + 1
  let monitor = process.monitor(owner)
  process.send(closing, Nil)
  let down =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
  assert process.selector_receive(down, 2000) == Ok(Nil)
  assert wait.until(2000, fn() { http.in_flight() == baseline })
  api.stop(stub)
}
