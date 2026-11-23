// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import support/wait

pub type Peer {
  Peer(pid: Pid, name: Atom)
}

type PeerStart

@external(erlang, "peer", "start")
fn peer_start(options: Dynamic) -> PeerStart

@external(erlang, "peer", "stop")
fn peer_stop(pid: Pid) -> Atom

@external(erlang, "peer", "call")
fn peer_call(
  pid: Pid,
  module: Atom,
  function: Atom,
  arguments: List(Dynamic),
  timeout: Int,
) -> Dynamic

@external(erlang, "peer", "cast")
fn peer_cast(
  pid: Pid,
  module: Atom,
  function: Atom,
  arguments: List(Dynamic),
) -> Atom

@external(erlang, "code", "get_path")
fn code_path() -> List(Charlist)

@external(erlang, "filename", "absname")
fn absolute(path: Charlist) -> Charlist

@external(erlang, "os", "getpid")
fn os_pid() -> Charlist

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

@external(erlang, "gleam_stdlib", "identity")
pub fn to_dynamic(value: a) -> Dynamic

pub fn unique_name(prefix: String) -> String {
  prefix
  <> "_"
  <> charlist.to_string(os_pid())
  <> "_"
  <> int.to_string(int.absolute_value(unique_integer()))
  <> "@127.0.0.1"
}

pub fn gateway_port(peer: Peer) -> Int {
  let found = call(peer, "fluxer_gateway@app@http", "bound_port", [])
  let assert Ok(port) = decode.run(found, decode.at([1], decode.int))
    as { "peer has no bound gateway port: " <> string.inspect(found) }
  port
}

fn build_paths() -> List(Charlist) {
  code_path()
  |> list.map(absolute)
  |> list.filter(fn(path) {
    string.contains(charlist.to_string(path), "/build/dev/erlang/")
  })
}

fn text(value: String) -> Dynamic {
  to_dynamic(charlist.from_string(value))
}

fn key(name: String) -> Dynamic {
  atom.to_dynamic(atom.create(name))
}

pub fn start(full_name: String, env: List(#(String, String))) -> Peer {
  start_within(full_name, env, 15_000)
}

pub fn start_within(
  full_name: String,
  env: List(#(String, String)),
  within_ms: Int,
) -> Peer {
  let #(short, _) = case string.split_once(full_name, "@") {
    Ok(parts) -> parts
    Error(Nil) -> #(full_name, "")
  }
  let args =
    list.flat_map(build_paths(), fn(path) { [text("-pa"), to_dynamic(path)] })
    |> list.append([
      text("-setcookie"),
      text("fluxer_scn_cluster"),
      text("-kernel"),
      text("prevent_overlapping_partitions"),
      text("false"),
    ])
  let options =
    dynamic.properties([
      #(key("name"), key(short)),
      #(key("host"), text("127.0.0.1")),
      #(key("longnames"), dynamic.bool(True)),
      #(key("connection"), dynamic.int(0)),
      #(key("wait_boot"), dynamic.int(within_ms)),
      #(key("args"), dynamic.list(args)),
      #(
        key("env"),
        dynamic.list(
          list.map(env, fn(entry) {
            to_dynamic(#(
              charlist.from_string(entry.0),
              charlist.from_string(entry.1),
            ))
          }),
        ),
      ),
    ])
  let started = to_dynamic(peer_start(options))
  let decoder = {
    use pid <- decode.field(1, decode_pid())
    use name <- decode.field(2, atom.decoder())
    decode.success(Peer(pid:, name:))
  }
  let assert Ok(peer) = decode.run(started, decoder)
    as { "peer failed to start: " <> string.inspect(started) }
  peer
}

fn decode_pid() -> decode.Decoder(Pid) {
  decode.new_primitive_decoder("Pid", fn(data) {
    case process_identity(data) {
      Ok(pid) -> Ok(pid)
      Error(Nil) -> Error(process.self())
    }
  })
}

fn process_identity(data: Dynamic) -> Result(Pid, Nil) {
  case is_pid(data) {
    True -> Ok(unsafe_pid(data))
    False -> Error(Nil)
  }
}

@external(erlang, "erlang", "is_pid")
fn is_pid(data: Dynamic) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_pid(data: Dynamic) -> Pid

pub fn in_parallel(items: List(a), run: fn(a) -> b, within_ms: Int) -> List(b) {
  items
  |> list.map(fn(item) {
    let reply = process.new_subject()
    let pid = process.spawn_unlinked(fn() { process.send(reply, run(item)) })
    #(reply, process.monitor(pid))
  })
  |> list.map(fn(pending) {
    let #(reply, monitor) = pending
    let outcome =
      process.new_selector()
      |> process.select_map(reply, Ok)
      |> process.select_specific_monitor(monitor, fn(down) {
        Error(string.inspect(down))
      })
      |> process.selector_receive(within_ms)
    process.demonitor_process(monitor)
    let assert Ok(Ok(found)) = outcome
      as { "parallel start failed: " <> string.inspect(outcome) }
    found
  })
}

pub fn call(
  peer: Peer,
  module: String,
  function: String,
  arguments: List(Dynamic),
) -> Dynamic {
  peer_call(
    peer.pid,
    atom.create(module),
    atom.create(function),
    arguments,
    15_000,
  )
}

pub fn cast(
  peer: Peer,
  module: String,
  function: String,
  arguments: List(Dynamic),
) -> Nil {
  peer_cast(peer.pid, atom.create(module), atom.create(function), arguments)
  Nil
}

pub fn stop(peer: Peer) -> Nil {
  peer_stop(peer.pid)
  Nil
}

pub fn node_name(peer: Peer) -> String {
  atom.to_string(peer.name)
}

pub fn wait_until(timeout_ms: Int, check: fn() -> Bool) -> Bool {
  wait.until_every(timeout_ms, 100, check)
}

pub fn http_get(
  peer: Peer,
  port: Int,
  path: String,
) -> Result(#(Int, String), Dynamic) {
  let url = "http://127.0.0.1:" <> int.to_string(port) <> path
  let found = call(peer, "httpc", "request", [text(url)])
  let decoder = {
    use status <- decode.subfield([1, 0, 1], decode.int)
    use body <- decode.subfield([1, 2], decode.dynamic)
    decode.success(#(status, charlist.to_string(coerce(body))))
  }
  decode.run(found, decoder) |> result.replace_error(found)
}

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: Dynamic) -> Charlist
