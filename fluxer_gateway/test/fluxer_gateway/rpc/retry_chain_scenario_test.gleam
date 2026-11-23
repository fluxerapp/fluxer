// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/infra/harness
import fluxer_gateway/rpc/server
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/result
import gleam/string
import support/cluster_app
import support/cluster_fake_nats as fake
import support/isolation

const retry_ms = 60

const window_ms = 600

@external(erlang, "erlang", "register")
fn register(name: Atom, pid: Pid) -> Bool

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

type Request {
  Request(subject: String, reply: Dynamic)
}

fn fake_pool(answer: fn(Int) -> Bool, seen: Subject(String)) -> Pid {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let assert True =
        register(atom.create("fluxer_gateway_nats_pool"), process.self())
      process.send(ready, Nil)
      serve(answer, seen, 0)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  pid
}

fn serve(answer: fn(Int) -> Bool, seen: Subject(String), index: Int) -> Nil {
  let request =
    process.new_selector()
    |> process.select_other(fn(message) {
      let decoder = {
        use subject <- decode.subfield([1, 1], decode.string)
        use reply <- decode.subfield([1, 5], decode.dynamic)
        decode.success(Request(subject:, reply:))
      }
      decode.run(message, decoder) |> result.replace_error(Nil)
    })
    |> process.selector_receive_forever
  case request {
    Ok(Request(subject:, reply:)) -> {
      case answer(index) {
        True -> {
          let id = #(atom.create("subscription_id"), index + 1, process.self())
          process.send(coerce(reply), coerce(id))
          process.send(seen, subject)
        }
        False -> Nil
      }
      serve(answer, seen, index + 1)
    }
    Error(Nil) -> serve(answer, seen, index)
  }
}

fn collect(
  seen: Subject(String),
  wanted: Int,
  acc: List(String),
) -> List(String) {
  case list.length(list.unique(acc)) >= wanted {
    True -> list.unique(acc)
    False ->
      case process.receive(seen, 3000) {
        Ok(subject) -> collect(seen, wanted, [subject, ..acc])
        Error(Nil) -> list.unique(acc)
      }
  }
}

fn retries(pid: Pid, during: Int) -> Int {
  harness.received_during(pid, during)
  |> harness.count_tagged("retry_subscribe")
}

fn start_server() -> Pid {
  let configured =
    settings.Settings(
      ..settings.get(),
      role: node.Guilds,
      nats_rpc_enabled: True,
    )
  let assert Ok(started) = server.supervised(configured).start()
  process.unlink(started.pid)
  started.pid
}

pub fn pool_loss_during_a_retry_chain_keeps_a_single_chain_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      rpc: settings.RpcDurations(..d.rpc, server_subscribe_retry_ms: retry_ms),
      nats: settings.NatsDurations(..d.nats, pool_call_timeout_ms: 50),
    )
  })
  let assert Ok(paused_rpc) = cluster_app.pause("fluxer_gateway_nats_rpc")
  let assert Ok(paused_pool) = cluster_app.pause("fluxer_gateway_nats_pool")
  let seen = process.new_subject()
  let partial = fake_pool(fn(index) { index == 0 }, seen)
  let rpc = start_server()
  assert collect(seen, 1, []) |> list.length == 1
  process.sleep(250)
  cluster_app.stop_linked(partial)
  let during_outage = retries(rpc, window_ms)
  let expected = window_ms / retry_ms
  assert during_outage >= expected / 2
  assert during_outage <= expected + expected / 2
  let full = fake_pool(fn(_) { True }, seen)
  let subjects = collect(seen, 3, [])
  assert list.sort(subjects, string.compare)
    == ["rpc.gateway.guild.>", "rpc.gateway.process.>", "rpc.gateway.voice.>"]
  process.sleep(retry_ms * 2)
  assert retries(rpc, 400) == 0
  assert fake.wait_until(1000, fn() { process.is_alive(rpc) })
  cluster_app.stop_linked(rpc)
  cluster_app.stop_linked(full)
  cluster_app.resume(paused_pool)
  cluster_app.resume(paused_rpc)
}

pub fn pool_loss_without_a_running_chain_arms_one_chain_test() {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      rpc: settings.RpcDurations(..d.rpc, server_subscribe_retry_ms: retry_ms),
      nats: settings.NatsDurations(..d.nats, pool_call_timeout_ms: 50),
    )
  })
  let assert Ok(paused_rpc) = cluster_app.pause("fluxer_gateway_nats_rpc")
  let assert Ok(paused_pool) = cluster_app.pause("fluxer_gateway_nats_pool")
  let seen = process.new_subject()
  let first = fake_pool(fn(_) { True }, seen)
  let rpc = start_server()
  assert collect(seen, 3, []) |> list.length == 3
  assert retries(rpc, 300) == 0
  cluster_app.stop_linked(first)
  let during_outage = retries(rpc, window_ms)
  let expected = window_ms / retry_ms
  assert during_outage >= expected / 2
  assert during_outage <= expected + expected / 2
  let second = fake_pool(fn(_) { True }, seen)
  assert collect(seen, 3, []) |> list.length == 3
  process.sleep(retry_ms * 2)
  assert retries(rpc, 400) == 0
  cluster_app.stop_linked(rpc)
  cluster_app.stop_linked(second)
  cluster_app.resume(paused_pool)
  cluster_app.resume(paused_rpc)
}
