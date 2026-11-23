// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/event.{type PresenceEvent}
import fluxer_gateway/presence/lanes
import fluxer_gateway/presence/owner
import fluxer_gateway/presence/protocol.{
  type Subscriber, GuildSubscriber, PresenceSubscriber,
}
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/list
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.presence().fanout_init_timeout_ms
}

fn drain_start_timeout_ms() -> Int {
  durations.presence().fanout_drain_start_timeout_ms
}

pub type RelayMsg {
  Deliver(event: PresenceEvent)
  DeliverTo(event: PresenceEvent, to: Subscriber)
  RelayTick
}

type Shed {
  Normal
  Coalescing(pending: Dict(Int, PresenceEvent), order: List(Int))
}

type Relay {
  Relay(ticks: Subject(RelayMsg), received: Int, shed: Shed)
}

type DrainMsg {
  Push(shard: Int, message: RelayMsg)
  Retry
}

type DrainKey {
  Broadcast(user: Int)
  Directed(user: Int, to: Subscriber)
  Tick
}

type Drain {
  Drain(
    node: Atom,
    inbox: Subject(DrainMsg),
    pending: Dict(DrainKey, #(Int, RelayMsg)),
    order: List(DrainKey),
  )
}

const check_every = 64

const relay_coalesce_above = 20_000

const relay_flush_below = 100

const relay_max_targets = 100_000

fn relay_tick_ms() -> Int {
  durations.presence().fanout_relay_tick_ms
}

fn drain_retry_ms() -> Int {
  durations.presence().fanout_drain_retry_ms
}

const drain_max_targets = 10_000

fn drain_linger_ms() -> Int {
  durations.presence().fanout_drain_linger_ms
}

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> #(Atom, Int)

pub fn queue_length(pid: Pid) -> Int {
  process_info(pid, atom.create("message_queue_len")).1
}

fn relay_name(shard: Int) -> Name(RelayMsg) {
  route.name(route.Relay, shard)
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  let base =
    static_supervisor.new(static_supervisor.RestForOne)
    |> static_supervisor.restart_tolerance(intensity: 10, period: 10)
    |> static_supervisor.add(owner.tables_with(
      list.append(registry.tables(), lanes.tables()),
      list.append(registry.bag_tables(), lanes.bag_tables()),
      registry.ordered_tables(),
    ))
  let with_registry =
    list.fold(route.shards(), base, fn(builder, shard) {
      static_supervisor.add(builder, registry.supervised(shard))
    })
  let with_lanes =
    list.fold(route.lanes(), with_registry, fn(builder, lane) {
      static_supervisor.add(builder, lanes.supervised(lane))
    })
  list.fold(route.shards(), with_lanes, fn(builder, shard) {
    static_supervisor.add(builder, relay_supervised(shard))
  })
  |> static_supervisor.supervised
  |> supervision.map_data(fn(_) { Nil })
}

pub fn publish(event: PresenceEvent) -> Int {
  let locals = deliver_local(event)
  let shard = route.shard(event.user)
  let nodes = registry.interest(event.user)
  list.each(nodes, fn(node) { send_remote(node, shard, Deliver(event)) })
  let remotes = list.length(nodes)
  counters.published()
  counters.delivered(counters.LocalDelivery, locals)
  counters.delivered(counters.NodeDelivery, remotes)
  locals + remotes
}

pub fn resend_to(event: PresenceEvent, subscriber: Subscriber) -> Nil {
  let target = case subscriber {
    PresenceSubscriber(inbox:, ..) -> remote.node_of(inbox)
    GuildSubscriber(inbox:, ..) -> remote.node_of(inbox)
  }
  case target == remote.self_atom() {
    True -> lanes.deliver(subscriber, event)
    False ->
      send_remote(target, route.shard(event.user), DeliverTo(event, subscriber))
  }
}

pub fn resend_to_node(event: PresenceEvent, target: Atom) -> Nil {
  case target == remote.self_atom() {
    True -> {
      let _ = deliver_local(event)
      Nil
    }
    False -> send_remote(target, route.shard(event.user), Deliver(event))
  }
}

fn deliver_local(event: PresenceEvent) -> Int {
  lanes.fan_out(event.user, event, registry.local_subscribers)
}

fn send_remote(target: Atom, shard: Int, message: RelayMsg) -> Nil {
  case remote.send_registered(drain_name(target), Push(shard, message)) {
    Ok(Nil) -> Nil
    Error(Nil) ->
      case remote.send(target, relay_name(shard), message) {
        Ok(Nil) -> Nil
        Error(remote.CallBusy) -> {
          counters.incr(counters.BusyDistPort)
          case ensure_drain(target) {
            Ok(drain) -> push_to(drain, target, Push(shard, message))
            Error(Nil) -> counters.incr(counters.DrainDropped)
          }
        }
        Error(_) -> counters.incr(counters.RemoteSendFailed)
      }
  }
}

fn push_to(drain: Pid, target: Atom, message: DrainMsg) -> Nil {
  remote.send_as(drain, drain_name(target), message)
}

fn relay_supervised(shard: Int) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(inbox) {
      let ticks = process.new_subject()
      Ok(
        actor.initialised(Relay(ticks:, received: 0, shed: Normal))
        |> actor.selecting(
          process.new_selector()
          |> process.select(inbox)
          |> process.select(ticks),
        ),
      )
    })
    |> actor.named(relay_name(shard))
    |> actor.on_message(relay_handle)
    |> actor.start
    |> owner.started_nil
  })
}

fn relay_handle(
  relay: Relay,
  message: RelayMsg,
) -> actor.Next(Relay, RelayMsg) {
  let relay = Relay(..relay, received: relay.received + 1)
  let relay = case relay.received % check_every == 0 {
    True -> adjust(relay)
    False -> relay
  }
  let relay = case message, relay.shed {
    Deliver(event), Normal -> {
      counters.delivered(counters.RelayedDelivery, deliver_local(event))
      relay
    }
    Deliver(event), Coalescing(pending, order) ->
      coalesce(relay, pending, order, event)
    DeliverTo(event, to), _ -> {
      lanes.deliver(to, event)
      counters.delivered(counters.RelayedDelivery, 1)
      relay
    }
    RelayTick, Normal -> relay
    RelayTick, Coalescing(..) -> {
      let relay = adjust(relay)
      case relay.shed {
        Coalescing(..) -> {
          let _ = process.send_after(relay.ticks, relay_tick_ms(), RelayTick)
          relay
        }
        Normal -> relay
      }
    }
  }
  actor.continue(relay)
}

fn coalesce(
  relay: Relay,
  pending: Dict(Int, PresenceEvent),
  order: List(Int),
  incoming: PresenceEvent,
) -> Relay {
  case dict.get(pending, incoming.user) {
    Ok(older) -> {
      counters.incr(counters.Coalesced)
      Relay(
        ..relay,
        shed: Coalescing(
          dict.insert(pending, incoming.user, event.merge(older, incoming)),
          order,
        ),
      )
    }
    Error(Nil) ->
      case dict.size(pending) >= relay_max_targets {
        True -> {
          counters.incr(counters.Dropped)
          relay
        }
        False ->
          Relay(
            ..relay,
            shed: Coalescing(dict.insert(pending, incoming.user, incoming), [
              incoming.user,
              ..order
            ]),
          )
      }
  }
}

fn adjust(relay: Relay) -> Relay {
  let length = queue_length(process.self())
  case relay.shed {
    Normal ->
      case length > relay_coalesce_above {
        True -> {
          let _ = process.send_after(relay.ticks, relay_tick_ms(), RelayTick)
          Relay(..relay, shed: Coalescing(dict.new(), []))
        }
        False -> relay
      }
    Coalescing(pending, order) ->
      case length < relay_flush_below {
        True -> {
          order
          |> list.reverse
          |> list.each(fn(user) {
            case dict.get(pending, user) {
              Ok(event) ->
                counters.delivered(
                  counters.RelayedDelivery,
                  deliver_local(event),
                )
              Error(Nil) -> Nil
            }
          })
          Relay(..relay, shed: Normal)
        }
        False -> relay
      }
  }
}

fn drain_name(target: Atom) -> Name(DrainMsg) {
  route.local_name("drain_" <> atom.to_string(target))
}

fn ensure_drain(target: Atom) -> Result(Pid, Nil) {
  case process.named(drain_name(target)) {
    Ok(pid) -> Ok(pid)
    Error(Nil) -> {
      let ready = process.new_subject()
      let _ = process.spawn_unlinked(fn() { run_drain(target, ready) })
      process.receive(ready, drain_start_timeout_ms())
      |> result.flatten
    }
  }
}

fn run_drain(target: Atom, ready: Subject(Result(Pid, Nil))) -> Nil {
  case process.register(process.self(), drain_name(target)) {
    Error(Nil) -> process.send(ready, process.named(drain_name(target)))
    Ok(Nil) -> {
      process.send(ready, Ok(process.self()))
      let inbox = process.named_subject(drain_name(target))
      let _ = process.send_after(inbox, drain_retry_ms(), Retry)
      drain_loop(Drain(node: target, inbox:, pending: dict.new(), order: []))
    }
  }
}

fn drain_loop(drain: Drain) -> Nil {
  case process.receive_forever(drain.inbox) {
    Push(shard, message) -> drain_loop(buffer(drain, shard, message))
    Retry -> {
      let drain = flush_drain(drain)
      case drain.order {
        [] -> {
          let _ = process.unregister(drain_name(drain.node))
          linger(drain)
        }
        _ -> {
          let _ = process.send_after(drain.inbox, drain_retry_ms(), Retry)
          drain_loop(drain)
        }
      }
    }
  }
}

fn message_key(message: RelayMsg) -> DrainKey {
  case message {
    Deliver(event) -> Broadcast(event.user)
    DeliverTo(event, to) -> Directed(event.user, to)
    RelayTick -> Tick
  }
}

fn buffer(drain: Drain, shard: Int, message: RelayMsg) -> Drain {
  let key = message_key(message)
  case dict.get(drain.pending, key) {
    Ok(#(_, older)) -> {
      counters.incr(counters.Coalesced)
      Drain(
        ..drain,
        pending: dict.insert(drain.pending, key, #(
          shard,
          merge_message(older, message),
        )),
        order: list.append(
          list.filter(drain.order, fn(found) { found != key }),
          [
            key,
          ],
        ),
      )
    }
    Error(Nil) ->
      case dict.size(drain.pending) >= drain_max_targets {
        True -> {
          counters.incr(counters.DrainDropped)
          drain
        }
        False ->
          Drain(
            ..drain,
            pending: dict.insert(drain.pending, key, #(shard, message)),
            order: list.append(drain.order, [key]),
          )
      }
  }
}

fn merge_message(older: RelayMsg, newer: RelayMsg) -> RelayMsg {
  case older, newer {
    Deliver(before), Deliver(after) -> Deliver(event.merge(before, after))
    DeliverTo(before, _), DeliverTo(after, to) ->
      DeliverTo(event.merge(before, after), to)
    _, _ -> newer
  }
}

fn flush_drain(drain: Drain) -> Drain {
  case drain.order {
    [] -> drain
    [key, ..rest] ->
      case dict.get(drain.pending, key) {
        Error(Nil) -> flush_drain(Drain(..drain, order: rest))
        Ok(#(shard, message)) ->
          case remote.send(drain.node, relay_name(shard), message) {
            Error(remote.CallBusy) -> drain
            Ok(Nil) | Error(_) ->
              flush_drain(
                Drain(
                  ..drain,
                  pending: dict.delete(drain.pending, key),
                  order: rest,
                ),
              )
          }
      }
  }
}

fn linger(drain: Drain) -> Nil {
  case process.receive(drain.inbox, drain_linger_ms()) {
    Error(Nil) -> Nil
    Ok(Retry) -> linger(drain)
    Ok(Push(shard, message)) -> {
      let drain = flush_drain(buffer(drain, shard, message))
      case drain.order {
        [] -> linger(drain)
        _ -> revive(drain)
      }
    }
  }
}

fn revive(drain: Drain) -> Nil {
  case process.register(process.self(), drain_name(drain.node)) {
    Ok(Nil) -> {
      let _ = process.send_after(drain.inbox, drain_retry_ms(), Retry)
      drain_loop(drain)
    }
    Error(Nil) ->
      case process.named(drain_name(drain.node)) {
        Ok(other) -> {
          list.each(drain.order, fn(key) {
            case dict.get(drain.pending, key) {
              Ok(#(shard, message)) ->
                push_to(other, drain.node, Push(shard, message))
              Error(Nil) -> Nil
            }
          })
          linger(Drain(..drain, pending: dict.new(), order: []))
        }
        Error(Nil) -> revive(drain)
      }
  }
}
