// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/discovery
import fluxer_gateway/cluster/liveness
import fluxer_gateway/cluster/membership/state.{type Membership}
import fluxer_gateway/cluster/metrics
import fluxer_gateway/cluster/node_metadata
import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/node.{type Node, type Role}
import fluxer_gateway/telemetry/metrics as telemetry
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import logging

fn init_timeout_ms() -> Int {
  durations.cluster().membership_init_timeout_ms
}

fn tick_ms() -> Int {
  durations.cluster().membership_tick_ms
}

fn probe_timeout_ms() -> Int {
  durations.cluster().membership_probe_timeout_ms
}

pub type Move {
  MoveOut(targets: List(Node), reason: state.MoveReason)
  MoveIn(from: Node, reason: state.MoveReason)
}

pub opaque type Message {
  Tick
  NodeEvent(NodeEvent)
  Discovered(discovery.PeersChanged)
  PeerHello(
    from: Node,
    role: Role,
    pod_name: String,
    pins: List(Int),
    draining: Bool,
    wants_reply: Bool,
  )
  PeerDraining(from: Node)
  PeerUndrained(from: Node)
  ProbeRequest(target: Node, reply_to: Node)
  ProbeResult(target: Node, from: Node, reachable: Bool)
  AnnounceDrain
  AnnounceUndrain
  SubscribeMoves(Subject(Move))
  MonitorDown(process.Down)
  SubscribeDiscovery
}

type NodeEvent {
  Joined(Node)
  Left(Node, state.DownReason)
  Unreadable
}

type MonitorOption {
  NodedownReason
}

type State {
  State(
    self: Subject(Message),
    membership: Membership,
    role: Role,
    pins: List(Int),
    moves: List(#(Pid, Subject(Move))),
    discovered: Subject(discovery.PeersChanged),
    discovery: Option(Pid),
  )
}

@external(erlang, "net_kernel", "monitor_nodes")
fn monitor_nodes(flag: Bool, options: List(MonitorOption)) -> Atom

@external(erlang, "erlang", "nodes")
fn connected_atoms() -> List(Atom)

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_cluster_membership")
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings) })
}

fn start(settings: Settings) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    liveness.init(list.map(connected_atoms(), node.from_atom))
    let _ = monitor_nodes(True, [NodedownReason])
    let pins = list.map(settings.pinned_guild_ids, id.guild_id_to_int)
    let config =
      state.Config(
        ..state.default_config(),
        seed: unique_integer() + clock.system_ms(),
      )
    let #(membership, decisions) =
      state.start(
        node.self(),
        settings.role,
        pins,
        config,
        router.published(),
        list.map(connected_atoms(), node.from_atom),
        draining.is_draining(),
        discovery.configured(settings),
        clock.monotonic_ms(),
      )
    let discovered = process.new_subject()
    let current =
      State(
        self:,
        membership:,
        role: settings.role,
        pins:,
        moves: [],
        discovered:,
        discovery: None,
      )
      |> subscribe_discovery
    let current = run(current, decisions)
    list.each(connected_atoms(), fn(peer) {
      process.send(self, NodeEvent(Joined(node.from_atom(peer))))
    })
    process.send_after(self, tick_ms(), Tick)
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_map(discovered, Discovered)
      |> process.select_record(atom.create("nodeup"), 2, fn(raw) {
        NodeEvent(
          decode.run(raw, node_event(True)) |> result.unwrap(Unreadable),
        )
      })
      |> process.select_record(atom.create("nodedown"), 2, fn(raw) {
        NodeEvent(
          decode.run(raw, node_event(False)) |> result.unwrap(Unreadable),
        )
      })
      |> process.select_monitors(MonitorDown)
    current
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

fn node_event(up: Bool) -> Decoder(NodeEvent) {
  use peer <- decode.field(1, atom.decoder())
  case up {
    True -> decode.success(Joined(node.from_atom(peer)))
    False -> {
      use reason <- decode.then(down_reason())
      decode.success(Left(node.from_atom(peer), reason))
    }
  }
}

fn down_reason() -> Decoder(state.DownReason) {
  decode.at([2], decode.list(decode.at([1], atom.decoder())))
  |> decode.map(fn(reasons) {
    case list.map(reasons, atom.to_string) {
      ["connection_closed", ..] -> state.ConnectionClosed
      ["net_tick_timeout", ..] -> state.NetTickTimeout
      ["disconnect", ..] -> state.Disconnect
      _ -> state.OtherReason
    }
  })
  |> decode.one_of([decode.success(state.OtherReason)])
}

pub fn announce_drain() -> Result(Nil, Nil) {
  remote.send_local(name(), AnnounceDrain)
}

pub fn announce_undrain() -> Result(Nil, Nil) {
  remote.send_local(name(), AnnounceUndrain)
}

pub fn subscribe_moves(to subscriber: Subject(Move)) -> Result(Pid, Nil) {
  use pid <- result.try(process.named(name()))
  use _ <- result.map(remote.send_local(name(), SubscribeMoves(subscriber)))
  pid
}

fn handle(current: State, message: Message) -> actor.Next(State, Message) {
  let now = clock.monotonic_ms()
  case message {
    Tick -> {
      process.send_after(current.self, tick_ms(), Tick)
      actor.continue(step(current, state.Tick(now), now))
    }
    NodeEvent(Joined(peer)) ->
      actor.continue(step(current, state.NodeUp(peer), now))
    NodeEvent(Left(peer, reason)) ->
      actor.continue(step(current, state.NodeDown(peer, reason), now))
    NodeEvent(Unreadable) -> actor.continue(current)
    Discovered(discovery.PeersChanged(peers)) ->
      actor.continue(step(current, state.DnsAnswer(peers), now))
    PeerHello(from, role, pod_name, pins, draining, wants_reply) -> {
      node_metadata.cache_node_pod_name(from, pod_name)
      actor.continue(step(
        current,
        state.PeerInfo(from, role, pins, draining, wants_reply),
        now,
      ))
    }
    PeerDraining(from) ->
      actor.continue(step(current, state.DrainAnnounced(from), now))
    PeerUndrained(from) ->
      actor.continue(step(current, state.UndrainAnnounced(from), now))
    ProbeRequest(target, reply_to) -> {
      probe(target, reply_to)
      actor.continue(current)
    }
    ProbeResult(target, from, reachable) ->
      actor.continue(step(
        current,
        state.ProbeReply(target, from, reachable),
        now,
      ))
    AnnounceDrain -> {
      let peers = list.map(connected_atoms(), node.from_atom)
      deliver_all(peers, PeerDraining(node.self()))
      actor.continue(step(current, state.DrainAnnounced(node.self()), now))
    }
    AnnounceUndrain -> {
      let peers = list.map(connected_atoms(), node.from_atom)
      deliver_all(peers, PeerUndrained(node.self()))
      actor.continue(step(current, state.UndrainAnnounced(node.self()), now))
    }
    SubscribeMoves(subscriber) -> actor.continue(add_moves(current, subscriber))
    MonitorDown(process.ProcessDown(pid:, ..)) ->
      case current.discovery == Some(pid) {
        True ->
          actor.continue(subscribe_discovery(State(..current, discovery: None)))
        False ->
          actor.continue(
            State(
              ..current,
              moves: list.filter(current.moves, fn(entry) { entry.0 != pid }),
            ),
          )
      }
    MonitorDown(process.PortDown(..)) -> actor.continue(current)
    SubscribeDiscovery -> actor.continue(subscribe_discovery(current))
  }
}

fn subscribe_discovery(current: State) -> State {
  case current.discovery {
    Some(_) -> current
    None ->
      case discovery.subscribe(current.discovered) {
        Ok(pid) -> {
          let _ = process.monitor(pid)
          State(..current, discovery: Some(pid))
        }
        Error(Nil) -> {
          process.send_after(current.self, tick_ms(), SubscribeDiscovery)
          current
        }
      }
  }
}

fn deliver_all(peers: List(Node), message: Message) -> Nil {
  list.each(peers, fn(peer) { remote.deliver_named(peer, name(), message) })
}

fn add_moves(current: State, subscriber: Subject(Move)) -> State {
  case process.subject_owner(subscriber) {
    Ok(pid) -> {
      let #(known, others) =
        list.partition(current.moves, fn(entry) { entry.0 == pid })
      case known {
        [] -> {
          let _ = process.monitor(pid)
          Nil
        }
        _ -> Nil
      }
      case state.drain_targets(current.membership) {
        Ok(targets) ->
          process.send(subscriber, MoveOut(targets, state.MovedDraining))
        Error(Nil) -> Nil
      }
      State(..current, moves: [#(pid, subscriber), ..others])
    }
    Error(Nil) -> current
  }
}

fn step(current: State, event: state.Event, now: Int) -> State {
  let #(membership, decisions) = state.step(current.membership, event, now)
  run(State(..current, membership:), decisions)
}

fn run(current: State, decisions: List(state.Decision)) -> State {
  list.each(decisions, fn(decision) { execute(current, decision) })
  current
}

fn execute(current: State, decision: state.Decision) -> Nil {
  case decision {
    state.Publish(published) -> router.publish(published)
    state.SetLiveness(peer, up) -> {
      set_liveness(peer, up)
      metrics.record_membership_transition(up)
    }
    state.Reconnect(peer) -> {
      process.spawn_unlinked(fn() { remote.connect(peer) })
      Nil
    }
    state.Probe(target, via) ->
      remote.broadcast_named(via, name(), ProbeRequest(target, node.self()))
    state.SendInfo(peer, draining, wants_reply) -> {
      let _ =
        remote.deliver_named(
          peer,
          name(),
          PeerHello(
            node.self(),
            current.role,
            node_metadata.local_pod_name(),
            current.pins,
            draining,
            wants_reply,
          ),
        )
      Nil
    }
    state.RefreshDiscovery -> discovery.refresh()
    state.MoveOut(targets, reason) ->
      notify_moves(current, MoveOut(targets, reason))
    state.MoveIn(from, reason) -> notify_moves(current, MoveIn(from, reason))
    state.Forget(peer) -> {
      set_liveness(peer, False)
      metrics.forget_peer(node.to_string(peer))
    }
    state.Count(metric) -> count(metric)
  }
}

fn set_liveness(peer: Node, up: Bool) -> Nil {
  let changed = liveness.is_up(peer) != up
  liveness.set(peer, up)
  case changed {
    True -> router.notify_watchers()
    False -> Nil
  }
}

fn notify_moves(current: State, move: Move) -> Nil {
  list.each(current.moves, fn(entry) { process.send(entry.1, move) })
}

fn probe(target: Node, reply_to: Node) -> Nil {
  process.spawn_unlinked(fn() {
    let answer = process.new_subject()
    process.spawn_unlinked(fn() { process.send(answer, remote.connect(target)) })
    let reachable = case process.receive(answer, probe_timeout_ms()) {
      Ok(found) -> found
      Error(Nil) -> False
    }
    let _ =
      remote.send_named(
        reply_to,
        name(),
        ProbeResult(target, node.self(), reachable),
      )
    Nil
  })
  Nil
}

pub fn move_reason(reason: state.MoveReason) -> String {
  case reason {
    state.MovedGone -> "gone"
    state.MovedDraining -> "draining"
    state.MovedJoin -> "join"
  }
}

fn count(metric: state.Metric) -> Nil {
  case metric {
    state.NodeDownSeen(peer, reason) ->
      metrics.record_nodedown(node.to_string(peer), reason_text(reason))
    state.DownDuration(_, ms) -> metrics.record_down_duration(ms)
    state.PeerStateChanged(peer, peer_state) -> {
      logging.log(
        logging.Info,
        "Gateway cluster peer "
          <> node.to_string(peer)
          <> " is now "
          <> state_text(peer_state),
      )
      list.each(["up", "suspect", "gone", "joining", "draining"], fn(text) {
        let value = case text == state_text(peer_state) {
          True -> 1
          False -> 0
        }
        metrics.record_peer_state(node.to_string(peer), text, value)
      })
    }
    state.GoneVetoed(_) -> metrics.record_gone_vetoed()
    state.TopologyPublished -> metrics.record_topology_publish()
    state.MembersChanged(size) -> {
      metrics.set_member_count(size)
      metrics.record_mesh_size(size)
    }
    state.Moves(reason, targets) ->
      telemetry.counter(
        "fluxer_gateway_cluster_move_decisions_total",
        [#("reason", move_reason(reason))],
        targets,
      )
  }
}

fn reason_text(reason: state.DownReason) -> String {
  case reason {
    state.ConnectionClosed -> "connection_closed"
    state.NetTickTimeout -> "net_tick_timeout"
    state.Disconnect -> "disconnect"
    state.OtherReason -> "other"
  }
}

fn state_text(peer_state: state.PeerState) -> String {
  case peer_state {
    state.Up -> "up"
    state.Suspect(_) -> "suspect"
    state.Gone(_) -> "gone"
    state.Joining(_) -> "joining"
    state.Draining -> "draining"
  }
}
