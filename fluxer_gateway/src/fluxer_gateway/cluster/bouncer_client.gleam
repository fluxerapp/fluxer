// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/codec_range.{type CodecRange}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease as proto_lease
import bouncer_proto/message.{
  type ToBouncer, type ToWorker, Beat, BeatAck, BouncerLeaving, BucketDelta,
  CloseRefused, DirectoryDelta, Granted, Hello, Welcome, WorkerBeat, WorkerBye,
  WorkerClaim, WorkerHello,
}
import bouncer_proto/rendezvous
import bouncer_proto/transport
import bouncer_proto/worker.{type WorkerId, Profile, WorkerId}
import fluxer_gateway/cluster/bouncer_view.{type View, Drop, Take}
import fluxer_gateway/cluster/buckets
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/lease_probe
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/metrics as cluster_metrics
import fluxer_gateway/cluster/router
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/build_info
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/handoff/move as handoff_move
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/util/table
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/set.{type Set}
import logging

pub type ClientConfig {
  ClientConfig(
    dns_name: String,
    basename: String,
    count: Int,
    role: String,
    pool: String,
    template_hash: String,
    build: String,
    codecs: List(CodecRange),
    pinned: List(Int),
  )
}

pub type BouncerView {
  BouncerView(welcomed: List(Int), majority: Bool, synced: Bool)
}

pub type ClientError {
  NoBouncers
  NotWelcomed
}

pub opaque type Message {
  FromBouncer(ToWorker)
  Tick
  Dialed(ordinal: Int)
  Probed(started_mono: Int, isolated: Bool)
  Ask(reply: Subject(BouncerView))
  SayBye(reply: Subject(Nil))
}

type State {
  State(
    self: WorkerId,
    hello: message.Hello,
    view: View,
    inbox: Subject(Message),
    dialing: Set(Int),
    probing: Bool,
    isolated_since: Option(Int),
    fenced: Dict(BucketId, Epoch),
    rewelcomed: Option(Set(Int)),
    homes: List(Int),
  )
}

type Control {
  Control
}

type Homes {
  Homes
}

type Fenced {
  Fenced
}

type LastFence {
  LastFence
}

fn control_table() -> table.Table {
  table.named("fluxer_gateway_bouncer_client")
}

fn watchers_table() -> table.Table {
  table.named("fluxer_gateway_bouncer_watchers")
}

pub fn config_from(
  settings: Settings,
  codecs: List(CodecRange),
) -> Result(ClientConfig, ClientError) {
  case settings.bouncer {
    None -> Error(NoBouncers)
    Some(bouncer) ->
      Ok(
        ClientConfig(
          dns_name: bouncer.dns_name,
          basename: bouncer.basename,
          count: bouncer.count,
          role: node.role_name(settings.role),
          pool: bouncer.pool,
          template_hash: bouncer.template_hash,
          build: build_info.version_header().1,
          codecs:,
          pinned: case bouncer.pool == "default" {
            True -> []
            False ->
              list.index_map(settings.pinned_guild_ids, fn(_, index) { index })
          },
        ),
      )
  }
}

pub fn supervised(
  settings: Settings,
  codecs: List(CodecRange),
) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    case config_from(settings, codecs) {
      Ok(config) ->
        start(config)
        |> result.map(fn(started) { actor.Started(started.pid, Nil) })
      Error(_) -> Error(actor.InitFailed("no bouncer tier is configured"))
    }
  })
}

pub fn bouncer_nodes(config: ClientConfig) -> List(Atom) {
  int.range(from: config.count - 1, to: -1, with: [], run: fn(acc, ordinal) {
    [
      transport.bouncer_node(
        basename: config.basename,
        dns_name: config.dns_name,
        ordinal:,
      ),
      ..acc
    ]
  })
}

pub fn start(
  config: ClientConfig,
) -> Result(actor.Started(Subject(ToWorker)), actor.StartError) {
  start_with(config, bouncer_nodes(config))
}

pub fn start_with(
  config: ClientConfig,
  bouncers: List(Atom),
) -> Result(actor.Started(Subject(ToWorker)), actor.StartError) {
  actor.new_with_initialiser(5000, fn(inbox) { init(config, bouncers, inbox) })
  |> actor.on_message(handle)
  |> actor.start
}

fn init(
  config: ClientConfig,
  bouncers: List(Atom),
  inbox: Subject(Message),
) -> Result(actor.Initialised(State, Message, Subject(ToWorker)), String) {
  let tables =
    lease.start() == Ok(Nil)
    && buckets.start() == Ok(Nil)
    && links.start()
    && table.ensure_public(control_table())
    && table.ensure_public(watchers_table())
  case
    tables,
    bouncers,
    process.register(process.self(), transport.worker_name())
  {
    False, _, _ -> Error("bouncer client tables could not be created")
    _, [], _ -> Error("no bouncer nodes are configured")
    _, _, Error(Nil) -> Error("a bouncer client is already registered")
    True, _, Ok(Nil) -> {
      let self = WorkerId(node.to_atom(node.self()), lease.incarnation())
      let from_bouncers = process.named_subject(transport.worker_name())
      table.insert(control_table(), #(Control, inbox))
      let state =
        State(
          self:,
          hello: Hello(
            worker: self,
            profile: Profile(
              role: config.role,
              pool: config.pool,
              build: config.build,
              template_hash: config.template_hash,
              wire_epoch: build_info.wire_epoch,
              codecs: config.codecs,
            ),
            protocol: bouncer_proto.own_range(),
            pinned: config.pinned,
          ),
          view: bouncer_view.new(bouncers),
          inbox:,
          dialing: set.new(),
          probing: False,
          isolated_since: None,
          fenced: dict.new(),
          rewelcomed: None,
          homes: [],
        )
      table.insert(control_table(), #(Homes, []))
      table.insert(control_table(), #(Fenced, False))
      process.send(inbox, Tick)
      state
      |> actor.initialised
      |> actor.selecting(
        process.new_selector()
        |> process.select(inbox)
        |> process.select_map(from_bouncers, FromBouncer),
      )
      |> actor.returning(from_bouncers)
      |> Ok
    }
  }
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  let now = clock.monotonic_ms()
  let state = case message {
    Tick -> tick(state, now)
    Dialed(ordinal) ->
      State(..state, dialing: set.delete(state.dialing, ordinal))
    Probed(started_mono:, isolated:) ->
      probed(state, started_mono, isolated, now)
    FromBouncer(incoming) -> from_bouncer(state, incoming, now)
    Ask(reply) -> {
      process.send(reply, snapshot(state.view))
      state
    }
    SayBye(reply) -> {
      each_bouncer(state, WorkerBye(state.self))
      process.send(reply, Nil)
      state
    }
  }
  actor.continue(publish_homes(state))
}

fn publish_homes(state: State) -> State {
  let welcomed = bouncer_view.welcomed(state.view) |> list.sort(int.compare)
  case welcomed == state.homes {
    True -> state
    False -> {
      table.insert(control_table(), #(
        Homes,
        list.filter_map(welcomed, fn(ordinal) {
          bouncer_view.node_of(state.view, ordinal)
          |> result.map(fn(found) { #(ordinal, found) })
        }),
      ))
      handoff_move.home_changed_local()
      State(..state, homes: welcomed)
    }
  }
}

pub fn home(bucket: BucketId) -> Result(Atom, Nil) {
  let rows: List(#(Homes, List(#(Int, Atom)))) =
    table.lookup(control_table(), Homes)
  case rows {
    [#(_, live), ..] -> {
      use found <- result.try(rendezvous.homes(
        bucket,
        list.map(live, fn(pair) { pair.0 }),
      ))
      list.key_find(live, found.home)
    }
    [] -> Error(Nil)
  }
}

pub fn bouncers_seen() -> Int {
  let rows: List(#(Homes, List(#(Int, Atom)))) =
    table.lookup(control_table(), Homes)
  case rows {
    [#(_, live), ..] -> list.length(live)
    [] -> 0
  }
}

pub fn last_fence_ms() -> Result(Int, Nil) {
  let rows: List(#(LastFence, Int)) = table.lookup(control_table(), LastFence)
  case rows {
    [#(_, at), ..] -> Ok(at)
    [] -> Error(Nil)
  }
}

pub fn fenced() -> Bool {
  let rows: List(#(Fenced, Bool)) = table.lookup(control_table(), Fenced)
  case rows {
    [#(_, fenced), ..] -> fenced
    [] -> False
  }
}

pub fn to_home(bucket: BucketId, outgoing: ToBouncer) -> Result(Nil, Nil) {
  use target <- result.try(home(bucket))
  transport.deliver(
    to: target,
    name: transport.bouncer_name(),
    message: outgoing,
  )
  |> result.replace_error(Nil)
}

fn snapshot(view: View) -> BouncerView {
  let majority = bouncer_view.majority(view)
  BouncerView(
    welcomed: bouncer_view.welcomed(view),
    majority:,
    synced: majority,
  )
}

fn each_bouncer(state: State, outgoing: ToBouncer) -> Nil {
  list.each(bouncer_view.nodes(state.view), fn(pair) {
    let _ =
      transport.send(
        to: pair.1,
        name: transport.bouncer_name(),
        message: outgoing,
      )
    Nil
  })
}

fn to_bouncer(state: State, ordinal: Int, outgoing: ToBouncer) -> Nil {
  case bouncer_view.node_of(state.view, ordinal) {
    Ok(target) -> {
      let _ =
        transport.send(
          to: target,
          name: transport.bouncer_name(),
          message: outgoing,
        )
      Nil
    }
    Error(Nil) -> Nil
  }
}

fn tick(state: State, now: Int) -> State {
  let timings = durations.bouncer()
  let view =
    bouncer_view.silent(state.view, now, timings.lease_ttl_ms)
    |> list.fold(state.view, bouncer_view.drop)
  let state = dial(State(..state, view:))
  let #(view, seq) = bouncer_view.beat(state.view, now, timings.lease_ttl_ms)
  let held = lease.held()
  each_bouncer(
    State(..state, view:),
    WorkerBeat(
      Beat(
        worker: state.self,
        seq:,
        sent_mono: now,
        held_digest: proto_lease.held_digest(held),
        roster_hash: case bouncer_view.roster(view) {
          Some(roster) -> roster.hash
          None -> 0
        },
        draining: draining.is_draining(),
        load: list.length(held),
        unreachable: [],
      ),
    ),
  )
  let _ = buckets.expire_routes(now)
  publish_joined(view, now)
  process.send_after(state.inbox, timings.beat_interval_ms, Tick)
  isolation(State(..state, view:), held, now)
}

fn dial(state: State) -> State {
  let waiting =
    bouncer_view.unwelcomed(state.view)
    |> list.filter(fn(ordinal) { !set.contains(state.dialing, ordinal) })
  list.each(waiting, fn(ordinal) {
    case bouncer_view.node_of(state.view, ordinal) {
      Ok(target) -> {
        let hello = WorkerHello(state.hello)
        let inbox = state.inbox
        process.spawn_unlinked(fn() {
          let _ = case
            target == node.to_atom(node.self()) || transport.connect(target)
          {
            True ->
              transport.send(
                to: target,
                name: transport.bouncer_name(),
                message: hello,
              )
            False -> Error(transport.NotConnected)
          }
          process.send(inbox, Dialed(ordinal))
        })
        Nil
      }
      Error(Nil) -> Nil
    }
  })
  State(..state, dialing: set.union(state.dialing, set.from_list(waiting)))
}

fn isolation(state: State, held: List(proto_lease.Claim), now: Int) -> State {
  case lease.sees_bouncers(now), held, lease.lapsed_at() {
    False, [_, ..], Ok(lapsed) -> {
      case state.isolated_since {
        None -> rollout_metrics.count(rollout_metrics.lease_lapses, [], 1)
        Some(_) -> Nil
      }
      let since = option.unwrap(state.isolated_since, lapsed)
      let state = State(..state, isolated_since: Some(since))
      case now - since >= durations.bouncer().isolation_cap_ms, state.probing {
        True, _ -> fence(state, "isolation cap reached", "isolation_cap")
        False, True -> state
        False, False -> {
          let inbox = state.inbox
          let rosters = lease.rosters(now)
          process.spawn_unlinked(fn() {
            process.send(inbox, Probed(now, lease_probe.round(rosters)))
          })
          State(..state, probing: True)
        }
      }
    }
    _, _, _ ->
      case state.isolated_since {
        Some(_) -> {
          lease.end_grace()
          State(..state, isolated_since: None)
        }
        None -> state
      }
  }
}

fn probed(state: State, started: Int, isolated: Bool, now: Int) -> State {
  cluster_metrics.record_probe(isolated, now - started)
  let state = State(..state, probing: False)
  case state.isolated_since, lease.sees_bouncers(now), isolated {
    None, _, _ | _, True, _ -> state
    Some(since), False, True -> {
      let timings = durations.bouncer()
      lease.grace(int.min(
        started + 2 * timings.beat_interval_ms + timings.probe_timeout_ms,
        since + timings.isolation_cap_ms,
      ))
      state
    }
    Some(_), False, False ->
      fence(
        state,
        "a roster majority still sees bouncers",
        "majority_sees_bouncers",
      )
  }
}

fn fence(state: State, reason: String, label: String) -> State {
  let fenced = lease.fence()
  table.insert(control_table(), #(Fenced, True))
  table.insert(control_table(), #(LastFence, clock.monotonic_ms()))
  links.release_serving()
  rollout_metrics.count(rollout_metrics.fences, [#("reason", label)], 1)
  logging.log(
    logging.Warning,
    "bouncer client fenced "
      <> int.to_string(list.length(fenced))
      <> " buckets: "
      <> reason,
  )
  router.notify_watchers()
  State(
    ..state,
    isolated_since: None,
    view: state.view
      |> bouncer_view.nodes
      |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
      |> list.map(fn(pair) { pair.1 })
      |> bouncer_view.new,
    dialing: set.new(),
    rewelcomed: Some(set.new()),
    fenced: list.fold(fenced, state.fenced, fn(acc, claim) {
      dict.insert(acc, claim.bucket, claim.epoch)
    }),
  )
}

fn rewelcome(state: State, bouncer: Int) -> State {
  case state.rewelcomed {
    None -> state
    Some(seen) -> {
      let seen = set.insert(seen, bouncer)
      case
        proto_lease.has_quorum(
          set.size(seen),
          bouncer_view.configured(state.view),
        )
      {
        True -> {
          table.insert(control_table(), #(Fenced, False))
          rollout_metrics.count(
            rollout_metrics.unfences,
            [#("reason", "rewelcomed")],
            1,
          )
          State(..state, fenced: dict.new(), rewelcomed: None)
        }
        False -> State(..state, rewelcomed: Some(seen))
      }
    }
  }
}

fn publish_joined(view: View, now: Int) -> Nil {
  let joined = bouncer_view.majority(view)
  case links.joined() == joined {
    True -> Nil
    False -> {
      case joined {
        True -> links.release_serving()
        False -> links.hold_serving(now + durations.bouncer().isolation_cap_ms)
      }
      links.set_joined(joined)
      router.notify_watchers()
    }
  }
}

fn from_bouncer(state: State, incoming: ToWorker, now: Int) -> State {
  case incoming {
    Welcome(bouncer:, incarnation:, directory:, records:, roster:, ..) -> {
      let view =
        bouncer_view.welcome(
          state.view,
          bouncer,
          incarnation,
          directory,
          roster,
          now,
        )
      let _ = lease.remember_roster(roster, now)
      let state = State(..state, view:) |> rewelcome(bouncer)
      to_bouncer(state, bouncer, WorkerClaim(state.self, lease.held()))
      publish_joined(view, now)
      state |> directory_changed |> apply_records(records)
    }
    BeatAck(bouncer:, seq:, matched:, roster:) -> {
      let #(view, acked) =
        bouncer_view.ack(state.view, bouncer, seq, matched, roster, now)
      case acked {
        Some(sent_mono) -> {
          cluster_metrics.record_beat_rtt(now - sent_mono)
          let _ = lease.extend(sent_mono)
          let _ = lease.remember_roster(roster, now)
          Nil
        }
        None -> Nil
      }
      cluster_metrics.record_beat_ack(matched)
      case matched {
        True -> Nil
        False -> {
          metrics.counter(
            "fluxer_gateway_bouncer_beat_mismatch_total",
            [#("bouncer", int.to_string(bouncer))],
            1,
          )
          to_bouncer(state, bouncer, WorkerClaim(state.self, lease.held()))
        }
      }
      State(..state, view:)
    }
    DirectoryDelta(bouncer:, changed:, removed:) ->
      State(
        ..state,
        view: bouncer_view.directory_delta(
          state.view,
          bouncer,
          changed,
          removed,
          now,
        ),
      )
      |> directory_changed
    BucketDelta(records:, ..) -> apply_records(state, records)
    Granted(bucket:, epoch:, ..) ->
      case
        bouncer_view.accepts_grant(
          bucket,
          epoch,
          buckets.record(bucket) |> result.replace_error(Nil),
          state.fenced,
        )
      {
        False -> state
        True -> {
          let _ = lease.grant(bucket, epoch)
          notify(incoming)
          router.notify_watchers()
          state
        }
      }
    CloseRefused(..) -> {
      notify(incoming)
      state
    }
    BouncerLeaving(ordinal) ->
      State(..state, view: bouncer_view.drop(state.view, ordinal))
      |> directory_changed
  }
}

fn directory_changed(state: State) -> State {
  let entries = bouncer_view.directory(state.view)
  case links.replace(entries) {
    True -> {
      let _ = wire.observe(entries)
      router.notify_watchers()
    }
    False -> Nil
  }
  state
}

fn apply_records(
  state: State,
  records: List(proto_lease.BucketRecord),
) -> State {
  let _ = buckets.apply(records)
  let held = lease.held()
  let touched =
    list.append(
      list.map(records, fn(found) { found.bucket }),
      list.map(held, fn(claim) { claim.bucket }),
    )
    |> list.unique
    |> list.filter_map(fn(bucket) {
      buckets.record(bucket) |> result.replace_error(Nil)
    })
  let changes = bouncer_view.reconcile(state.self, held, touched, state.fenced)
  list.each(changes, fn(change) {
    let _ = case change {
      Take(bucket, epoch) -> lease.grant(bucket, epoch)
      Drop(bucket) -> lease.release(bucket)
    }
    Nil
  })
  case records, changes {
    [], [] -> Nil
    _, _ -> router.notify_watchers()
  }
  state
}

fn control() -> Result(Subject(Message), ClientError) {
  let rows: List(#(Control, Subject(Message))) =
    table.lookup(control_table(), Control)
  case rows {
    [#(_, inbox), ..] ->
      case process.subject_owner(inbox) {
        Ok(pid) ->
          case process.is_alive(pid) {
            True -> Ok(inbox)
            False -> Error(NoBouncers)
          }
        Error(Nil) -> Error(NoBouncers)
      }
    [] -> Error(NoBouncers)
  }
}

pub fn view() -> Result(BouncerView, ClientError) {
  use inbox <- result.map(control())
  process.call(inbox, durations.bouncer().probe_timeout_ms, Ask)
}

pub fn set_draining(draining: Bool) -> Result(Nil, ClientError) {
  use _ <- result.map(control())
  case draining {
    True -> draining.set_draining()
    False -> draining.clear_draining()
  }
}

pub fn bye() -> Result(Nil, ClientError) {
  use inbox <- result.map(control())
  process.call(inbox, durations.bouncer().probe_timeout_ms, SayBye)
}

pub fn subscribe(watcher: Subject(ToWorker)) -> Result(Nil, ClientError) {
  case process.subject_owner(watcher), table.ensure_public(watchers_table()) {
    Ok(pid), True -> Ok(table.insert(watchers_table(), #(pid, watcher)))
    _, _ -> Error(NotWelcomed)
  }
}

fn notify(incoming: ToWorker) -> Nil {
  let rows: List(#(Pid, Subject(ToWorker))) = table.to_list(watchers_table())
  list.each(rows, fn(row) {
    case process.is_alive(row.0) {
      True -> process.send(row.1, incoming)
      False -> table.delete(watchers_table(), row.0)
    }
  })
}
