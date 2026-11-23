// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/wire
import fluxer_gateway/config/durations
import fluxer_gateway/core/error
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type ClientOp, type ResumeError, type SessionInbox, type SessionRef,
  type SocketRef, type StartError, Dispatch, DispatchFrame, GuildDispatch,
  GuildDown, Heartbeat, HeartbeatAckFrame, InvalidResumeSeq, PresenceFrame,
  PresenceUpdate, ReconnectFrame, ResumeAuthFailed, Send, SessionGone,
  SessionRef, SocketDown, SocketOp, Terminate, VoiceSignal, VoiceStateUpdate,
}
import fluxer_gateway/core/user
import fluxer_gateway/gateway/limits
import fluxer_gateway/guild/handoff/signal as guild_signal
import fluxer_gateway/presence/handoff/signal as presence_signal
import fluxer_gateway/presence/protocol
import fluxer_gateway/session/arrival_log
import fluxer_gateway/session/calls
import fluxer_gateway/session/control.{type Control}
import fluxer_gateway/session/departure
import fluxer_gateway/session/dispatch.{FromGuild, FromPresence}
import fluxer_gateway/session/guilds
import fluxer_gateway/session/links
import fluxer_gateway/session/manager/leases
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/transfer
import fluxer_gateway/session/payload
import fluxer_gateway/session/pending_presences
import fluxer_gateway/session/presence_link.{NowLinked, StillDown}
import fluxer_gateway/session/reactions
import fluxer_gateway/session/readiness.{type Outcome, Continue, Stop}
import fluxer_gateway/session/replay_buffer
import fluxer_gateway/session/seed.{type Seed, Seed}
import fluxer_gateway/session/session_metrics
import fluxer_gateway/session/state.{
  type Arrival, type Held, type Internal, type State, ArrivalExpired, Arriving,
  Attached, CallReconnect, ConnectGuild, Detached, EnablePresenceUpdates,
  FlushReactions, HeldControl, HeldInbox, JoinExpired, JoinReplied, Lapsed,
  OfflineGrace, PresenceConnect, PresenceDown, PresenceReplied, PresenceTimeout,
  PresenceUp, ReadyDeadline, RepairTick, ResumeExpired, Settled, State,
} as st
import fluxer_gateway/session/subscriptions
import fluxer_gateway/session/voice
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/telemetry/timings.{type Timing}
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Down, ProcessDown}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/set

fn init_timeout_ms() -> Int {
  durations.session().server_init_timeout_ms
}

pub type Start {
  Start(
    seed: Seed,
    socket: Option(SocketRef),
    reply: Option(process.Subject(Result(SessionRef, StartError))),
  )
}

pub type Message {
  Inbox(SessionInbox)
  Ctl(Control)
  Tick(Internal)
  Watch(Down)
}

fn arrival_wait_ms() -> Int {
  durations.session().server_move_relay_ms
  + durations.session().server_rebind_ack_wait_ms
}

fn ready_deadline_ms() -> Int {
  durations.session().server_ready_deadline_ms
}

fn presence_enable_ms() -> Int {
  durations.session().server_presence_enable_ms
}

fn repair_interval_ms() -> Int {
  durations.session().server_repair_interval_ms
}

fn offline_grace_ms() -> Int {
  durations.session().server_offline_grace_ms
}

const max_inline_replay = 256

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Int) -> Int

pub fn start(start: Start) -> actor.StartResult(SessionRef) {
  actor.new_with_initialiser(init_timeout_ms(), fn(_self) { Ok(init(start)) })
  |> actor.on_message(handle)
  |> actor.start
}

fn init(start: Start) -> actor.Initialised(State, Message, SessionRef) {
  process_flag(atom.create("fullsweep_after"), 10)
  let seed = start.seed
  let inbox = process.new_subject()
  let internal = process.new_subject()
  let join_replies = process.new_subject()
  let presence_replies = process.new_subject()
  let me = SessionRef(id: seed.id, user: user_id(seed), inbox:)
  let now = st.now_ms()
  let socket = case start.socket {
    Some(socket) -> Attached(socket, process.monitor(socket.pid))
    None -> {
      session_metrics.detach(process.self())
      Detached(1)
    }
  }
  let state =
    State(
      id: seed.id,
      me:,
      user_id: user_id(seed),
      user: seed.user,
      staff: seed.staff,
      bot: is_bot(seed),
      version: seed.version,
      token_hash: seed.token_hash,
      auth_hash: seed.auth_hash,
      flags: seed.flags,
      shard: seed.shard,
      e2ee_capable: seed.e2ee_capable,
      country_code: seed.country_code,
      ignored: set.from_list(seed.ignored_events),
      initial_guild: seed.initial_guild,
      status: seed.status,
      resume_status: seed.resume_status,
      afk: seed.afk,
      mobile: seed.mobile,
      custom_status: seed.custom_status,
      settings_invisible: seed.settings_invisible,
      socket:,
      generation: 1,
      seq: seed.seq,
      ack_seq: int.min(seed.ack_seq, seed.seq),
      replay_floor: int.min(seed.replay_floor, seed.seq),
      replay: replay_buffer.from_entries(seed.buffer),
      ready: seed.ready,
      collected: [],
      links: links.new(seed.guilds),
      relations: seed.relations,
      pending_presences: pending_presences.new(),
      suppress_presences: True,
      presence_view: seed.presence_view,
      reactions: reactions.new(),
      presence: PresenceDown,
      presence_targets: set.new(),
      presence_sent: None,
      subscriptions: subscriptions.new(seed.active, seed.subscriptions),
      visits: seed.visits,
      pending_bot_joins: set.new(),
      unannounced: set.new(),
      dm_mutual: dict.new(),
      calls: dict.new(),
      call_timers: dict.new(),
      health: seed.health,
      fenced: False,
      arrival: Settled,
      ready_deadline_ms: now + links.jitter(ready_deadline_ms(), 10),
      timing: timings.start("session"),
      internal:,
      join_replies:,
      presence_replies:,
    )
  let #(state, adopted) = arrive(state, seed.moved)
  leases.open(process.self(), state.user_id)
  case seed.transferred {
    True -> voice.session_rehydrated(state)
    False -> Nil
  }
  case presence_link.is_linked(state) {
    True -> Nil
    False -> st.schedule(state, 0, PresenceConnect(0))
  }
  case state.bot {
    True -> st.schedule(state, 0, ReadyDeadline)
    False -> Nil
  }
  list.each(seed.guilds, fn(guild) {
    case list.contains(adopted, guild) {
      True -> Nil
      False -> st.schedule(state, 0, ConnectGuild(guild, 0))
    }
  })
  case seed.transferred {
    True -> {
      let expected = handed_over(seed)
      let missing = expected - list.length(adopted)
      rebinds("guild", "fallback", missing)
      fallbacks("guild", "not_adopted", missing)
      report_rejoining(seed, expected, list.length(adopted))
      case presence_link.is_linked(state) {
        True -> Nil
        False -> {
          rebinds("presence", "fallback", 1)
          fallbacks("presence", "not_adopted", 1)
        }
      }
    }
    False -> Nil
  }
  st.schedule(state, state.ready_deadline_ms - now, ReadyDeadline)
  st.schedule(state, presence_enable_ms(), EnablePresenceUpdates)
  st.schedule(state, links.jitter(repair_interval_ms(), 20), RepairTick)
  case socket {
    Detached(generation) ->
      st.schedule(state, limits.resume_timeout_ms(), ResumeExpired(generation))
    Attached(_, _) -> Nil
  }
  let selector =
    process.new_selector()
    |> process.select_map(inbox, Inbox)
    |> process.select_map(control.subject(process.self()), Ctl)
    |> process.select_map(internal, Tick)
    |> process.select_map(join_replies, fn(reply) { Tick(JoinReplied(reply)) })
    |> process.select_map(presence_replies, fn(result) {
      Tick(PresenceReplied(result))
    })
    |> process.select_monitors(Watch)
  announce(me, start.reply)
  actor.initialised(state)
  |> actor.selecting(selector)
  |> actor.returning(me)
}

fn arrive(state: State, moved: Option(seed.Moved)) -> #(State, List(GuildId)) {
  case moved, option.then(moved, fn(found) { source_pid(found.source) }) {
    Some(moved), Some(source) -> {
      let state =
        State(
          ..state,
          dm_mutual: moved.dm_mutual,
          suppress_presences: moved.suppress_presences,
          pending_presences: moved.pending_presences,
        )
      let #(state, adopted) = guilds.adopt_moved(state, moved.linked)
      let state = case moved.presence {
        Some(link) ->
          presence_link.adopt_moved(state, link, moved.presence_targets)
        None -> state
      }
      st.schedule(state, arrival_wait_ms(), ArrivalExpired)
      #(
        State(
          ..state,
          arrival: Arriving(
            source: process.monitor(source),
            guilds: adopted,
            held: [],
          ),
        ),
        adopted,
      )
    }
    _, _ -> #(state, [])
  }
}

fn source_pid(source: SessionRef) -> Option(process.Pid) {
  process.subject_owner(source.inbox) |> option.from_result
}

fn handed_over(seed: Seed) -> Int {
  case seed.moved {
    Some(moved) -> list.length(moved.linked)
    None -> list.length(seed.guilds)
  }
}

fn report_rejoining(seed: Seed, expected: Int, adopted: Int) -> Nil {
  let guilds = list.length(seed.guilds)
  case
    arrival_log.classify(
      guilds:,
      expected:,
      adopted:,
      moved: option.is_some(seed.moved),
    )
  {
    Error(Nil) -> Nil
    Ok(reason) ->
      arrival_log.warn(arrival_log.Arrival(
        session_id: session.session_id_to_string(seed.id),
        source_node: source_node(seed.moved),
        guilds:,
        expected:,
        adopted:,
        reason:,
      ))
  }
}

fn source_node(moved: Option(seed.Moved)) -> String {
  case option.then(moved, fn(found) { source_pid(found.source) }) {
    Some(pid) -> atom.to_string(pid_node(pid))
    None -> "unknown"
  }
}

@external(erlang, "erlang", "node")
fn pid_node(pid: process.Pid) -> Atom

fn announce(
  me: SessionRef,
  reply: Option(process.Subject(Result(SessionRef, StartError))),
) -> Nil {
  registry.insert(me)
  let _ =
    process_registry.register(process_registry.sessions, me.id, process.self())
  case reply {
    Some(reply) -> process.send(reply, Ok(me))
    None -> Nil
  }
}

fn user_id(seed: Seed) -> id.UserId {
  user_of(seed.user)
}

fn user_of(value: user.User) -> id.UserId {
  user.id(value)
}

fn is_bot(seed: Seed) -> Bool {
  user.is_bot(seed.user)
}

type Step {
  Go(State)
  Done
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case step(state, message) {
    Go(state) -> actor.continue(state)
    Done -> actor.stop()
  }
}

fn step(state: State, message: Message) -> Step {
  case state.arrival, message {
    Lapsed, Ctl(control.Relayed(_)) -> {
      metrics.counter("fluxer_gateway_session_late_relays_total", [], 1)
      Go(state)
    }
    Settled, _ | Lapsed, _ -> route(state, message)
    Arriving(source:, ..) as arriving, _ ->
      arriving_step(state, arriving, source, message)
  }
}

fn route(state: State, message: Message) -> Step {
  case message {
    Inbox(inbox) -> on_inbox(state, inbox)
    Ctl(control) -> on_control(state, control)
    Tick(internal) -> on_internal(state, internal)
    Watch(down) -> on_down(state, down)
  }
}

fn arriving_step(
  state: State,
  arrival: Arrival,
  source: process.Monitor,
  message: Message,
) -> Step {
  case message {
    Inbox(SocketOp(_)) | Inbox(SocketDown(_)) -> route(state, message)
    Inbox(inbox) -> Go(hold(state, arrival, HeldInbox(inbox)))
    Ctl(control.Relayed(inbox)) -> on_inbox(state, inbox)
    Ctl(control.RelayedControl(found)) -> on_control(state, found)
    Ctl(control.MoveDone(guilds, presence)) ->
      settle(state, arrival, guilds, presence, "not_rebound")
    Ctl(control.Reattach(..)) | Ctl(control.Resume(..)) -> route(state, message)
    Ctl(control.HandOff(_, reply)) -> {
      process.send(reply, False)
      Go(state)
    }
    Ctl(found) -> Go(hold(state, arrival, HeldControl(found)))
    Tick(ArrivalExpired) -> settle(state, arrival, [], False, "expired")
    Watch(ProcessDown(monitor:, ..)) if monitor == source ->
      settle(state, arrival, [], False, "source_down")
    Tick(_) | Watch(_) -> route(state, message)
  }
}

fn hold(state: State, arrival: Arrival, held: Held) -> State {
  case arrival {
    Arriving(..) ->
      State(..state, arrival: Arriving(..arrival, held: [held, ..arrival.held]))
    Settled | Lapsed -> state
  }
}

fn settle(
  state: State,
  arrival: Arrival,
  rebound: List(GuildId),
  presence: Bool,
  reason: String,
) -> Step {
  case arrival {
    Settled | Lapsed -> Go(state)
    Arriving(source:, guilds: moved, held:) -> {
      process.demonitor_process(source)
      let unmoved =
        list.filter(moved, fn(guild) { !list.contains(rebound, guild) })
      record_settle(
        list.length(moved) - list.length(unmoved),
        list.length(unmoved),
        presence_link.is_linked(state),
        presence,
        reason,
      )
      rollout_metrics.count(
        rollout_metrics.held_messages,
        [],
        list.length(held),
      )
      let state =
        unmoved
        |> list.fold(
          State(..state, arrival: case reason {
            "not_rebound" -> Settled
            _ -> Lapsed
          }),
          guilds.unmoved,
        )
        |> presence_link.after_move(presence)
      replay_held(state, list.reverse(held))
    }
  }
}

fn record_settle(
  rebound: Int,
  unmoved: Int,
  linked: Bool,
  presence_kept: Bool,
  reason: String,
) -> Nil {
  rebinds("guild", "rebound", rebound)
  rebinds("guild", "fallback", unmoved)
  fallbacks("guild", reason, unmoved)
  case linked, presence_kept {
    False, _ -> Nil
    True, True -> rebinds("presence", "rebound", 1)
    True, False -> {
      rebinds("presence", "fallback", 1)
      fallbacks("presence", reason, 1)
    }
  }
}

fn rebinds(kind: String, outcome: String, by: Int) -> Nil {
  rollout_metrics.count(
    rollout_metrics.session_rebinds,
    [#("kind", kind), #("outcome", outcome)],
    by,
  )
}

fn fallbacks(kind: String, reason: String, by: Int) -> Nil {
  rollout_metrics.count(
    rollout_metrics.rebind_fallbacks,
    [#("kind", kind), #("reason", reason)],
    by,
  )
}

fn op7(reason: String) -> Nil {
  rollout_metrics.count(rollout_metrics.op7_sent, [#("reason", reason)], 1)
}

fn replay_held(state: State, held: List(Held)) -> Step {
  case held {
    [] -> Go(state)
    [first, ..rest] -> {
      let message = case first {
        HeldInbox(inbox) -> Inbox(inbox)
        HeldControl(found) -> Ctl(found)
      }
      case step(state, message) {
        Go(state) -> replay_held(state, rest)
        Done -> Done
      }
    }
  }
}

fn next(outcome: Outcome) -> Step {
  case outcome {
    Continue(state) -> Go(state)
    Stop(state) -> stop(state)
  }
}

fn stop(state: State) -> Step {
  leases.release(process.self())
  case state.fenced {
    True -> {
      voice.session_handed_off(state)
      process.send_abnormal_exit(process.self(), protocol.handoff_exit())
    }
    False -> {
      voice.session_closed(state)
      presence_link.disconnect(state)
      guilds.leave_all(state)
    }
  }
  Done
}

fn on_inbox(state: State, inbox: SessionInbox) -> Step {
  case inbox {
    SocketOp(op) -> on_op(state, op)
    SocketDown(pid) ->
      case state.socket {
        Attached(socket, monitor) if socket.pid == pid -> {
          process.demonitor_process(monitor)
          Go(hold_for_resume(state))
        }
        _ -> Go(state)
      }
    Dispatch(event, payload) -> Go(dispatch.emit(state, event, payload))
    GuildDispatch(guild, event, payload, facts) ->
      Go(dispatch.incoming(state, FromGuild(guild), event, payload, facts))
    PresenceFrame(user, payload, facts) ->
      Go(dispatch.incoming(
        state,
        FromPresence(user),
        "PRESENCE_UPDATE",
        payload,
        facts,
      ))
    GuildDown(guild, retry_after_ms) ->
      Go(guilds.guild_down(state, guild, retry_after_ms))
    session.GuildRebound(..) | session.PresenceRebound(..) -> Go(state)
    Terminate(_) -> stop(state)
  }
}

fn on_op(state: State, op: ClientOp) -> Step {
  case op {
    Heartbeat(seq) -> {
      let state = case seq {
        Some(seq) -> acknowledge(state, seq)
        None -> state
      }
      st.send_frames(state, [HeartbeatAckFrame])
      Go(state)
    }
    PresenceUpdate(data) -> Go(presence_update(state, data))
    VoiceStateUpdate(data) -> {
      voice.state_update(state, data)
      Go(state)
    }
    VoiceSignal(data) -> {
      voice.signal(state, data)
      Go(state)
    }
    session.LazyRequest(data) -> {
      let state = store_subscriptions(state, data)
      let _ = voice.forward(state, op, data)
      Go(state)
    }
    session.RequestGuildCounts(data) -> {
      voice.guild_counts(state, data)
      Go(state)
    }
    session.RequestChannelMemberCounts(data) ->
      case voice.channel_counts(state, data) {
        True -> Go(state)
        False ->
          Go(dispatch.emit(
            state,
            voice.channel_counts_event,
            voice.empty_counts(data),
          ))
      }
    session.RequestGuildMembers(data) -> {
      voice.request_members(state, op, data)
      Go(state)
    }
    session.RequestForumUnreads(data) -> {
      case state.bot || state.flags.channel_threads {
        True -> {
          let _ = voice.forward(state, op, data)
          Nil
        }
        False -> st.close_socket(state, error.UnknownOpcode, "Unknown opcode")
      }
      Go(state)
    }
  }
}

fn store_subscriptions(state: State, data: Dynamic) -> State {
  let stored =
    subscriptions.store(
      state.subscriptions,
      subscriptions.decode_incoming(data),
      fn(guild) {
        case id.guild_id(guild) {
          Ok(guild_id) -> links.has(state.links, guild_id)
          Error(_) -> False
        }
      },
    )
  case stored.previous_member_list_guild {
    Some(previous) ->
      case id.guild_id(previous) {
        Ok(guild) ->
          case links.linked_ref(state.links, guild) {
            Some(guild_ref) ->
              core_guild.send(guild_ref, core_guild.DropMemberLists(state.id))
            None -> Nil
          }
        Error(_) -> Nil
      }
    None -> Nil
  }
  State(..state, subscriptions: stored.subscriptions)
}

fn acknowledge(state: State, seq: Int) -> State {
  case seq < state.ack_seq {
    True -> state
    False -> {
      let acked = int.min(seq, state.seq)
      guilds.repair(
        State(
          ..state,
          ack_seq: acked,
          replay: replay_buffer.ack(state.replay, acked),
        ),
      )
    }
  }
}

type PresenceChange {
  PresenceChange(
    status: Option(Status),
    afk: Option(Bool),
    mobile: Option(Bool),
    custom_status: Option(Option(RawJson)),
  )
}

fn presence_change_decoder() -> Decoder(PresenceChange) {
  let optional_bool = fn(name) {
    decode.optional_field(
      name,
      None,
      decode.one_of(decode.bool |> decode.map(Some), [decode.success(None)]),
      decode.success,
    )
  }
  use status <- decode.optional_field(
    "status",
    None,
    decode.one_of(
      decode.string
        |> decode.map(fn(text) {
          case presence.status_from_wire(text) {
            Ok(status) -> Some(status)
            Error(Nil) -> None
          }
        }),
      [decode.success(None)],
    ),
  )
  use afk <- decode.then(optional_bool("afk"))
  use mobile <- decode.then(optional_bool("mobile"))
  use custom_status <- decode.optional_field(
    "custom_status",
    None,
    decode.optional(payload.raw())
      |> decode.map(Some),
  )
  decode.success(PresenceChange(status:, afk:, mobile:, custom_status:))
}

fn presence_update(state: State, data: Dynamic) -> State {
  case decode.run(data, presence_change_decoder()) {
    Error(_) -> state
    Ok(change) -> apply_presence(state, change)
  }
}

fn apply_presence(state: State, change: PresenceChange) -> State {
  let previous_hold = st.push_hold(state)
  let status = option.unwrap(change.status, state.status)
  let state =
    State(
      ..state,
      status:,
      afk: option.unwrap(change.afk, state.afk),
      mobile: option.unwrap(change.mobile, state.mobile),
      custom_status: option.unwrap(change.custom_status, state.custom_status),
      resume_status: case status {
        presence.Offline -> state.resume_status
        _ -> status
      },
    )
  presence_link.push_update(state, custom_status_op(change.custom_status))
  case st.push_hold(state) == previous_hold {
    True -> Nil
    False -> guilds.send_push_hold(state)
  }
  state
}

fn custom_status_op(change: Option(Option(RawJson))) -> Option(RawJson) {
  case change {
    None -> None
    Some(None) -> Some(raw_json.null())
    Some(Some(custom)) -> Some(custom)
  }
}

fn attach(state: State, socket: SocketRef) -> State {
  case state.socket {
    Attached(old, monitor) -> {
      process.demonitor_process(monitor)
      case old.pid == socket.pid {
        True -> Nil
        False -> {
          op7("replaced")
          process.send(old.outbox, Send(state.id, [ReconnectFrame]))
        }
      }
    }
    Detached(_) -> session_metrics.attach(process.self())
  }
  State(
    ..state,
    socket: Attached(socket, process.monitor(socket.pid)),
    generation: state.generation + 1,
  )
}

fn resume(
  state: State,
  seq: Int,
  socket: SocketRef,
) -> Result(State, ResumeError) {
  case seq > state.seq || seq < state.ack_seq, seq < state.replay_floor {
    True, _ -> Error(InvalidResumeSeq)
    False, True -> Error(SessionGone)
    False, False -> {
      let previous_hold = st.push_hold(state)
      let status = case state.status {
        presence.Offline ->
          case state.resume_status {
            presence.Offline -> presence.Online
            other -> other
          }
        other -> other
      }
      let state = State(..attach(state, socket), status:, resume_status: status)
      case st.push_hold(state) == previous_hold {
        True -> Nil
        False -> guilds.send_push_hold(state)
      }
      case presence_link.is_linked(state) {
        True -> {
          presence_link.push_update(state, None)
          Ok(state)
        }
        False -> Ok(presence_link.force_reconnect(state))
      }
    }
  }
}

fn replay_after_attach(state: State, seq: Int, timing: Timing) -> State {
  let replay_started_us = timings.now_us()
  let missed =
    list.map(replay_buffer.after(state.replay, seq), fn(entry) {
      DispatchFrame(entry.seq, entry.event, entry.payload)
    })
  let resumed =
    DispatchFrame(
      state.seq,
      "RESUMED",
      resumed_payload(state, timing, replay_started_us),
    )
  session_metrics.replayed(session_metrics.ResumeReplay, list.length(missed))
  let frames = list.append(missed, [resumed])
  case list.length(frames) > max_inline_replay {
    True ->
      list.each(list.sized_chunk(frames, max_inline_replay), fn(chunk) {
        st.send_frames(state, chunk)
      })
    False -> st.send_frames(state, frames)
  }
  state
}

fn resumed_payload(
  state: State,
  timing: Timing,
  replay_started_us: Int,
) -> RawJson {
  case state.staff {
    False -> raw_json.wrap(<<"{}":utf8>>)
    True ->
      raw_json.from_json(
        json.object([
          #(
            "_timings_gw",
            timing
              |> timings.record("replay_missed_events", replay_started_us, None)
              |> timings.finish,
          ),
        ]),
      )
  }
}

fn hold_for_resume(state: State) -> State {
  session_metrics.detach(process.self())
  let generation = state.generation + 1
  st.schedule(state, limits.resume_timeout_ms(), ResumeExpired(generation))
  st.schedule(state, offline_grace_ms(), OfflineGrace(generation))
  State(
    ..state,
    socket: Detached(generation),
    generation:,
    resume_status: case state.status {
      presence.Offline -> state.resume_status
      status -> status
    },
  )
}

fn on_control(state: State, message: Control) -> Step {
  case message {
    control.GuildJoin(guild) -> Go(guilds.guild_join(state, guild))
    control.GuildLeave(guild) -> next(guilds.leave(state, guild))
    control.GuildUnavailable(guild, hidden) ->
      Go(guilds.forced_unavailable(state, guild, hidden))
    control.VisitStart(guild, operator) ->
      Go(guilds.visit_start(state, guild, operator))
    control.VisitEnd(guild) -> next(guilds.visit_end(state, guild))
    control.VisitRejected(guild) -> next(guilds.visit_rejected(state, guild))
    control.VisitPromoted(guild_ref, guild_create) ->
      next(guilds.visit_promoted(state, guild_ref, guild_create))
    control.GuildHealth(guild, degraded) ->
      Go(guilds.health(state, guild, degraded))
    control.DmPartnerMutual(guild, partners) ->
      Go(guilds.dm_mutual(state, guild, partners))
    control.TerminateMatching(hashes, reply) -> {
      let matched =
        state.auth_hash != <<>>
        && list.any(hashes, fn(hash) {
          seed.base64url_decode(hash) == Ok(state.auth_hash)
        })
      process.send(reply, matched)
      case matched {
        True -> stop(state)
        False -> Go(state)
      }
    }
    control.VoiceRejected(error) -> {
      st.send_error(state, error)
      Go(state)
    }
    control.CallMonitor(channel, call) -> Go(calls.watch(state, channel, call))
    control.CallUnmonitor(channel) -> Go(calls.unwatch(state, channel))
    control.PresenceRejoinCheck -> Go(presence_link.relink(state))
    control.PresenceMoved -> Go(presence_link.force_reconnect(state))
    control.PresenceRebind(to, stub) -> {
      let state = presence_link.rebind(state, to, stub)
      presence_signal.repointed(stub, state.id)
      Go(state)
    }
    control.GuildRebind(guild, stub) -> {
      let state = guilds.rebind(state, guild)
      guild_signal.repointed(stub, state.id)
      Go(state)
    }
    control.Reattach(socket, seq) -> Go(reattach(state, socket, seq))
    control.Relayed(inbox) -> on_inbox(state, inbox)
    control.RelayedControl(found) -> on_control(state, found)
    control.MoveDone(..) -> Go(state)
    control.HandOff(target, reply) -> {
      let state = dispatch.flush_reactions(state)
      let speaks_moves = wire.speaks(target, wire.move_forms_epoch)
      let committed =
        transfer.push(target, export(state, speaks_moves), speaks_moves)
        |> result.map(transfer.commit)
      case committed {
        Ok(transfer.Adopted(target)) -> {
          registry.depart(state.me)
          process.send(reply, True)
          departure.run(state, target, speaks_moves)
          stop(State(..state, fenced: True))
        }
        Ok(transfer.Unconfirmed) -> {
          registry.depart(state.me)
          process.send(reply, True)
          op7("move_unconfirmed")
          st.send_frames(state, [ReconnectFrame])
          stop(State(..state, fenced: True))
        }
        Ok(transfer.Refused) | Error(Nil) -> {
          process.send(reply, False)
          Go(state)
        }
      }
    }
    control.Resume(token, seq, socket, reply) -> {
      let timing = timings.start("resume")
      case seed.token_matches(state.token_hash, token) {
        False -> {
          process.send(reply, Error(ResumeAuthFailed))
          Go(state)
        }
        True ->
          case resume(state, seq, socket) {
            Ok(state) -> {
              process.send(reply, Ok(state.me))
              Go(replay_after_attach(
                state,
                seq,
                timings.step(timing, "session_resume"),
              ))
            }
            Error(error) -> {
              process.send(reply, Error(error))
              Go(state)
            }
          }
      }
    }
  }
}

pub fn gap_reason(state: State, seq: Int) -> String {
  case seq > state.seq, seq < state.ack_seq {
    True, _ -> "ahead"
    False, True -> "acked"
    False, False -> "evicted"
  }
}

fn reattach(state: State, socket: SocketRef, seq: Int) -> State {
  case state.socket {
    Attached(held, _) if held.pid == socket.pid -> state
    Attached(..) -> {
      op7("replaced")
      process.send(socket.outbox, Send(state.id, [ReconnectFrame]))
      state
    }
    Detached(_) -> {
      let state = attach(state, socket)
      case seq > state.seq || seq < state.ack_seq || seq < state.replay_floor {
        True -> {
          rollout_metrics.count(
            rollout_metrics.dispatch_gaps,
            [#("reason", gap_reason(state, seq))],
            1,
          )
          op7("replay_gap")
          st.send_frames(state, [ReconnectFrame])
        }
        False -> {
          let missed =
            list.map(replay_buffer.after(state.replay, seq), fn(entry) {
              DispatchFrame(entry.seq, entry.event, entry.payload)
            })
          session_metrics.replayed(
            session_metrics.ReattachReplay,
            list.length(missed),
          )
          missed
          |> list.sized_chunk(max_inline_replay)
          |> list.each(st.send_frames(state, _))
          process.send(socket.outbox, session.Reattached(state.id))
        }
      }
      state
    }
  }
}

fn export(state: State, speaks_moves: Bool) -> Seed {
  let #(buffer, dropped_up_to) = replay_buffer.transfer(state.replay)
  Seed(
    id: state.id,
    user: state.user,
    staff: state.staff,
    token_hash: state.token_hash,
    auth_hash: state.auth_hash,
    version: state.version,
    flags: state.flags,
    shard: state.shard,
    e2ee_capable: state.e2ee_capable,
    country_code: state.country_code,
    ignored_events: set.to_list(state.ignored),
    initial_guild: state.initial_guild,
    status: state.status,
    resume_status: state.resume_status,
    afk: state.afk,
    mobile: state.mobile,
    custom_status: state.custom_status,
    settings_invisible: state.settings_invisible,
    guilds: links.guild_ids(state.links),
    visits: state.visits,
    relations: state.relations,
    ready: state.ready,
    seq: state.seq,
    ack_seq: state.ack_seq,
    replay_floor: int.max(state.replay_floor, dropped_up_to),
    buffer:,
    active: subscriptions.active(state.subscriptions),
    subscriptions: subscriptions.guilds(state.subscriptions),
    health: state.health,
    presence_view: state.presence_view,
    transferred: True,
    moved: case state.ready, speaks_moves {
      None, True ->
        Some(seed.Moved(
          source: state.me,
          linked: links.linked(state.links),
          presence: case state.presence {
            PresenceUp(link, _) -> Some(link)
            _ -> None
          },
          presence_targets: state.presence_targets,
          dm_mutual: state.dm_mutual,
          suppress_presences: state.suppress_presences,
          pending_presences: state.pending_presences,
        ))
      _, _ -> None
    },
  )
}

fn on_internal(state: State, message: Internal) -> Step {
  case message {
    ConnectGuild(guild, attempt) -> Go(guilds.connect(state, guild, attempt))
    JoinExpired(guild, token) -> next(guilds.on_expired(state, guild, token))
    JoinReplied(reply) -> next(guilds.on_reply(state, reply))
    PresenceConnect(attempt) -> Go(presence_link.connect(state, attempt))
    PresenceReplied(result) ->
      case presence_link.on_reply(state, result) {
        NowLinked(state) -> next(readiness.check(state))
        StillDown(state) -> Go(state)
      }
    PresenceTimeout(attempt) -> Go(presence_link.on_timeout(state, attempt))
    ReadyDeadline -> next(readiness.send_ready(state))
    EnablePresenceUpdates -> Go(dispatch.enable_presences(state))
    RepairTick -> {
      st.schedule(state, links.jitter(repair_interval_ms(), 20), RepairTick)
      let state = guilds.repair(state)
      let state = case state.presence {
        PresenceDown -> presence_link.connect(state, 0)
        _ -> presence_link.sync_targets(presence_link.relink(state))
      }
      Go(state)
    }
    ResumeExpired(generation) ->
      case state.socket {
        Detached(current) if current == generation -> stop(state)
        _ -> Go(state)
      }
    OfflineGrace(generation) ->
      case state.socket {
        Detached(current) if current == generation ->
          Go(apply_presence(
            state,
            PresenceChange(
              status: Some(presence.Offline),
              afk: None,
              mobile: None,
              custom_status: None,
            ),
          ))
        _ -> Go(state)
      }
    FlushReactions -> Go(dispatch.flush_reactions(state))
    CallReconnect(channel, attempt) ->
      Go(calls.reconnect(state, channel, attempt))
    ArrivalExpired -> Go(state)
  }
}

fn on_down(state: State, down: Down) -> Step {
  case down {
    ProcessDown(monitor, _, reason) ->
      case state.socket {
        Attached(_, socket_monitor) if socket_monitor == monitor ->
          case is_client_closed(reason) {
            True -> stop(State(..state, socket: Detached(state.generation + 1)))
            False -> Go(hold_for_resume(state))
          }
        _ ->
          case presence_link.on_down(state, monitor) {
            Some(state) -> Go(state)
            None ->
              case guilds.on_down(state, monitor, reason) {
                Some(outcome) -> next(outcome)
                None ->
                  calls.on_down(state, monitor, reason)
                  |> option.unwrap(state)
                  |> Go
              }
          }
      }
    _ -> Go(state)
  }
}

fn is_client_closed(reason: process.ExitReason) -> Bool {
  case reason {
    process.Abnormal(term) ->
      decode.run(term, client_closed_decoder()) == Ok(True)
    _ -> False
  }
}

fn client_closed_decoder() -> Decoder(Bool) {
  use tag <- decode.field(0, atom.decoder())
  use detail <- decode.field(1, atom.decoder())
  decode.success(
    atom.to_string(tag) == "shutdown"
    && atom.to_string(detail) == "client_closed",
  )
}
