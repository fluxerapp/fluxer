// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{Idle, Online}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/presence/custom_status
import fluxer_gateway/presence/dispatch_id
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/profile
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/push_buffer
import fluxer_gateway/presence/state.{type Effect, type State}
import fluxer_gateway/presence/version
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

const me = 1

fn session_ref(suffix: String) -> SessionRef {
  let assert Ok(user) = id.user_id(me)
  let assert Ok(session_id) =
    session.session_id_from_string("0000000000000000000000000000000" <> suffix)
  SessionRef(id: session_id, user:, inbox: process.new_subject())
}

fn start(targets: List(Int)) -> #(State(Int), List(Effect(Int))) {
  state.init(
    protocol.StartRequest(
      user: me,
      profile: profile.new(
        fields: [#(profile.Id, <<"1">>)],
        restricted: False,
        bot: False,
      ),
      custom_status: None,
      settings_invisible: False,
      guilds: id_set.empty(),
      targets: id_set.from_list(targets),
      push_limits: push_buffer.default_limits(),
      carried: None,
    ),
    version.new_epoch(),
    [],
    0,
  )
}

fn connect(
  machine: State(Int),
  ref: SessionRef,
  monitor: Int,
) -> #(State(Int), List(Effect(Int))) {
  state.handle(
    machine,
    protocol.Connect(
      protocol.ConnectRequest(
        session: ref,
        status: Online,
        afk: False,
        mobile: False,
        guilds: id_set.empty(),
        targets: id_set.empty(),
        monitor:,
      ),
      process.new_subject(),
    ),
    1000,
  )
}

fn update(
  machine: State(Int),
  ref: SessionRef,
  status: presence.Status,
) -> #(State(Int), List(Effect(Int))) {
  state.handle(
    machine,
    protocol.Update(
      session: ref.id,
      status:,
      afk: False,
      mobile: False,
      custom_status: custom_status.OpAbsent,
    ),
    2000,
  )
}

fn fanouts(effects: List(Effect(Int))) -> List(event.PresenceEvent) {
  list.filter_map(effects, fn(effect) {
    case effect {
      state.Fanout(outgoing) -> Ok(outgoing)
      _ -> Error(Nil)
    }
  })
}

fn has_stop(effects: List(Effect(Int))) -> Bool {
  list.contains(effects, state.Stop)
}

pub fn init_subscribes_targets_without_self_test() {
  let #(machine, effects) = start([me, 2, 3])
  id_set.to_list(state.targets(machine)) |> should.equal([2, 3])
  effects
  |> should.equal([
    state.Subscribe(id_set.from_list([2, 3])),
    state.ArmIdleCheck(30_000),
  ])
}

pub fn connect_publishes_once_and_reads_snapshot_test() {
  let #(machine, _) = start([2])
  let ref = session_ref("a")
  let #(machine, effects) = connect(machine, ref, 10)
  list.length(fanouts(effects)) |> should.equal(1)
  list.any(effects, fn(effect) {
    case effect {
      state.ReadSnapshot(..) -> True
      _ -> False
    }
  })
  |> should.be_true
  let #(_, again) = update(machine, ref, Online)
  fanouts(again) |> should.equal([])
}

pub fn status_change_bumps_version_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let before = state.current_version(machine)
  let #(machine, effects) = update(machine, ref, Idle)
  case fanouts(effects) {
    [outgoing] -> {
      display.status(outgoing.display) |> should.equal(Idle)
      version.compare(outgoing.version, before) |> should.equal(version.Newer)
    }
    _ -> should.fail()
  }
  version.seq(state.current_version(machine))
  |> should.equal(version.seq(before) + 1)
}

pub fn last_session_down_publishes_offline_and_stops_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let #(_, effects) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Closed),
      3000,
    )
  case fanouts(effects) {
    [outgoing] -> display.is_visible(outgoing.display) |> should.be_false
    _ -> should.fail()
  }
  has_stop(effects) |> should.be_true
}

pub fn noconnection_grace_keeps_presence_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let #(machine, effects) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Unreachable),
      3000,
    )
  fanouts(effects) |> should.equal([])
  has_stop(effects) |> should.be_false
  let token = case effects {
    [state.ArmGrace(token, _)] -> token
    _ -> -1
  }
  let #(reconnected, effects) = connect(machine, session_ref("b"), 11)
  fanouts(effects) |> should.equal([])
  let #(_, late) =
    state.handle(reconnected, protocol.GraceExpired(token), 20_000)
  late |> should.equal([])
  let #(_, expired) =
    state.handle(machine, protocol.GraceExpired(token), 20_000)
  has_stop(expired) |> should.be_true
}

pub fn handoff_revives_lost_session_with_fresh_monitor_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let #(machine, armed) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Unreachable),
      3000,
    )
  let token = case armed {
    [state.ArmGrace(token, _)] -> token
    _ -> -1
  }
  let carried =
    protocol.Carried(
      version: state.current_version(machine),
      last: None,
      current: None,
      temporary_guilds: id_set.empty(),
      push: push_buffer.new(),
      sessions: [
        protocol.CarriedSession(
          session: ref,
          status: Online,
          afk: False,
          mobile: False,
        ),
      ],
      validating: None,
      delivered: dispatch_id.empty(),
      doomed: [],
    )
  let request =
    protocol.StartRequest(
      user: me,
      profile: profile.new(
        fields: [#(profile.Id, <<"1">>)],
        restricted: False,
        bot: False,
      ),
      custom_status: None,
      settings_invisible: False,
      guilds: id_set.empty(),
      targets: id_set.empty(),
      push_limits: push_buffer.default_limits(),
      carried: Some(carried),
    )
  let reply = process.new_subject()
  let #(machine, adopted) =
    state.handle(machine, protocol.Adopt(request, [20], reply), 4000)
  list.contains(adopted, state.ReplyAdopt(reply, protocol.Adopted))
  |> should.be_true
  list.contains(adopted, state.Demonitor(20)) |> should.be_false
  list.contains(adopted, state.Demonitor(10)) |> should.be_true
  let #(machine, late) =
    state.handle(machine, protocol.GraceExpired(token), 20_000)
  late |> should.equal([])
  let #(_, down) =
    state.handle(
      machine,
      protocol.SessionDown(20, ending: protocol.Closed),
      21_000,
    )
  has_stop(down) |> should.be_true
}

pub fn adopt_without_carried_state_still_confirms_test() {
  let #(machine, _) = start([])
  let reply = process.new_subject()
  let request =
    protocol.StartRequest(
      user: me,
      profile: profile.new(
        fields: [#(profile.Id, <<"1">>)],
        restricted: False,
        bot: False,
      ),
      custom_status: None,
      settings_invisible: False,
      guilds: id_set.empty(),
      targets: id_set.empty(),
      push_limits: push_buffer.default_limits(),
      carried: None,
    )
  let #(_, effects) =
    state.handle(machine, protocol.Adopt(request, [30], reply), 0)
  effects
  |> should.equal([
    state.Demonitor(30),
    state.ReplyAdopt(reply, protocol.Adopted),
  ])
}

pub fn adopt_arriving_while_moving_is_redirected_not_dropped_test() {
  let #(machine, _) = start([])
  let target = atom.create("presence_target@host")
  let #(machine, started) = state.handle(machine, protocol.MoveTo(target), 0)
  let token = case started {
    [state.StartMove(_, token, _)] -> token
    _ -> -1
  }
  let request =
    protocol.StartRequest(
      user: me,
      profile: profile.new(
        fields: [#(profile.Id, <<"1">>)],
        restricted: False,
        bot: False,
      ),
      custom_status: None,
      settings_invisible: False,
      guilds: id_set.empty(),
      targets: id_set.empty(),
      push_limits: push_buffer.default_limits(),
      carried: None,
    )
  let reply = process.new_subject()
  let #(machine, deferred) =
    state.handle(machine, protocol.AdoptHandoff(request, reply), 10)
  deferred |> should.equal([])
  let next = process.new_subject()
  let #(_, finished) =
    state.handle(machine, protocol.MoveFinished(token, Ok(next)), 20)
  list.contains(finished, state.ReplyAdopt(reply, protocol.AdoptMoved(next)))
  |> should.be_true
  has_stop(finished) |> should.be_true
}

pub fn foreign_events_dedupe_by_version_test() {
  let #(machine, _) = start([2])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let shown = display.new(status: Online, mobile: False, afk: False)
  let second = version.next(version.first(version.new_epoch()))
  let first =
    event.status_changed(
      user: 2,
      version: version.next(second),
      display: shown,
      custom_status: <<"null">>,
      user_wire: <<"{\"id\":2}">>,
    )
  let older =
    event.status_changed(
      user: 2,
      version: second,
      display: shown,
      custom_status: <<"null">>,
      user_wire: <<"{\"id\":2}">>,
    )
  let #(machine, effects) = state.handle(machine, protocol.Foreign(first), 0)
  list.length(effects) |> should.equal(1)
  let #(_, stale) = state.handle(machine, protocol.Foreign(older), 0)
  stale |> should.equal([])
}

pub fn foreign_event_for_unknown_target_is_ignored_test() {
  let #(machine, _) = start([2])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let shown = display.new(status: Online, mobile: False, afk: False)
  let stranger =
    event.status_changed(
      user: 9,
      version: version.first(version.new_epoch()),
      display: shown,
      custom_status: <<"null">>,
      user_wire: <<"{\"id\":9}">>,
    )
  let #(_, effects) = state.handle(machine, protocol.Foreign(stranger), 0)
  effects |> should.equal([])
}

pub fn unsubscribe_sends_forced_offline_test() {
  let #(machine, _) = start([2, 3])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let #(machine, effects) =
    state.handle(
      machine,
      protocol.UnsubscribeTargets(id_set.from_list([3, 4])),
      0,
    )
  id_set.to_list(state.targets(machine)) |> should.equal([2])
  case effects {
    [state.Unsubscribe(removed), state.SendSessions(_, _)] ->
      id_set.to_list(removed) |> should.equal([3])
    _ -> should.fail()
  }
}

pub fn custom_status_validation_is_generation_checked_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let assert Ok(value) = custom_status_value("{\"text\":\"hi\"}")
  let #(machine, effects) =
    state.handle(
      machine,
      protocol.Update(
        session: ref.id,
        status: Online,
        afk: False,
        mobile: False,
        custom_status: custom_status.OpMap(value),
      ),
      0,
    )
  let generation = case effects {
    [state.StartValidation(_, generation), ..] -> generation
    _ -> -1
  }
  let #(_, stale) =
    state.handle(
      machine,
      protocol.Validated(generation + 1, custom_status.Rejected),
      0,
    )
  stale |> should.equal([])
  let #(applied, effects) =
    state.handle(
      machine,
      protocol.Validated(generation, custom_status.Validated(Some(value))),
      0,
    )
  list.length(fanouts(effects)) |> should.equal(1)
  state.stored_custom_status(applied) |> should.equal(Some(value))
}

fn custom_status_value(
  text: String,
) -> Result(custom_status.CustomStatus, Nil) {
  case custom_status.op_value(Some(raw_json.wrap(<<text:utf8>>))) {
    custom_status.OpMap(value) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn user_dispatch(id: dispatch_id.DispatchId) -> protocol.Input(Int) {
  protocol.UserDispatch(
    id:,
    event: "MESSAGE_CREATE",
    data: raw_json.wrap(<<"{}">>),
    parsed: protocol.OtherEvent,
  )
}

fn sends(effects: List(Effect(Int))) -> Int {
  list.count(effects, fn(effect) {
    case effect {
      state.SendSessions(..) -> True
      _ -> False
    }
  })
}

fn begin_move(
  machine: State(Int),
) -> #(State(Int), Int, protocol.StartRequest) {
  let target = atom.create("presence_target@host")
  let assert #(machine, [state.StartMove(_, token, request)]) =
    state.handle(machine, protocol.MoveTo(target), 0)
  #(machine, token, request)
}

pub fn repeated_user_dispatch_reaches_sessions_once_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let id = dispatch_id.mint()
  let #(machine, first) = state.handle(machine, user_dispatch(id), 0)
  sends(first) |> should.equal(1)
  let #(_, again) = state.handle(machine, user_dispatch(id), 0)
  again |> should.equal([])
}

pub fn user_dispatch_while_moving_is_forwarded_not_sent_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let #(machine, token, _) = begin_move(machine)
  let id = dispatch_id.mint()
  let #(machine, held) = state.handle(machine, user_dispatch(id), 10)
  held |> should.equal([])
  let next = process.new_subject()
  let #(_, finished) =
    state.handle(machine, protocol.MoveFinished(token, Ok(next)), 20)
  sends(finished) |> should.equal(0)
  list.contains(
    finished,
    state.Forward(next, [
      protocol.UserDispatch(
        id:,
        event: "MESSAGE_CREATE",
        data: raw_json.wrap(<<"{}">>),
        parsed: protocol.OtherEvent,
      ),
    ]),
  )
  |> should.be_true
}

pub fn user_dispatch_while_moving_is_replayed_on_failure_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let #(machine, token, _) = begin_move(machine)
  let id = dispatch_id.mint()
  let #(machine, _) = state.handle(machine, user_dispatch(id), 10)
  let #(_, failed) =
    state.handle(machine, protocol.MoveFinished(token, Error(Nil)), 20)
  list.contains(failed, state.Replay([user_dispatch(id)])) |> should.be_true
}

pub fn handoff_target_drops_dispatch_it_already_delivered_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let earlier = dispatch_id.mint()
  let #(machine, _) = state.handle(machine, user_dispatch(earlier), 5)
  let #(_, _, request) = begin_move(machine)
  let #(target, _) = state.init(request, version.new_epoch(), [20], 30)
  let #(target, carried_again) =
    state.handle(target, user_dispatch(earlier), 30)
  carried_again |> should.equal([])
  let during = dispatch_id.mint()
  let #(target, direct) = state.handle(target, user_dispatch(during), 40)
  sends(direct) |> should.equal(1)
  let #(_, forwarded) = state.handle(target, user_dispatch(during), 50)
  forwarded |> should.equal([])
}

fn carrying(refs: List(SessionRef)) -> protocol.StartRequest {
  protocol.StartRequest(
    user: me,
    profile: profile.new(
      fields: [#(profile.Id, <<"1">>)],
      restricted: False,
      bot: False,
    ),
    custom_status: None,
    settings_invisible: False,
    guilds: id_set.empty(),
    targets: id_set.empty(),
    push_limits: push_buffer.default_limits(),
    carried: Some(
      protocol.Carried(
        version: version.first(version.new_epoch()),
        last: None,
        current: None,
        temporary_guilds: id_set.empty(),
        push: push_buffer.new(),
        sessions: list.map(refs, fn(ref) {
          protocol.CarriedSession(
            session: ref,
            status: Online,
            afk: False,
            mobile: False,
          )
        }),
        validating: None,
        delivered: dispatch_id.empty(),
        doomed: [],
      ),
    ),
  )
}

fn kicks(effects: List(Effect(Int))) -> Bool {
  list.any(effects, fn(effect) {
    case effect {
      state.KickTemporary(..) -> True
      _ -> False
    }
  })
}

pub fn handed_off_session_reconnecting_keeps_presence_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let #(machine, down) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Unreachable),
      3000,
    )
  has_stop(down) |> should.be_false
  kicks(down) |> should.be_false
  fanouts(down) |> should.equal([])
  let #(machine, reconnected) = connect(machine, session_ref("a"), 11)
  has_stop(reconnected) |> should.be_false
  kicks(reconnected) |> should.be_false
  fanouts(reconnected) |> should.equal([])
  let #(_, stale) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Closed),
      4000,
    )
  stale |> should.equal([])
}

pub fn adopt_keeps_live_session_over_stale_carried_copy_test() {
  let #(machine, _) = start([])
  let live = session_ref("a")
  let #(machine, _) = connect(machine, live, 10)
  let stale = SessionRef(..live, inbox: process.new_subject())
  let reply = process.new_subject()
  let #(machine, adopted) =
    state.handle(machine, protocol.Adopt(carrying([stale]), [20], reply), 4000)
  list.contains(adopted, state.ReplyAdopt(reply, protocol.Adopted))
  |> should.be_true
  list.contains(adopted, state.Demonitor(20)) |> should.be_true
  list.contains(adopted, state.Demonitor(10)) |> should.be_false
  let #(machine, dispatched) =
    state.handle(machine, user_dispatch(dispatch_id.mint()), 5000)
  list.any(dispatched, fn(effect) {
    case effect {
      state.SendSessions([target], _) -> target == live
      _ -> False
    }
  })
  |> should.be_true
  let #(machine, carried_down) =
    state.handle(
      machine,
      protocol.SessionDown(20, ending: protocol.Closed),
      6000,
    )
  carried_down |> should.equal([])
  let #(_, live_down) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Closed),
      7000,
    )
  has_stop(live_down) |> should.be_true
}

pub fn session_closed_during_move_is_disconnected_at_new_owner_test() {
  let #(machine, _) = start([])
  let ref = session_ref("a")
  let #(machine, _) = connect(machine, ref, 10)
  let #(machine, token, _) = begin_move(machine)
  let #(machine, held) =
    state.handle(machine, protocol.SessionDown(10, ending: protocol.Closed), 10)
  held |> should.equal([])
  let next = process.new_subject()
  let #(_, finished) =
    state.handle(machine, protocol.MoveFinished(token, Ok(next)), 20)
  list.contains(
    finished,
    state.Forward(next, [protocol.Disconnect(ref.id, ref.inbox)]),
  )
  |> should.be_true
}

pub fn session_lost_during_move_is_left_to_new_owner_grace_test() {
  let #(machine, _) = start([])
  let #(machine, _) = connect(machine, session_ref("a"), 10)
  let #(machine, token, _) = begin_move(machine)
  let #(machine, _) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Unreachable),
      10,
    )
  let next = process.new_subject()
  let #(_, finished) =
    state.handle(machine, protocol.MoveFinished(token, Ok(next)), 20)
  list.any(finished, fn(effect) {
    case effect {
      state.Forward(..) -> True
      _ -> False
    }
  })
  |> should.be_false
}

pub fn lost_session_beside_live_one_keeps_display_until_grace_test() {
  let #(machine, _) = start([])
  let desktop = session_ref("a")
  let phone = session_ref("b")
  let #(machine, _) = connect(machine, desktop, 10)
  let #(machine, _) = connect(machine, phone, 11)
  let #(machine, _) = update(machine, phone, Idle)
  let #(machine, lost) =
    state.handle(
      machine,
      protocol.SessionDown(10, ending: protocol.Unreachable),
      3000,
    )
  fanouts(lost) |> should.equal([])
  let token = case lost {
    [state.ArmGrace(token, _), ..] -> token
    _ -> -1
  }
  let #(_, expired) =
    state.handle(machine, protocol.GraceExpired(token), 20_000)
  case fanouts(expired) {
    [outgoing] -> display.status(outgoing.display) |> should.equal(Idle)
    _ -> should.fail()
  }
  has_stop(expired) |> should.be_false
}
