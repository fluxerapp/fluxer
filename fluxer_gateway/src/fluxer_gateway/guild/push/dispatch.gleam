// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild.{type GuildFacts, type GuildRef}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/guild/members.{type MembersTable}
import fluxer_gateway/guild/push/admission
import fluxer_gateway/guild/push/engagement
import fluxer_gateway/guild/push/grace
import fluxer_gateway/guild/push/guild_info
import fluxer_gateway/guild/push/masks
import fluxer_gateway/guild/push/plan.{type Capture, type Routed, Capture}
import fluxer_gateway/guild/push/profiles
import fluxer_gateway/guild/push/state
import fluxer_gateway/guild/push/thread_scope.{type ThreadScope}
import fluxer_gateway/member_list.{type MemberList}
import fluxer_gateway/perms
import fluxer_gateway/push/fetch
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/metrics
import fluxer_gateway/push/permit
import fluxer_gateway/push/rule
import fluxer_gateway/push/server
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import logging

fn settings_deadline_ms() -> Int {
  durations.guild().dispatch_settings_deadline_ms
}

fn profile_sweep_ms() -> Int {
  durations.guild().dispatch_profile_sweep_ms
}

pub type GuildPush =
  state.GuildPush

pub type PushGuildMsg =
  state.GuildMessage

pub type PushInputs {
  PushInputs(
    facts: GuildFacts,
    reader: perms.Reader,
    model: perms.Model,
    list: MemberList,
    members: MembersTable,
    roles: List(Role),
    sessions: List(SessionRef),
    self_ref: GuildRef,
  )
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Atom) -> Atom

pub fn new(guild: GuildId) -> GuildPush {
  state.new(guild)
}

pub fn subject(push: GuildPush) -> Subject(PushGuildMsg) {
  push.inbox
}

pub fn set_info(
  push: GuildPush,
  name name: Option(String),
  default_message_notifications default_message_notifications: Int,
  disabled_operations disabled_operations: Int,
  channels channels: List(Channel),
) -> GuildPush {
  state.GuildPush(
    ..push,
    info: guild_info.from_channels(
      name,
      default_message_notifications,
      disabled_operations,
      channels,
    ),
  )
}

pub fn busy(push: GuildPush, admitted_by admitted_by_ms: Int) -> Bool {
  admission.in_flight(
    push.admission,
    admitted_by: admitted_by_ms,
    now_ms: clock.monotonic_ms(),
    is_live: process.is_alive,
  )
}

pub fn release(push: GuildPush) -> Nil {
  profiles.release(push.profiles)
}

pub fn apply_info(push: GuildPush, event: String, data: Dynamic) -> GuildPush {
  state.GuildPush(..push, info: guild_info.apply_event(push.info, event, data))
}

pub fn on_event(
  push: GuildPush,
  inputs: PushInputs,
  event: String,
  data: Dynamic,
  thread: Option(ThreadScope),
) -> GuildPush {
  case event {
    "MESSAGE_CREATE" -> on_message(push, inputs, data, thread)
    _ -> push
  }
}

fn on_message(
  push: GuildPush,
  inputs: PushInputs,
  data: Dynamic,
  thread: Option(ThreadScope),
) -> GuildPush {
  case message.decode_data(data) {
    Error(Nil) -> push
    Ok(decoded) ->
      case
        message.suppresses_notifications(decoded),
        decoded.author.id,
        decoded.channel_id,
        decoded.id
      {
        False, Some(author), Some(channel), Some(message_id) -> {
          let #(engagement, snapshot) =
            engagement.snapshot(push.engagement, inputs.sessions)
          let push = state.GuildPush(..push, engagement:)
          let capture =
            Capture(
              guild: push.guild,
              data: decoded,
              channel:,
              message: message_id,
              author:,
              thread:,
              snapshot:,
              reader: inputs.reader,
              model: inputs.model,
              members: inputs.members,
              list: inputs.list,
              roles: inputs.roles,
              facts: inputs.facts,
              info: push.info,
              profiles: push.profiles,
              inbox: push.inbox,
            )
          reregister_profiles(push, inputs.list)
          let push =
            push
            |> sweep_profiles(inputs.list)
            |> refresh_masks(capture)
          case push.info.default_level > 3 {
            True -> push
            False -> plan_message(push, capture)
          }
        }
        _, _, _, _ -> push
      }
  }
}

fn reregister_profiles(push: GuildPush, member_list: MemberList) -> Nil {
  let _ =
    profiles.ensure_registered(push.profiles)
    |> list.map(masks.class_update(_, masks.ClassUncertain))
    |> masks.update(member_list, _)
  Nil
}

fn sweep_profiles(push: GuildPush, member_list: MemberList) -> GuildPush {
  let now = clock.monotonic_ms()
  case now < push.next_profile_sweep_ms {
    True -> push
    False -> {
      let _ =
        profiles.expire(push.profiles)
        |> list.map(masks.class_update(_, masks.ClassUncertain))
        |> masks.update(member_list, _)
      state.GuildPush(..push, next_profile_sweep_ms: now + profile_sweep_ms())
    }
  }
}

fn mask_key(capture: Capture) -> masks.Key {
  masks.key(
    capture.list,
    default_level: capture.info.default_level,
    large: plan.is_large(capture),
  )
}

fn refresh_masks(push: GuildPush, capture: Capture) -> GuildPush {
  let mask_key =
    masks.refresh(capture.list, push.mask_key, mask_key(capture), fn() {
      let canonical = plan.canonical(capture)
      profiles.all(push.profiles)
      |> list.map(fn(row) {
        masks.class_update(row.0, masks.class_of(row.1, canonical))
      })
    })
  state.GuildPush(..push, mask_key:)
}

fn plan_message(push: GuildPush, capture: Capture) -> GuildPush {
  let planned = plan.plan(capture)
  let forum = case plan.is_large(capture) {
    True -> None
    False -> plan.forum_starter(capture)
  }
  case planned, forum {
    plan.Nothing, None -> push
    plan.Explicit(users), None ->
      case list.length(users) <= plan.narrow_limit {
        True -> {
          narrow(capture, users)
          push
        }
        False -> wide(push, capture, planned)
      }
    _, _ -> wide(push, capture, planned)
  }
}

fn narrow(capture: Capture, users: List(Int)) -> Nil {
  case guild_info.push_disabled(capture.info) {
    True -> Nil
    False ->
      case plan.route(capture, users, set.new()) {
        Ok(routed) -> deliver(capture, routed, False, False)
        Error(reason) -> unavailable(capture, reason)
      }
  }
}

fn wide(push: GuildPush, capture: Capture, planned: plan.Plan) -> GuildPush {
  let now = clock.monotonic_ms()
  let #(verdict, pruned, counts) =
    admission.admit(
      push.admission,
      now_ms: now,
      limits: admission.default_limits(),
      is_live: process.is_alive,
    )
  report_counts(counts)
  case verdict {
    admission.Drop -> {
      metrics.guild("dropped_at_limit")
      state.GuildPush(
        ..push,
        admission: admission.note_dropped(push.admission, pruned),
      )
    }
    admission.Run -> start_worker(push, capture, planned, pruned, None, now)
    admission.Wait(predecessor) ->
      start_worker(push, capture, planned, pruned, Some(predecessor), now)
  }
}

fn report_counts(counts: admission.Counts) -> Nil {
  repeat(counts.queued, "queued")
  repeat(counts.slot_reclaimed, "slot_reclaimed")
  repeat(counts.slot_deduped, "slot_deduped")
}

fn repeat(times: Int, key: String) -> Nil {
  case times > 0 {
    True -> {
      metrics.guild(key)
      repeat(times - 1, key)
    }
    False -> Nil
  }
}

fn start_worker(
  push: GuildPush,
  capture: Capture,
  planned: plan.Plan,
  pruned: List(admission.Tracked(Pid)),
  gate: Option(Pid),
  now: Int,
) -> GuildPush {
  let worker = process.spawn_unlinked(fn() { run_wide(capture, planned, gate) })
  metrics.guild("worker_started")
  state.GuildPush(
    ..push,
    admission: admission.note_started(pruned, worker, now),
  )
}

fn run_wide(capture: Capture, planned: plan.Plan, gate: Option(Pid)) -> Nil {
  let _ = process_flag(atom.create("priority"), atom.create("low"))
  wait_for_slot(gate)
  case permit.acquire(permit.GuildWide, admission.max_age_ms()) {
    Error(_) -> metrics.guild("permit_dropped")
    Ok(held) -> {
      case guild_info.push_disabled(capture.info) {
        True -> Nil
        False -> scan(capture, planned)
      }
      permit.release(held)
      metrics.guild("worker_completed")
    }
  }
}

fn wait_for_slot(gate: Option(Pid)) -> Nil {
  case gate {
    None -> Nil
    Some(predecessor) -> {
      let monitor = process.monitor(predecessor)
      let selector =
        process.new_selector()
        |> process.select_specific_monitor(monitor, fn(_) { Nil })
      case process.selector_receive(selector, admission.max_age_ms()) {
        Ok(Nil) -> Nil
        Error(Nil) -> {
          process.demonitor_process(monitor)
          Nil
        }
      }
    }
  }
}

fn scan(capture: Capture, planned: plan.Plan) -> Nil {
  case scan_routes(capture, planned) {
    Ok(routes) ->
      list.each(routes, fn(route) { deliver(capture, route.0, route.1, True) })
    Error(reason) -> unavailable(capture, reason)
  }
}

fn scan_routes(
  capture: Capture,
  planned: plan.Plan,
) -> Result(List(#(Routed, Bool)), plan.Unavailable) {
  let direct = message.direct_mention_ids(capture.data)
  use #(candidates, adds) <- result.try(case capture.thread, planned {
    Some(scope), _ -> {
      use adds <- result.map(plan.role_adds(capture, scope, direct))
      let base =
        dict.keys(scope.members)
        |> list.append(set.to_list(direct))
        |> list.append(plan.format_members(capture.data))
        |> list.append(set.to_list(adds))
        |> list.unique
      #(base, adds)
    }
    None, plan.Explicit(users) -> Ok(#(users, set.new()))
    None, plan.AllMembers ->
      plan.all_member_ids(capture)
      |> result.map(fn(users) { #(users, set.new()) })
    None, plan.Nothing -> Ok(#([], set.new()))
  })
  use main <- result.try(case candidates {
    [] -> Ok([])
    _ ->
      plan.route(capture, candidates, adds)
      |> result.map(fn(routed) { [#(routed, False)] })
  })
  use forum <- result.map(
    case plan.is_large(capture), plan.forum_starter(capture) {
      False, Some(#(forum, scope)) ->
        plan.forum_recipients(capture, forum, scope)
        |> result.map(fn(routed) { [#(routed, True)] })
      _, _ -> Ok([])
    },
  )
  list.append(main, forum)
}

fn unavailable(capture: Capture, reason: plan.Unavailable) -> Nil {
  let #(key, source) = case reason {
    plan.PermsUnavailable -> #("scan_perms_unavailable", "permission index")
    plan.MembersUnavailable -> #("scan_table_unavailable", "members table")
  }
  metrics.guild(key)
  logging.log(
    logging.Warning,
    "guild push skipped because the "
      <> source
      <> " was released before routing finished: guild_id="
      <> int.to_string(plan.guild_int(capture))
      <> " message_id="
      <> int.to_string(capture.message),
  )
}

fn deliver(
  capture: Capture,
  routed: Routed,
  forum: Bool,
  evaluate: Bool,
) -> Nil {
  case routed.eligible, routed.held {
    [], [] -> Nil
    _, _ -> {
      let create = plan.build(capture, routed, forum)
      case routed.eligible, evaluate {
        [], _ -> Nil
        _, False -> server.handle_message_create(create)
        _, True -> evaluate_and_send(capture, create)
      }
      grace.hold(create, routed.held, capture.inbox)
    }
  }
}

fn evaluate_and_send(capture: Capture, create: MessageCreate) -> Nil {
  let guild = plan.guild_int(capture)
  let missing =
    list.filter(create.recipients, fn(user) {
      profiles.lookup(capture.profiles, user) == None
    })
  let fetch_started_ms = clock.monotonic_ms()
  let fetched =
    fetch.resolve(
      guild:,
      users: missing,
      author: create.author,
      deadline: clock.deadline_in(settings_deadline_ms()),
    )
  let canonical = plan.canonical(capture)
  let classes =
    dict.fold(fetched, [], fn(acc, user, row) {
      case row {
        fetch.Resolved(found) ->
          case
            profiles.put_fetched(
              capture.profiles,
              user,
              found,
              fetch_started_ms,
            )
          {
            True -> [
              state.FetchedClass(
                user:,
                settings: found,
                class: masks.class_of(found, canonical),
              ),
              ..acc
            ]
            False -> acc
          }
        fetch.Unresolved(_) -> acc
      }
    })
  case classes {
    [] -> Nil
    _ ->
      process.send(
        capture.inbox,
        state.ProfilesFetched(key: mask_key(capture), classes:),
      )
  }
  let shape =
    rule.shape(
      data: create.data,
      guild:,
      channel: create.channel,
      author: create.author,
      guild_default_level: create.guild_default_level,
      large: create.large,
    )
  let now_ms = clock.system_ms()
  let #(direct, bulk) =
    list.fold(list.reverse(create.recipients), #([], []), fn(acc, user) {
      let row =
        profiles.lookup(capture.profiles, user)
        |> option.lazy_or(fn() {
          case dict.get(fetched, user) {
            Ok(fetch.Resolved(found)) -> Some(found)
            _ -> None
          }
        })
      case row {
        None -> acc
        Some(found) -> {
          let facts =
            rule.Facts(
              user:,
              settings: found,
              connected: set.contains(create.connected, user),
              roles: dict.get(create.user_roles, user) |> result.unwrap([]),
            )
          case rule.evaluate(shape, facts, now_ms:) {
            rule.Eligible(reason) ->
              case rule.class(reason) {
                rule.Direct -> #([user, ..acc.0], acc.1)
                rule.Bulk -> #(acc.0, [user, ..acc.1])
              }
            rule.Ineligible(_) -> acc
          }
        }
      }
    })
  send_class(create, direct, message.FilteredDirect)
  send_class(create, bulk, message.FilteredBulk)
}

fn send_class(
  create: MessageCreate,
  users: List(Int),
  prefilter: message.Prefilter,
) -> Nil {
  case users {
    [] -> Nil
    _ ->
      server.handle_message_create(
        message.MessageCreate(..create, recipients: users, prefilter:),
      )
  }
}

pub fn on_permit(
  push: GuildPush,
  member_list: MemberList,
  present: fn(SessionId) -> Bool,
  message: PushGuildMsg,
) -> GuildPush {
  case message {
    state.ProfileSynced(user) -> {
      let _ =
        masks.update(member_list, [
          masks.class_update(user, masks.ClassUncertain),
        ])
      push
    }
    state.ProfilesFetched(key:, classes:) -> {
      let current = push.mask_key == Some(key)
      let _ =
        list.filter_map(classes, fn(fetched) {
          case profiles.lookup(push.profiles, fetched.user) {
            Some(settings) if settings == fetched.settings && current ->
              Ok(masks.class_update(fetched.user, fetched.class))
            Some(settings) if settings == fetched.settings ->
              Ok(masks.class_update(fetched.user, masks.ClassUncertain))
            _ -> Error(Nil)
          }
        })
        |> masks.update(member_list, _)
      push
    }
    state.HeldSessions(sessions:, reply:) -> {
      let ids =
        list.filter_map(sessions, fn(text) {
          session.session_id_from_string(text)
        })
      let released =
        engagement.released_holds(push.engagement, ids)
        |> list.map(session.session_id_to_string)
      let present =
        list.filter(ids, present)
        |> list.map(session.session_id_to_string)
      process.send(reply, state.Holds(present:, released:))
      push
    }
  }
}
