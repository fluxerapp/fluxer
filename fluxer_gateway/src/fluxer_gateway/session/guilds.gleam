// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{
  type GuildRef, type JoinReply, AsMember, AsVisitor, Background, Focused,
  JoinNotEligible, JoinOverloaded, JoinRequest, JoinUnavailable, Joined, Leave,
  NotMember, Resume, SessionOp, SessionPushHold, UpdateDmPartners, VisitRejected,
} as core_guild
import fluxer_gateway/core/guild_create.{type GuildCreate}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{LazyRequest}
import fluxer_gateway/guild/client as guild_client
import fluxer_gateway/session/dispatch
import fluxer_gateway/session/join_budget.{OverQuota, Send}
import fluxer_gateway/session/links.{
  type Link, Detached, Linked, NoHint, Slot, Unavailable as LinkUnavailable,
  Waiting,
}
import fluxer_gateway/session/payload
import fluxer_gateway/session/presence_link
import fluxer_gateway/session/readiness.{type Outcome, Continue}
import fluxer_gateway/session/ready
import fluxer_gateway/session/seed
import fluxer_gateway/session/state.{
  type State, ConnectGuild, JoinExpired, State,
} as st
import fluxer_gateway/session/subscriptions
import fluxer_gateway/session/visits.{ConnectVisit, Ignore, MarkVisit}
import gleam/bool
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import logging

fn busy_retry_ms() -> Int {
  durations.session().guilds_busy_retry_ms
}

fn after_ready_join_ms() -> Int {
  durations.session().guilds_after_ready_join_ms
}

fn first_join_floor_ms() -> Int {
  durations.session().guilds_first_join_floor_ms
}

fn join_grace_ms() -> Int {
  durations.session().guilds_join_grace_ms
}

const max_retry_attempts = 25

fn down_reconnect_ms() -> Int {
  durations.session().guilds_down_reconnect_ms
}

pub fn connect(state: State, guild: GuildId, attempt: Int) -> State {
  case links.get(state.links, guild) {
    None | Some(Linked(_, _)) -> state
    Some(link) -> {
      use <- bool.guard(links.is_in_flight(state.links, guild), state)
      case links.in_flight_count(state.links) >= links.max_in_flight {
        True -> {
          st.schedule(state, busy_retry_ms(), ConnectGuild(guild, attempt))
          state
        }
        False -> admit(state, guild, link, attempt)
      }
    }
  }
}

fn admit(state: State, guild: GuildId, link: Link, attempt: Int) -> State {
  let now = st.now_ms()
  case join_budget.try_send(st.guild_key(guild), process.self(), now) {
    OverQuota(retry_after_ms) -> {
      let internal = state.internal
      join_budget.wait(st.guild_key(guild), process.self(), fn() {
        process.send(internal, ConnectGuild(guild, attempt))
      })
      st.schedule(state, retry_after_ms, ConnectGuild(guild, attempt))
      state
    }
    Send -> {
      let #(links_now, token) = links.begin(state.links, guild, attempt)
      let remaining = remaining_ms(state, now)
      let request =
        JoinRequest(
          session: state.me,
          flags: state.flags,
          class: class(state, guild, link),
          mode: mode(state, guild),
          presence: session_presence(state),
          bot: state.bot,
          staff: state.staff,
          remaining_ms: remaining,
          attempt: token,
        )
      guild_client.join(guild, request, state.join_replies)
      st.schedule(state, remaining + join_grace_ms(), JoinExpired(guild, token))
      State(..state, links: links_now)
    }
  }
}

fn remaining_ms(state: State, now: Int) -> Int {
  case state.ready {
    Some(_) -> int.max(state.ready_deadline_ms - now, first_join_floor_ms())
    None -> after_ready_join_ms()
  }
}

fn class(state: State, guild: GuildId, link: Link) -> core_guild.JoinClass {
  case link {
    Detached(_) -> Resume
    _ ->
      case
        state.initial_guild == Some(guild)
        || subscriptions.is_active(state.subscriptions, st.guild_key(guild))
      {
        True -> Focused
        False -> Background
      }
  }
}

fn mode(state: State, guild: GuildId) -> core_guild.JoinMode {
  case dict.get(state.visits.guilds, st.guild_key(guild)) {
    Ok(operator) ->
      AsVisitor(
        operator:,
        adult_content_allowed: state.visits.adult_content_allowed,
        tier: state.visits.tier,
        ip_key: state.visits.ip_key,
      )
    Error(Nil) -> AsMember
  }
}

pub fn session_presence(state: State) -> presence.Presence {
  let base = [
    #("user", json.object([#("id", id.user_json(state.user_id))])),
    #(
      "status",
      json.string(
        presence.status_to_wire(presence.visible_status(state.status)),
      ),
    ),
    #("mobile", json.bool(state.mobile)),
    #("afk", json.bool(state.afk)),
  ]
  let fields = case state.custom_status {
    Some(custom) ->
      list.append(base, [#("custom_status", raw_json.to_json(custom))])
    None -> base
  }
  presence.new(
    user: state.user_id,
    status: state.status,
    mobile: state.mobile,
    afk: state.afk,
    custom_status: state.custom_status,
    wire: raw_json.from_json(json.object(fields)),
  )
}

pub fn on_reply(state: State, reply: JoinReply) -> Outcome {
  case reply {
    Joined(guild_ref, guild_create, token) ->
      joined(state, guild_ref, guild_create, token)
    JoinOverloaded(retry_after_ms, token) ->
      with_in_flight(state, token, fn(state, entry) {
        Continue(retry_at_slot(
          state,
          entry.guild,
          entry.attempt,
          retry_after_ms,
        ))
      })
    JoinUnavailable(retry_after_ms, token) ->
      with_in_flight(state, token, fn(state, entry) {
        case entry.attempt < max_retry_attempts {
          True ->
            Continue(retry_at_slot(
              state,
              entry.guild,
              entry.attempt,
              retry_after_ms,
            ))
          False -> readiness.check(give_up(state, entry.guild))
        }
      })
    JoinNotEligible(token) ->
      with_in_flight(state, token, fn(state, entry) {
        readiness.check(mark_link_unavailable(state, entry.guild, False))
      })
    VisitRejected(token) ->
      with_in_flight(state, token, fn(state, entry) {
        visit_rejected(state, entry.guild)
      })
    NotMember(token) ->
      with_in_flight(state, token, fn(state, entry) {
        case entry.attempt < links.not_member_retries {
          True -> {
            st.schedule(
              state,
              links.backoff_ms(entry.attempt),
              ConnectGuild(entry.guild, entry.attempt + 1),
            )
            Continue(state)
          }
          False -> {
            logging.log(
              logging.Info,
              "guild_connect_not_member_removed: guild_id="
                <> id.guild_id_to_string(entry.guild)
                <> " user_id="
                <> id.user_id_to_string(state.user_id),
            )
            case visits.is_visiting(state.visits, st.guild_key(entry.guild)) {
              True -> remove_visited(state, entry.guild)
              False -> {
                let state =
                  dispatch.emit(
                    state,
                    "GUILD_DELETE",
                    ready.deleted_guild(entry.guild),
                  )
                readiness.check(forget_guild(state, entry.guild))
              }
            }
          }
        }
      })
  }
}

fn with_in_flight(
  state: State,
  token: Int,
  next: fn(State, links.InFlight) -> Outcome,
) -> Outcome {
  let #(links_now, entry) = links.settle(state.links, token)
  case entry {
    None -> Continue(State(..state, links: links_now))
    Some(entry) -> {
      join_budget.settle(st.guild_key(entry.guild), process.self())
      next(State(..state, links: links_now), entry)
    }
  }
}

fn joined(
  state: State,
  guild_ref: GuildRef,
  guild_create: GuildCreate,
  token: Int,
) -> Outcome {
  let guild = guild_ref.guild
  let #(links_now, entry) = links.settle(state.links, token)
  case entry {
    Some(_) -> join_budget.settle(st.guild_key(guild), process.self())
    None -> Nil
  }
  let state = State(..state, links: links_now)
  case links.get(state.links, guild) {
    None -> {
      core_guild.send(guild_ref, Leave(state.id, process.self()))
      Continue(state)
    }
    Some(Linked(current, _)) -> {
      case current.inbox == guild_ref.inbox {
        True -> Nil
        False -> core_guild.send(guild_ref, Leave(state.id, process.self()))
      }
      Continue(state)
    }
    Some(_) -> link(state, guild_ref, guild_create)
  }
}

fn link(
  state: State,
  guild_ref: GuildRef,
  guild_create: GuildCreate,
) -> Outcome {
  let guild = guild_ref.guild
  case guild_pid(guild_ref) {
    Error(Nil) -> Continue(retry_or_fail(state, guild, 0))
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      let state =
        State(
          ..state,
          links: state.links
            |> links.put(guild, Linked(guild_ref, monitor))
            |> links.reset_sleep(guild)
            |> links.refund,
          health: dict.delete(state.health, st.guild_key(guild)),
        )
      send_hold(state, guild_ref, st.push_hold(state))
      case readiness.guild_created(state, guild_create) {
        Continue(state) -> {
          replay_subscriptions(state, guild_ref)
          register_dm_partners(state, guild_ref)
          Continue(presence_link.sync_targets(state))
        }
        stop -> stop
      }
    }
  }
}

fn register_dm_partners(state: State, guild_ref: GuildRef) -> Nil {
  case state.bot {
    True -> Nil
    False ->
      case dispatch.dm_partner_ids(state) {
        [] -> Nil
        partners ->
          core_guild.send(guild_ref, UpdateDmPartners(state.id, partners))
      }
  }
}

fn guild_pid(guild_ref: GuildRef) -> Result(process.Pid, Nil) {
  use <- result.lazy_or(process.subject_owner(guild_ref.inbox))
  process.subject_name(guild_ref.inbox) |> result.try(process.named)
}

fn replay_subscriptions(state: State, guild_ref: GuildRef) -> Nil {
  case
    subscriptions.replay(state.subscriptions, st.guild_key(guild_ref.guild))
  {
    Some(raw) ->
      case payload.raw_to_dynamic(raw) {
        Ok(data) ->
          core_guild.send(guild_ref, SessionOp(state.me, LazyRequest(data)))
        Error(Nil) -> Nil
      }
    None -> Nil
  }
}

pub fn on_expired(state: State, guild: GuildId, token: Int) -> Outcome {
  let #(links_now, entry) = links.settle(state.links, token)
  case entry {
    None -> Continue(state)
    Some(entry) -> {
      join_budget.settle(st.guild_key(guild), process.self())
      logging.log(
        logging.Warning,
        "guild_connect_timeout: guild_id="
          <> id.guild_id_to_string(guild)
          <> " user_id="
          <> id.user_id_to_string(state.user_id),
      )
      readiness.check(retry_or_fail(
        State(..state, links: links_now),
        guild,
        entry.attempt,
      ))
    }
  }
}

fn retry_at_slot(
  state: State,
  guild: GuildId,
  attempt: Int,
  retry_after_ms: Int,
) -> State {
  let #(links_now, sleep) =
    links.next_sleep(state.links, guild, Slot(retry_after_ms))
  st.schedule(state, sleep, ConnectGuild(guild, attempt + 1))
  State(..state, links: links_now)
}

fn retry_or_fail(state: State, guild: GuildId, attempt: Int) -> State {
  case links.spend(state.links) {
    Ok(spent) -> {
      let #(links_now, sleep) = links.next_sleep(spent, guild, NoHint)
      st.schedule(state, sleep, ConnectGuild(guild, attempt + 1))
      State(..state, links: links_now)
    }
    Error(Nil) -> give_up(state, guild)
  }
}

fn give_up(state: State, guild: GuildId) -> State {
  logging.log(
    logging.Warning,
    "guild_connect_exhausted: guild_id="
      <> id.guild_id_to_string(guild)
      <> " user_id="
      <> id.user_id_to_string(state.user_id),
  )
  let state = mark_link_unavailable(state, guild, False)
  st.schedule(
    state,
    links.jitter(links.slow_retry_ms(), 20),
    ConnectGuild(guild, 0),
  )
  state
}

fn mark_link_unavailable(state: State, guild: GuildId, hidden: Bool) -> State {
  case links.get(state.links, guild) {
    Some(LinkUnavailable(_, True)) -> state
    Some(LinkUnavailable(_, False)) | Some(Waiting(_)) | Some(Detached(_)) -> {
      let state =
        State(
          ..state,
          links: links.put(state.links, guild, LinkUnavailable(guild, False)),
        )
      readiness.mark_unavailable(state, guild, hidden)
    }
    _ -> state
  }
}

type DownCause {
  Handoff
  Unreachable
  Crashed
}

pub fn on_down(
  state: State,
  monitor: Monitor,
  reason: process.ExitReason,
) -> Option(Outcome) {
  links.by_monitor(state.links, monitor)
  |> option.map(fn(guild) {
    case down_cause(reason) {
      Handoff -> Continue(detach(state, guild, 0))
      Unreachable | Crashed -> {
        let state = readiness.mark_unavailable(state, guild, False)
        Continue(detach(state, guild, links.jitter(down_reconnect_ms(), 82)))
      }
    }
  })
}

fn down_cause(reason: process.ExitReason) -> DownCause {
  case reason {
    process.Killed | process.Normal -> Crashed
    process.Abnormal(term) ->
      case decode.run(term, atom.decoder()) {
        Ok(name) ->
          case atom.to_string(name) {
            "handoff" -> Handoff
            "noconnection" -> Unreachable
            _ -> Crashed
          }
        Error(_) ->
          case decode.run(term, shutdown_decoder()) {
            Ok("handoff") -> Handoff
            _ -> Crashed
          }
      }
  }
}

fn shutdown_decoder() -> decode.Decoder(String) {
  use tag <- decode.field(0, atom.decoder())
  use detail <- decode.field(1, atom.decoder())
  case atom.to_string(tag) {
    "shutdown" -> decode.success(atom.to_string(detail))
    _ -> decode.failure("", "shutdown")
  }
}

pub fn rebind(state: State, guild_ref: GuildRef) -> State {
  case links.get(state.links, guild_ref.guild), guild_pid(guild_ref) {
    Some(Linked(_, monitor)), Ok(pid) -> {
      process.demonitor_process(monitor)
      replay(state, guild_ref, subscriptions.replay_threads)
      replay(state, guild_ref, subscriptions.replay_member_lists)
      send_hold(state, guild_ref, st.push_hold(state))
      State(
        ..state,
        links: links.put(
          state.links,
          guild_ref.guild,
          Linked(guild_ref, process.monitor(pid)),
        ),
      )
    }
    _, _ -> state
  }
}

fn replay(
  state: State,
  guild_ref: GuildRef,
  build: fn(subscriptions.Subscriptions, Int) -> Option(raw_json.RawJson),
) -> Nil {
  case
    build(state.subscriptions, st.guild_key(guild_ref.guild))
    |> option.then(fn(raw) { payload.raw_to_dynamic(raw) |> option.from_result })
  {
    Some(data) ->
      core_guild.send(guild_ref, SessionOp(state.me, LazyRequest(data)))
    None -> Nil
  }
}

pub fn adopt_moved(
  state: State,
  refs: List(GuildRef),
) -> #(State, List(GuildId)) {
  list.fold(refs, #(state, []), fn(acc, guild_ref) {
    let #(state, adopted) = acc
    case links.get(state.links, guild_ref.guild), guild_pid(guild_ref) {
      Some(Waiting(_)), Ok(pid) -> #(
        State(
          ..state,
          links: links.put(
            state.links,
            guild_ref.guild,
            Linked(guild_ref, process.monitor(pid)),
          ),
        ),
        [guild_ref.guild, ..adopted],
      )
      _, _ -> acc
    }
  })
}

pub fn unmoved(state: State, guild: GuildId) -> State {
  relink(state, guild, 0, True)
}

pub fn guild_down(state: State, guild: GuildId, retry_after_ms: Int) -> State {
  relink(state, guild, retry_after_ms, False)
}

pub fn guild_lost(state: State, guild: GuildId, retry_after_ms: Int) -> State {
  relink(state, guild, retry_after_ms, True)
}

fn relink(state: State, guild: GuildId, delay_ms: Int, lossy: Bool) -> State {
  case links.get(state.links, guild) {
    Some(Linked(_, monitor)) -> {
      process.demonitor_process(monitor)
      let state = case lossy {
        True -> readiness.mark_unavailable(state, guild, False)
        False -> state
      }
      detach(state, guild, delay_ms)
    }
    _ -> state
  }
}

fn detach(state: State, guild: GuildId, delay_ms: Int) -> State {
  st.schedule(state, delay_ms, ConnectGuild(guild, 0))
  State(
    ..state,
    links: links.put(state.links, guild, Detached(guild)),
    health: dict.delete(state.health, st.guild_key(guild)),
  )
}

pub fn leave(state: State, guild: GuildId) -> Outcome {
  case links.get(state.links, guild) {
    None -> Continue(state)
    Some(link) -> {
      case link {
        Linked(_, monitor) -> process.demonitor_process(monitor)
        _ -> Nil
      }
      let key = st.guild_key(guild)
      let state = State(..state, visits: visits.drop(state.visits, key))
      let state = case set.contains(state.unannounced, key) {
        True -> state
        False ->
          dispatch.emit(state, "GUILD_DELETE", ready.deleted_guild(guild))
      }
      readiness.check(forget_guild(state, guild))
    }
  }
}

fn forget_guild(state: State, guild: GuildId) -> State {
  let key = st.guild_key(guild)
  case links.is_in_flight(state.links, guild) {
    True -> join_budget.settle(key, process.self())
    False -> Nil
  }
  let had_partners = dict.has_key(state.dm_mutual, key)
  let state =
    State(
      ..state,
      links: links.remove(state.links, guild),
      health: dict.delete(state.health, key),
      subscriptions: subscriptions.remove(state.subscriptions, key),
      collected: readiness.forget_collected(state.collected, guild),
      dm_mutual: dict.delete(state.dm_mutual, key),
      unannounced: set.delete(state.unannounced, key),
    )
  case had_partners {
    True -> presence_link.sync_targets(state)
    False -> state
  }
}

pub fn forced_unavailable(state: State, guild: GuildId, hidden: Bool) -> State {
  case links.get(state.links, guild) {
    Some(Linked(_, monitor)) -> process.demonitor_process(monitor)
    _ -> Nil
  }
  let state =
    dispatch.emit(state, "GUILD_DELETE", ready.unavailable_guild(guild, hidden))
  st.schedule(state, 0, ConnectGuild(guild, 0))
  State(
    ..state,
    links: links.put(state.links, guild, LinkUnavailable(guild, True)),
    health: dict.delete(state.health, st.guild_key(guild)),
  )
}

pub fn guild_join(state: State, guild: GuildId) -> State {
  case seed.retain_in_shard([guild], state.shard) {
    [] -> state
    _ -> {
      let state =
        State(
          ..state,
          visits: visits.drop(state.visits, st.guild_key(guild)),
          pending_bot_joins: case state.bot {
            True -> set.insert(state.pending_bot_joins, st.guild_key(guild))
            False -> state.pending_bot_joins
          },
        )
      let state = case links.has(state.links, guild) {
        True -> state
        False ->
          State(
            ..state,
            links: state.links
              |> links.put(guild, Waiting(guild))
              |> links.grow_budget,
            unannounced: case state.ready {
              None -> set.insert(state.unannounced, st.guild_key(guild))
              Some(_) -> state.unannounced
            },
          )
      }
      st.schedule(state, 0, ConnectGuild(guild, 0))
      state
    }
  }
}

pub fn visit_start(state: State, guild: GuildId, operator: Bool) -> State {
  let key = st.guild_key(guild)
  case
    visits.decide_start(
      state.visits,
      key,
      bot: state.bot,
      known: links.has(state.links, guild),
    )
  {
    Ignore -> state
    MarkVisit -> State(..state, visits: visits.put(state.visits, key, operator))
    ConnectVisit -> {
      st.schedule(state, 0, ConnectGuild(guild, 0))
      State(
        ..state,
        visits: visits.put(state.visits, key, operator),
        links: state.links
          |> links.put(guild, Waiting(guild))
          |> links.grow_budget,
      )
    }
  }
}

pub fn visit_end(state: State, guild: GuildId) -> Outcome {
  case visits.is_visiting(state.visits, st.guild_key(guild)) {
    False -> Continue(state)
    True -> {
      case links.linked_ref(state.links, guild) {
        Some(guild_ref) ->
          core_guild.send(guild_ref, Leave(state.id, process.self()))
        None -> Nil
      }
      remove_visited(state, guild)
    }
  }
}

pub fn visit_rejected(state: State, guild: GuildId) -> Outcome {
  case
    visits.is_visiting(state.visits, st.guild_key(guild)),
    links.get(state.links, guild)
  {
    True, _ -> remove_visited(state, guild)
    False, Some(Linked(_, monitor)) -> {
      process.demonitor_process(monitor)
      st.schedule(state, 0, ConnectGuild(guild, 0))
      Continue(
        State(..state, links: links.put(state.links, guild, Waiting(guild))),
      )
    }
    False, _ -> Continue(state)
  }
}

pub fn visit_promoted(
  state: State,
  guild_ref: GuildRef,
  guild_create: GuildCreate,
) -> Outcome {
  let guild = guild_ref.guild
  let state =
    State(..state, visits: visits.drop(state.visits, st.guild_key(guild)))
  case links.linked_ref(state.links, guild) {
    Some(current) if current.inbox == guild_ref.inbox ->
      readiness.guild_created(
        State(
          ..state,
          collected: readiness.forget_collected(state.collected, guild),
        ),
        guild_create,
      )
    _ -> Continue(state)
  }
}

fn remove_visited(state: State, guild: GuildId) -> Outcome {
  let state =
    State(..state, visits: visits.drop(state.visits, st.guild_key(guild)))
  case links.get(state.links, guild) {
    Some(Linked(_, monitor)) -> process.demonitor_process(monitor)
    _ -> Nil
  }
  let state = forget_guild(state, guild)
  let state = dispatch.emit(state, "GUILD_DELETE", ready.deleted_guild(guild))
  readiness.check(state)
}

pub fn health(state: State, guild: GuildId, degraded: Bool) -> State {
  case links.linked_ref(state.links, guild) {
    None -> state
    Some(_) -> {
      let key = st.guild_key(guild)
      let previous = dict.get(state.health, key) |> result.unwrap(False)
      let state =
        State(..state, health: dict.insert(state.health, key, degraded))
      case previous != degraded && st.is_ready_sent(state) {
        True -> readiness.health_update(state, guild, degraded)
        False -> state
      }
    }
  }
}

pub fn dm_mutual(state: State, guild: GuildId, partners: PackedIds) -> State {
  case links.linked_ref(state.links, guild) {
    None -> state
    Some(_) -> {
      let key = st.guild_key(guild)
      let ids = partners |> packed_ids.to_list |> set.from_list
      let next = case set.is_empty(ids) {
        True -> dict.delete(state.dm_mutual, key)
        False -> dict.insert(state.dm_mutual, key, ids)
      }
      case next == state.dm_mutual {
        True -> state
        False -> presence_link.sync_targets(State(..state, dm_mutual: next))
      }
    }
  }
}

pub fn repair(state: State) -> State {
  let now = st.now_ms()
  case links.repair_due(state.links, now) {
    False -> state
    True -> {
      let #(links_now, stalled) = links.stalled(state.links, now)
      case stalled {
        [] -> Nil
        _ ->
          logging.log(
            logging.Info,
            "session_guild_connect_repair: session_id="
              <> st.session_id_text(state)
              <> " user_id="
              <> id.user_id_to_string(state.user_id),
          )
      }
      list.each(stalled, fn(guild) {
        st.schedule(state, 0, ConnectGuild(guild, 0))
      })
      State(..state, links: links_now)
    }
  }
}

pub fn send_push_hold(state: State) -> Nil {
  let hold = st.push_hold(state)
  list.each(links.linked(state.links), send_hold(state, _, hold))
}

fn send_hold(state: State, guild_ref: GuildRef, hold: Bool) -> Nil {
  case state.bot {
    True -> Nil
    False -> core_guild.send(guild_ref, SessionPushHold(state.id, hold))
  }
}

pub fn leave_all(state: State) -> Nil {
  list.each(links.linked(state.links), fn(guild_ref) {
    core_guild.send(guild_ref, Leave(state.id, process.self()))
  })
}

pub fn send_op(state: State, guild: GuildId, op: session.ClientOp) -> Bool {
  case links.linked_ref(state.links, guild) {
    Some(guild_ref) -> {
      core_guild.send(guild_ref, SessionOp(state.me, op))
      True
    }
    None -> False
  }
}
