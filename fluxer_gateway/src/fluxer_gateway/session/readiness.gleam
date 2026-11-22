// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild_create.{
  type GuildCreate, Available, Unavailable,
} as created
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/client as presence_client
import fluxer_gateway/session/calls
import fluxer_gateway/session/dispatch
import fluxer_gateway/session/links
import fluxer_gateway/session/pending_presences
import fluxer_gateway/session/presence_link
import fluxer_gateway/session/presence_view
import fluxer_gateway/session/ready.{Build}
import fluxer_gateway/session/relations
import fluxer_gateway/session/state.{type State, Attached, Detached, State} as st
import fluxer_gateway/telemetry/timings
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/set

pub type Outcome {
  Continue(State)
  Stop(State)
}

pub fn guild_created(state: State, guild_create: GuildCreate) -> Outcome {
  case state.ready, guild_create {
    None, Unavailable(..) ->
      Continue(dispatch.emit(
        state,
        "GUILD_DELETE",
        created.to_raw(guild_create),
      ))
    None, Available(..) ->
      Continue(dispatch.emit(
        state,
        "GUILD_CREATE",
        created.to_raw(guild_create),
      ))
    Some(_), _ -> check(collect(state, guild_create))
  }
}

fn collect(state: State, entry: GuildCreate) -> State {
  State(..state, collected: [
    entry,
    ..forget_collected(state.collected, created.guild(entry))
  ])
}

pub fn mark_unavailable(state: State, guild: GuildId, hidden: Bool) -> State {
  case state.ready {
    None ->
      dispatch.emit(
        state,
        "GUILD_DELETE",
        ready.unavailable_guild(guild, hidden),
      )
    Some(_) -> collect(state, Unavailable(guild, hidden))
  }
}

pub fn forget_collected(
  collected: List(GuildCreate),
  guild: GuildId,
) -> List(GuildCreate) {
  list.filter(collected, fn(entry) { created.guild(entry) != guild })
}

pub fn check(state: State) -> Outcome {
  case state.ready {
    None -> Continue(state)
    Some(_) ->
      case presence_link.is_linked(state) && links.all_resolved(state.links) {
        True -> send_ready(state)
        False -> Continue(state)
      }
  }
}

pub fn send_ready(state: State) -> Outcome {
  case state.ready, state.socket {
    None, _ -> Continue(state)
    Some(_), Detached(_) -> Stop(state)
    Some(data), Attached(_, _) -> Continue(deliver_ready(state, data))
  }
}

fn deliver_ready(state: State, data: ready.ReadyData) -> State {
  let presences = case state.bot {
    True -> []
    False -> {
      let targets = presence_link.targets(state)
      case set.is_empty(targets) {
        True -> []
        False ->
          targets
          |> set.to_list
          |> list.sort(int.compare)
          |> packed_ids.from_list
          |> presence_client.lookup_many
      }
    }
  }
  let timing = case state.staff {
    True -> Some(timings.finish(timings.step(state.timing, "ready")))
    False -> None
  }
  let built =
    ready.assemble(Build(
      data: ready.ReadyData(..data, sessions: presence_link.sessions(state)),
      session_id: st.session_id_text(state),
      version: state.version,
      shard: state.shard,
      bot: state.bot,
      staff: state.staff,
      guilds: links.guild_ids(state.links),
      collected: state.collected,
      presences:,
      timings: timing,
    ))
  let state =
    State(
      ..st.deliver_unbuffered(state, "READY", built.payload),
      presence_view: presence_view.from_ready(
        list.map(presences, presence.wire),
        built.guild_members,
        relations.me_context(state.relations, st.user_int(state)),
      ),
    )
  let state = case state.bot {
    True -> bot_guild_creates(state)
    False -> state
  }
  let state = health_after_ready(State(..state, ready: None))
  calls.announce(state)
  let covered =
    presences
    |> list.map(fn(p) { id.user_id_to_int(presence.user(p)) })
    |> set.from_list
  let state =
    State(
      ..state,
      pending_presences: pending_presences.drop_users(
        state.pending_presences,
        covered,
      ),
      collected: [],
    )
  dispatch.enable_presences(state)
}

fn bot_guild_creates(state: State) -> State {
  let state = State(..state, pending_bot_joins: set.new())
  state.collected
  |> list.reverse
  |> list.fold(state, fn(acc, collected) {
    case collected {
      Available(_, body) ->
        st.deliver(acc, "GUILD_CREATE", created.bot_wire(body))
      Unavailable(guild, hidden) ->
        st.deliver(acc, "GUILD_DELETE", ready.unavailable_guild(guild, hidden))
    }
  })
}

fn health_after_ready(state: State) -> State {
  state.health
  |> dict.to_list
  |> list.fold(state, fn(acc, entry) {
    case entry.1, id.guild_id(entry.0) {
      True, Ok(guild) ->
        case links.linked_ref(acc.links, guild) {
          Some(_) -> health_update(acc, guild, True)
          None -> acc
        }
      _, _ -> acc
    }
  })
}

pub fn health_update(state: State, guild: GuildId, degraded: Bool) -> State {
  dispatch.emit(
    state,
    "GUILD_HEALTH_UPDATE",
    raw_json.from_json(
      json.object([
        #("guild_id", id.json(id.guild_id_to_int(guild))),
        #("degraded", json.bool(degraded)),
      ]),
    ),
  )
}
