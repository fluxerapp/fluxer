// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{
  type DispatchFacts, Channel, Guild, Message, Presence, Reaction, Scoped,
  Unmarked, Unscoped, UserSettings, UserUpdate,
}
import fluxer_gateway/core/guild.{UpdateDmPartners} as core_guild
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/user
import fluxer_gateway/presence/status as presence_status
import fluxer_gateway/session/links
import fluxer_gateway/session/payload as body
import fluxer_gateway/session/pending_presences.{type Held, Held}
import fluxer_gateway/session/presence_link
import fluxer_gateway/session/presence_view
import fluxer_gateway/session/reactions.{Buffered}
import fluxer_gateway/session/relations
import fluxer_gateway/session/state.{type State, FlushReactions, State} as st
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

const max_dm_partners = 1000

pub type Source {
  Direct
  FromGuild(guild: GuildId)
  FromPresence(user: UserId)
}

pub fn emit(state: State, event: String, payload: RawJson) -> State {
  incoming(state, Direct, event, payload, dispatch_facts.scan(event, payload))
}

pub fn incoming(
  state: State,
  source: Source,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> State {
  let state = track_user(state, facts)
  case thread_backstop(state, event, payload, facts) {
    Error(Nil) -> state
    Ok(payload) -> {
      let withheld =
        skip_for_shard(state, source, event, facts)
        || ignored(state, event, facts)
      let #(state, payload) = bot_guild_event(state, event, payload, facts)
      case withheld {
        True -> track_relations(state, event, facts)
        False ->
          route(announce(state, event, facts), source, event, payload, facts)
      }
    }
  }
}

fn thread_backstop(
  state: State,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> Result(RawJson, Nil) {
  let capable = state.bot || state.flags.channel_threads
  case !capable && is_thread_event(event), facts.thread, capable {
    True, _, _ -> Error(Nil)
    False, Unmarked, _ -> Ok(payload)
    False, Scoped, True -> Ok(without(payload, dispatch_facts.scoped_marker))
    False, Scoped, False -> Error(Nil)
    False, Unscoped, True -> Error(Nil)
    False, Unscoped, False ->
      Ok(without(payload, dispatch_facts.unscoped_marker))
  }
}

fn is_thread_event(event: String) -> Bool {
  string.starts_with(event, "THREAD_") || event == "FORUM_UNREADS"
}

fn without(payload: RawJson, key: String) -> RawJson {
  raw_json.without_field(payload, key) |> result.unwrap(payload)
}

fn track_user(state: State, facts: DispatchFacts) -> State {
  case facts.detail {
    UserSettings(status: Some(text)) ->
      case presence_status.settings_invisible(Some(text)) {
        Some(invisible) -> State(..state, settings_invisible: invisible)
        None -> state
      }
    UserUpdate(restricted: Some(restricted)) ->
      State(
        ..state,
        user: user.new(
          id: user.id(state.user),
          bot: user.is_bot(state.user),
          flags: user.flags(state.user),
          restricted:,
          wire: user.wire(state.user),
        ),
      )
    _ -> state
  }
}

fn shard_filter_active(state: State) -> Bool {
  case state.shard {
    Some(#(shard_id, _)) -> shard_id != 0
    None -> False
  }
}

fn skip_for_shard(
  state: State,
  source: Source,
  event: String,
  facts: DispatchFacts,
) -> Bool {
  case shard_filter_active(state), source {
    False, _ -> False
    True, FromGuild(_) -> False
    True, _ -> !has_guild_context(event, facts)
  }
}

fn has_guild_context(event: String, facts: DispatchFacts) -> Bool {
  case event, facts.guild_id, facts.id {
    "RATE_LIMITED", _, _
    | "GUILD_COUNTS_UPDATE", _, _
    | "CHANNEL_MEMBER_COUNTS_UPDATE", _, _
    -> True
    _, Some(_), _ -> True
    "GUILD_CREATE", None, Some(_)
    | "GUILD_UPDATE", None, Some(_)
    | "GUILD_DELETE", None, Some(_)
    | "GUILD_SYNC", None, Some(_)
    -> True
    _, _, _ -> False
  }
}

fn ignored(state: State, event: String, facts: DispatchFacts) -> Bool {
  set.contains(state.ignored, event)
  && !{ event == "MESSAGE_CREATE" && is_mentioned(state, facts) }
}

fn is_mentioned(state: State, facts: DispatchFacts) -> Bool {
  case facts.detail {
    Message(everyone:, here:, mentions:) ->
      { everyone && !here }
      || here
      || list.contains(mentions, st.user_int(state))
    _ -> False
  }
}

fn announce(state: State, event: String, facts: DispatchFacts) -> State {
  case event, facts.id {
    "GUILD_CREATE", Some(guild) | "GUILD_DELETE", Some(guild) ->
      State(..state, unannounced: set.delete(state.unannounced, guild))
    _, _ -> state
  }
}

fn bot_guild_event(
  state: State,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> #(State, RawJson) {
  case state.bot, event, facts.id, facts.detail {
    False, _, _, _ -> #(state, payload)
    True, "GUILD_CREATE", Some(guild), Guild(..) ->
      case set.contains(state.pending_bot_joins, guild) {
        True -> #(
          State(
            ..state,
            pending_bot_joins: set.delete(state.pending_bot_joins, guild),
          ),
          payload,
        )
        False -> #(state, body.mark_available(payload))
      }
    True, "GUILD_CREATE", None, Guild(..) -> #(
      state,
      body.mark_available(payload),
    )
    True, "GUILD_DELETE", Some(guild), Guild(unavailable: False, ..) -> #(
      State(
        ..state,
        pending_bot_joins: set.delete(state.pending_bot_joins, guild),
      ),
      payload,
    )
    True, _, _, _ -> #(state, payload)
  }
}

fn route(
  state: State,
  source: Source,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> State {
  case event, state.flags.debounce_message_reactions, facts.detail {
    "MESSAGE_REACTION_ADD", True, Reaction(reaction) -> {
      let Buffered(held, arm) =
        reactions.hold(state.reactions, reaction, payload)
      case arm {
        True -> st.schedule(state, reactions.interval_ms(), FlushReactions)
        False -> Nil
      }
      State(..state, reactions: held)
    }
    "MESSAGE_REACTION_REMOVE", _, Reaction(reaction) ->
      case reactions.cancel(state.reactions, reaction) {
        Ok(remaining) -> State(..state, reactions: remaining)
        Error(Nil) -> after_reactions(state, source, event, payload, facts)
      }
    "MESSAGE_REACTION_REMOVE_ALL", _, _
    | "MESSAGE_REACTION_REMOVE_EMOJI", _, _
    | "MESSAGE_DELETE", _, _
    | "MESSAGE_DELETE_BULK", _, _
    -> after_reactions(flush_reactions(state), source, event, payload, facts)
    _, _, _ -> after_reactions(state, source, event, payload, facts)
  }
}

fn after_reactions(
  state: State,
  source: Source,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> State {
  case event {
    "PRESENCE_UPDATE" -> {
      let held = presence_held(source, payload, facts)
      case state.suppress_presences || hold_back(state, held) {
        True ->
          State(
            ..state,
            pending_presences: pending_presences.hold(
              state.pending_presences,
              held,
            ),
          )
        False -> deliver_presence(state, payload, facts)
      }
    }
    _ -> deliver(state, event, payload, facts)
  }
}

fn deliver_presence(
  state: State,
  payload: RawJson,
  facts: DispatchFacts,
) -> State {
  case presence_view.admit(state.presence_view, facts) {
    Ok(view) ->
      deliver(
        State(..state, presence_view: view),
        "PRESENCE_UPDATE",
        payload,
        facts,
      )
    Error(Nil) -> track_relations(state, "PRESENCE_UPDATE", facts)
  }
}

fn presence_held(
  source: Source,
  payload: RawJson,
  facts: DispatchFacts,
) -> Held {
  let user = case facts.detail {
    Presence(user:, ..) -> user
    _ -> None
  }
  case source {
    FromGuild(guild_id) ->
      Held(user:, guild: Some(id.guild_id_to_int(guild_id)), payload:, facts:)
    FromPresence(user_id) ->
      Held(
        user: Some(id.user_id_to_int(user_id)),
        guild: facts.guild_id,
        payload:,
        facts:,
      )
    Direct -> Held(user:, guild: facts.guild_id, payload:, facts:)
  }
}

fn hold_back(state: State, held: Held) -> Bool {
  case held.guild, held.user {
    Some(guild), _ ->
      case id.guild_id(guild) {
        Ok(guild_id) -> !links.has(state.links, guild_id)
        Error(_) -> True
      }
    None, Some(user) ->
      !relations.allows_presence(
        state.relations,
        user,
        st.user_int(state),
        st.eligible_partners(state),
      )
    None, None -> False
  }
}

fn deliver(
  state: State,
  event: String,
  payload: RawJson,
  facts: DispatchFacts,
) -> State {
  let state = st.deliver(state, event, payload)
  let state =
    State(
      ..state,
      presence_view: presence_view.observe(state.presence_view, event, facts),
    )
  track_relations(state, event, facts)
}

fn track_relations(state: State, event: String, facts: DispatchFacts) -> State {
  let change =
    relations.apply(state.relations, event, facts, st.user_int(state))
  let state = State(..state, relations: change.relations)
  let state = case event, facts.detail {
    "GUILD_DELETE", _ -> forget_deleted_guild(state, facts)
    "CHANNEL_CREATE", Channel(kind: Some(1), ..)
    | "CHANNEL_UPDATE", Channel(kind: Some(1), ..)
    | "CHANNEL_DELETE", Channel(kind: Some(1), ..)
    -> {
      register_all_dm_partners(state)
      state
    }
    _, _ -> state
  }
  let state =
    list.fold(change.released, state, fn(acc, user) { release_user(acc, user) })
  case relations.changes_targets(event) {
    True -> presence_link.sync_targets(state)
    False -> state
  }
}

pub fn dm_partner_ids(state: State) -> List(Int) {
  relations.direct_partners(state.relations, st.user_int(state))
  |> list.take(max_dm_partners)
}

fn register_all_dm_partners(state: State) -> Nil {
  case state.bot {
    True -> Nil
    False -> {
      let partners = dm_partner_ids(state)
      list.each(links.linked(state.links), fn(guild_ref) {
        core_guild.send(guild_ref, UpdateDmPartners(state.id, partners))
      })
    }
  }
}

fn forget_deleted_guild(state: State, facts: DispatchFacts) -> State {
  case facts.id {
    Some(guild) ->
      State(..state, dm_mutual: dict.delete(state.dm_mutual, guild))
    None -> state
  }
}

fn release_user(state: State, user: Int) -> State {
  let #(pending, released) =
    pending_presences.release_user(state.pending_presences, user)
  send_held(State(..state, pending_presences: pending), released)
}

fn send_held(state: State, released: List(Held)) -> State {
  list.fold(released, state, fn(acc, held) {
    case presence_view.admit(acc.presence_view, held.facts) {
      Ok(view) ->
        st.deliver(
          State(..acc, presence_view: view),
          "PRESENCE_UPDATE",
          held.payload,
        )
      Error(Nil) -> acc
    }
  })
}

fn release_presences(state: State) -> State {
  let #(pending, released) =
    pending_presences.release(state.pending_presences, fn(held) {
      !hold_back(state, held)
    })
  send_held(State(..state, pending_presences: pending), released)
}

pub fn enable_presences(state: State) -> State {
  State(..release_presences(state), suppress_presences: False)
}

pub fn flush_reactions(state: State) -> State {
  let #(empty, frames) = reactions.flush(state.reactions)
  list.fold(frames, State(..state, reactions: empty), fn(acc, frame) {
    deliver(acc, frame.0, frame.1, dispatch_facts.plain())
  })
}
