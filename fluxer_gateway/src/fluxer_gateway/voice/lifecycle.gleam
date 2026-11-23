// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/channel
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_state.{
  type Ctx, type Pending, type State, type Step, Recent, State,
}
import fluxer_gateway/voice/store
import fluxer_gateway/voice/virtual_access
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn force_disconnect_retry_ms() -> Int {
  durations.voice().lifecycle_force_disconnect_retry_ms
}

const max_pending = 1000

const max_recent = 500

const max_room_keys = 1000

fn recent_ttl_ms() -> Int {
  durations.voice().lifecycle_recent_ttl_ms
}

fn sweep_recent_ttl_ms() -> Int {
  durations.voice().lifecycle_sweep_recent_ttl_ms
}

pub type Visibility {
  Add
  Remove
}

pub fn store_pending(step: Step, pending: Pending) -> Result(Step, Nil) {
  let key = guild_state.pending_key(pending)
  let full =
    store.size(step.state.pending) >= max_pending
    && !store.has_connection(step.state.pending, voice_key.connection(key))
  case full {
    True -> Error(Nil)
    False ->
      Ok(
        guild_state.map_state(step, fn(state) {
          State(..state, pending: store.put(state.pending, key, pending))
        }),
      )
  }
}

pub fn abandon_transition(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
) -> Step {
  case guild_state.transition_in_use(step.state, user, channel) {
    True -> step
    False ->
      step
      |> clear_transition(user, channel)
      |> cleanup_access(ctx, user)
  }
}

pub fn forget_key_if_idle(step: Step, channel: Option(ChannelId)) -> Step {
  case channel {
    None -> step
    Some(channel) ->
      guild_state.map_state(step, fn(state) {
        let pending_e2ee =
          state.pending
          |> store.in_channel(channel)
          |> list.any(fn(pair) { entry.e2ee_capable({ pair.1 }.entry) })
        State(
          ..state,
          room_keys: e2ee.forget_if_idle(
            state.room_keys,
            channel,
            guild_state.entries(state),
            pending_e2ee,
          ),
        )
      })
  }
}

pub fn forget_keys_for(step: Step, removed: List(#(VoiceKey, Entry))) -> Step {
  list.fold(removed, step, fn(acc, pair) {
    forget_key_if_idle(acc, entry.channel(pair.1))
  })
}

pub fn cache_recent(
  step: Step,
  removed: List(#(VoiceKey, Entry)),
  now: Int,
) -> Step {
  guild_state.map_state(step, fn(state) {
    let kept =
      store.filter(state.recent, fn(_key, recent) {
        now - recent.disconnected_at < recent_ttl_ms()
      })
    let recent =
      list.fold(removed, kept, fn(acc, pair) {
        let #(key, value) = pair
        store.put(
          acc,
          key,
          Recent(
            value,
            guild_state.geo_of(state, voice_key.connection(key)),
            now,
          ),
        )
      })
    State(..state, recent:)
  })
}

pub fn clear_recent(step: Step, connections: List(ConnectionId)) -> Step {
  guild_state.map_state(step, fn(state) {
    State(
      ..state,
      recent: list.fold(connections, state.recent, store.delete_connection),
    )
  })
}

pub fn remove_states(step: Step, removed: List(#(VoiceKey, Entry))) -> Step {
  guild_state.map_state(step, fn(state) {
    let gone = list.map(removed, fn(pair) { pair.0 })
    State(
      ..state,
      states: store.drop(state.states, gone),
      geo: dict.drop(state.geo, list.map(gone, voice_key.connection)),
    )
  })
}

pub fn connections_of(removed: List(#(VoiceKey, Entry))) -> List(ConnectionId) {
  list.map(removed, fn(pair) { voice_key.connection(pair.0) })
}

fn clear_pending_for_channel(step: Step, ctx: Ctx, channel: ChannelId) -> Step {
  release_pending(step, ctx, fn(key, _pending) {
    voice_key.channel(key) != channel
  })
}

fn clear_recent_for_channel(step: Step, channel: ChannelId) -> Step {
  guild_state.map_state(step, fn(state) {
    State(
      ..state,
      recent: store.filter(state.recent, fn(key, _recent) {
        voice_key.channel(key) != channel
      }),
    )
  })
}

pub fn disconnect_channel(step: Step, ctx: Ctx, channel: ChannelId) -> Step {
  let step = clear_pending_for_channel(step, ctx, channel)
  let removed = store.in_channel(step.state.states, channel)
  case removed {
    [] -> forget_key_if_idle(step, Some(channel))
    _ -> {
      list.each(removed, fn(pair) {
        guild_state.spawn_force_disconnect(step.state.guild, pair.1)
      })
      let step =
        step
        |> remove_states(removed)
        |> clear_recent_for_channel(channel)
        |> forget_key_if_idle(Some(channel))
        |> guild_state.broadcast_disconnects(removed)
      removed
      |> list.map(fn(pair) { voice_key.user(pair.0) })
      |> list.unique
      |> list.fold(step, fn(acc, user) { cleanup_access(acc, ctx, user) })
    }
  }
}

fn release_pending(
  step: Step,
  ctx: Ctx,
  keep: fn(VoiceKey, Pending) -> Bool,
) -> Step {
  let #(kept, removed) = store.partition(step.state.pending, keep)
  let step =
    guild_state.map_state(step, fn(state) { State(..state, pending: kept) })
  let step = broadcast_moved_removals(step, removed)
  let step =
    list.fold(removed, step, fn(acc, pair) {
      let #(_, pending) = pair
      forget_key_if_idle(acc, Some(pending.channel))
    })
  release_transitions(step, ctx, list.map(removed, fn(pair) { pair.1 }))
}

fn release_transitions(step: Step, ctx: Ctx, released: List(Pending)) -> Step {
  let cleared =
    released
    |> list.map(fn(pending) { #(pending.user, pending.channel) })
    |> list.unique
    |> list.filter(fn(pair) {
      !guild_state.transition_in_use(step.state, pair.0, pair.1)
    })
  let step =
    guild_state.map_state(step, fn(state) {
      State(
        ..state,
        access: virtual_access.clear_transitions(state.access, cleared),
      )
    })
  released
  |> list.map(fn(pending) { pending.user })
  |> list.unique
  |> list.fold(step, fn(acc, user) { cleanup_access(acc, ctx, user) })
}

fn broadcast_moved_removals(
  step: Step,
  removed: List(#(VoiceKey, Pending)),
) -> Step {
  list.fold(removed, step, fn(acc, pair) {
    let #(key, pending) = pair
    let connected =
      store.has_connection(acc.state.states, voice_key.connection(key))
    case pending.moved && !connected {
      True ->
        guild_state.broadcast(
          acc,
          entry.disconnected(pending.entry),
          Some(pending.channel),
        )
      False -> acc
    }
  })
}

pub fn clear_pending_connection(
  step: Step,
  ctx: Ctx,
  connection_id: ConnectionId,
  user: UserId,
) -> Step {
  release_pending(step, ctx, fn(key, _pending) {
    !{
      voice_key.connection(key) == connection_id && voice_key.user(key) == user
    }
  })
}

pub fn clear_pending_for_user(
  step: Step,
  ctx: Ctx,
  user: UserId,
  session_id: Option(String),
) -> Step {
  release_pending(step, ctx, fn(key, pending) {
    !{
      voice_key.user(key) == user
      && session_matches(pending.session_id, session_id)
    }
  })
}

pub fn clear_pending_for_user_channel(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
) -> Step {
  release_pending(step, ctx, fn(key, _pending) {
    !{ voice_key.user(key) == user && voice_key.channel(key) == channel }
  })
}

pub fn session_matches(
  stored: Option(String),
  requested: Option(String),
) -> Bool {
  case requested {
    None -> True
    Some(_) -> stored == requested
  }
}

pub fn sweep(state: State, now: Int) -> State {
  let recent =
    state.recent
    |> store.filter(fn(_key, recent) {
      now - recent.disconnected_at < sweep_recent_ttl_ms()
    })
    |> store.to_list
    |> list.take(max_recent)
    |> store.from_list
  let room_keys = cap(state.room_keys, max_room_keys)
  State(..state, recent:, room_keys:)
}

fn cap(map: Dict(k, v), max: Int) -> Dict(k, v) {
  case dict.size(map) <= max {
    True -> map
    False ->
      map
      |> dict.to_list
      |> list.take(max)
      |> dict.from_list
  }
}

pub fn sweep_pending(step: Step, ctx: Ctx) -> Step {
  let now = ctx.now_ms
  let #(kept, expired) =
    store.partition(step.state.pending, fn(_key, pending) {
      now < pending.expires_at
    })
  case expired {
    [] -> step
    _ -> {
      let guild = step.state.guild
      list.each(expired, fn(pair) {
        let #(key, _pending) = pair
        process_force_disconnect(guild, key)
      })
      let step =
        broadcast_moved_removals(step, expired)
        |> guild_state.map_state(fn(state) { State(..state, pending: kept) })
      let step =
        list.fold(expired, step, fn(acc, pair) {
          let #(_, pending) = pair
          forget_key_if_idle(acc, Some(pending.channel))
        })
      release_transitions(step, ctx, list.map(expired, fn(pair) { pair.1 }))
    }
  }
}

fn process_force_disconnect(guild: id.GuildId, key: VoiceKey) -> Nil {
  process.spawn_unlinked(fn() { retry_force_disconnect(guild, key, 3) })
  Nil
}

fn retry_force_disconnect(
  guild: id.GuildId,
  key: VoiceKey,
  retries: Int,
) -> Nil {
  case retries {
    0 -> Nil
    _ ->
      case
        guild_state.force_disconnect(
          Some(guild),
          voice_key.channel(key),
          voice_key.user(key),
          voice_key.connection(key),
        )
      {
        Ok(Nil) -> Nil
        Error(Nil) -> {
          process.sleep(force_disconnect_retry_ms())
          retry_force_disconnect(guild, key, retries - 1)
        }
      }
  }
}

fn visibility(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
  change: Visibility,
) -> Step {
  let guild_id = id.guild_json(step.state.guild)
  let channel_id = id.channel_json(channel)
  let message = case change, ctx.channel(channel) {
    Add, Some(value) ->
      Ok(#("CHANNEL_CREATE", channel_with_ids(value, channel_id, guild_id)))
    Add, None -> Error(Nil)
    Remove, _ ->
      Ok(#(
        "CHANNEL_DELETE",
        json.object([#("id", channel_id), #("guild_id", guild_id)])
          |> raw_json.from_json,
      ))
  }
  case message {
    Error(Nil) -> step
    Ok(#(event, payload)) ->
      ctx.sessions_of(user)
      |> list.fold(step, fn(acc, session) {
        guild_state.emit(acc, guild_state.ToSession(session, event, payload))
      })
  }
}

fn channel_with_ids(
  value: channel.Channel,
  channel_id: json.Json,
  guild_id: json.Json,
) -> raw_json.RawJson {
  {
    use raw <- result.try(raw_json.with_field(
      channel.raw(value),
      "id",
      raw_json.from_json(channel_id),
    ))
    raw_json.with_field(raw, "guild_id", raw_json.from_json(guild_id))
  }
  |> result.lazy_unwrap(fn() {
    raw_json.from_json(
      json.object([#("guild_id", guild_id), #("id", channel_id)]),
    )
  })
}

pub fn grant_access(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
) -> Step {
  let step =
    guild_state.map_state(step, fn(state) {
      State(..state, access: virtual_access.grant(state.access, user, channel))
    })
  visibility(step, ctx, user, channel, Add)
}

pub fn cleanup_access(step: Step, ctx: Ctx, user: UserId) -> Step {
  virtual_access.channels(step.state.access, user)
  |> list.fold(step, fn(acc, channel) {
    let connected =
      store.keys_of_user(acc.state.states, user)
      |> list.any(fn(key) { voice_key.channel(key) == channel })
    let held = virtual_access.is_held(acc.state.access, user, channel)
    case connected || held {
      True -> acc
      False -> {
        let had = virtual_access.has_access(acc.state.access, user, channel)
        let acc =
          guild_state.map_state(acc, fn(state) {
            State(
              ..state,
              access: virtual_access.revoke(state.access, user, channel),
            )
          })
        let visible =
          permissions.has(
            guild_state.permissions_of(ctx, user, channel),
            permissions.ViewChannel,
          )
        case had && !visible {
          True -> visibility(acc, ctx, user, channel, Remove)
          False -> acc
        }
      }
    }
  })
}

pub fn cleanup_after_disconnect(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: Option(ChannelId),
) -> Step {
  let held = case channel {
    Some(channel) -> virtual_access.is_held(step.state.access, user, channel)
    None -> False
  }
  case held {
    True -> step
    False -> cleanup_access(step, ctx, user)
  }
}

pub fn cleanup_if_gone(step: Step, ctx: Ctx, user: UserId) -> Step {
  case store.has_user(step.state.states, user) {
    True -> step
    False -> cleanup_access(step, ctx, user)
  }
}

pub fn drop_orphan_transitions(state: State) -> State {
  State(
    ..state,
    access: virtual_access.retain_transitions(state.access, fn(user, channel) {
      guild_state.transition_in_use(state, user, channel)
    }),
  )
}

pub fn clear_transition(step: Step, user: UserId, channel: ChannelId) -> Step {
  guild_state.map_state(step, fn(state) {
    case guild_state.transition_in_use(state, user, channel) {
      True -> state
      False ->
        State(
          ..state,
          access: virtual_access.clear_transition(state.access, user, channel),
        )
    }
  })
}

pub fn mark_transition(step: Step, user: UserId, channel: ChannelId) -> Step {
  guild_state.map_state(step, fn(state) {
    State(
      ..state,
      access: virtual_access.mark_transition(state.access, user, channel),
    )
  })
}
