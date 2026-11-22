// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/voice_key
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/guild_state.{type Ctx, type State}
import fluxer_gateway/voice/store
import fluxer_gateway/voice/virtual_access
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set

const camera_user_limit = 25

pub fn check(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel_id: ChannelId,
  channel: Channel,
  is_update: Bool,
) -> Result(Nil, GatewayError) {
  use <- bool.guard(
    guild_state.is_timed_out(ctx, user),
    Error(error.VoiceMemberTimedOut),
  )
  use <- bool.guard(
    !has_view_and_connect(ctx, state, user, channel_id),
    Error(error.VoicePermissionDenied),
  )
  capacity(ctx, state, user, channel_id, channel, is_update)
}

pub fn has_view_and_connect(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  virtual_access.has_voice_access(state.access, user, channel)
  || virtual_access.is_move_pending(state.access, user, channel)
  || guild_state.can_view_and_connect(ctx, user, channel)
}

type Stats {
  Stats(users: set.Set(UserId), camera: Bool, own_connections: Int)
}

fn stats(ctx: Ctx, state: State, user: UserId, channel: ChannelId) -> Stats {
  let active =
    guild_state.channel_entries(state, channel)
    |> list.fold(Stats(set.new(), False, 0), fn(acc, value) {
      let other = entry.user(value)
      Stats(
        users: set.insert(acc.users, other),
        camera: acc.camera || entry.has(value, voice_state.SelfVideo),
        own_connections: case other == user {
          True -> acc.own_connections + 1
          False -> acc.own_connections
        },
      )
    })
  state.pending
  |> store.in_channel(channel)
  |> list.map(fn(pair) { pair.1 })
  |> list.filter(fn(pending) {
    pending.user != user && pending.expires_at > ctx.now_ms
  })
  |> list.fold(active, fn(acc, pending) {
    Stats(
      ..acc,
      users: set.insert(acc.users, pending.user),
      camera: acc.camera || entry.has(pending.entry, voice_state.SelfVideo),
    )
  })
}

fn capacity(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel_id: ChannelId,
  channel: Channel,
  is_update: Bool,
) -> Result(Nil, GatewayError) {
  let facts = guild_state.channel_facts(channel)
  let stats = stats(ctx, state, user, channel_id)
  use Nil <- result.try(within_user_limit(
    facts.user_limit,
    stats,
    user,
    is_update,
  ))
  let active = case is_update && stats.own_connections > 0 {
    True -> stats.own_connections - 1
    False -> stats.own_connections
  }
  let pending = pending_count(ctx, state, user, channel_id)
  case active + pending < facts.connection_limit {
    True -> Ok(Nil)
    False -> Error(error.VoiceConnectionLimitReached)
  }
}

pub fn check_user_limit(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel_id: ChannelId,
) -> Result(Nil, GatewayError) {
  case ctx.channel(channel_id) {
    None -> Error(error.VoiceChannelNotFound)
    Some(channel) ->
      within_user_limit(
        guild_state.channel_facts(channel).user_limit,
        stats(ctx, state, user, channel_id),
        user,
        False,
      )
  }
}

fn within_user_limit(
  user_limit: Int,
  stats: Stats,
  user: UserId,
  is_update: Bool,
) -> Result(Nil, GatewayError) {
  let limit = case user_limit, stats.camera {
    0, False -> 0
    0, True -> camera_user_limit
    limit, False -> limit
    limit, True -> int.min(limit, camera_user_limit)
  }
  let count = set.size(stats.users)
  let adjusted = case set.contains(stats.users, user) || is_update {
    True -> count - 1
    False -> count
  }
  case limit > 0 && adjusted >= limit {
    True -> Error(error.VoiceChannelFull)
    False -> Ok(Nil)
  }
}

fn pending_count(
  ctx: Ctx,
  state: State,
  user: UserId,
  channel: ChannelId,
) -> Int {
  state.pending
  |> store.of_user(user)
  |> list.count(fn(pair) {
    voice_key.channel(pair.0) == channel
    && { pair.1 }.expires_at > ctx.now_ms
    && !store.has_connection(state.states, voice_key.connection(pair.0))
  })
}

pub fn check_camera(
  state: State,
  user: UserId,
  channel: ChannelId,
  self_video: Bool,
) -> Result(Nil, GatewayError) {
  case self_video {
    False -> Ok(Nil)
    True -> {
      let users =
        guild_state.channel_entries(state, channel)
        |> list.filter(entry.has(_, voice_state.SelfVideo))
        |> list.map(entry.user)
        |> set.from_list
        |> set.insert(user)
      case set.size(users) > camera_user_limit {
        True -> Error(error.VoiceCameraUserLimit)
        False -> Ok(Nil)
      }
    }
  }
}
