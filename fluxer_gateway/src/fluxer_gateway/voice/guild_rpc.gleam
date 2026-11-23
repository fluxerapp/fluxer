// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type GatewayError, type RpcError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/connection
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_state.{
  type Ctx, type RpcCall, type Step, ReissueJob, State,
}
import fluxer_gateway/voice/lifecycle
import fluxer_gateway/voice/member
import fluxer_gateway/voice/p2p
import fluxer_gateway/voice/permissions
import fluxer_gateway/voice/region
import fluxer_gateway/voice/store
import fluxer_gateway/voice/token
import gleam/dict
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}

fn recent_restore_ms() -> Int {
  durations.voice().guild_rpc_recent_restore_ms
}

pub fn handle(
  step: Step,
  ctx: Ctx,
  call: RpcCall,
) -> #(Step, Result(Json, RpcError)) {
  case call {
    guild_state.MemberVoice(user, mute, deaf) ->
      case member.update_member_voice(step, ctx, user, mute, deaf) {
        Ok(step) -> #(step, Ok(success([])))
        Error(_) -> #(step, Error(error.RpcInternalError))
      }
    guild_state.DisconnectIfInChannel(user, channel, conn) ->
      disconnect_if_in_channel(step, ctx, user, channel, conn)
    guild_state.Confirm(conn, nonce) -> confirm(step, ctx, conn, nonce)
    guild_state.ChannelStates(channel) -> #(
      step,
      Ok(json.object([#("voice_states", channel_states(step, channel))])),
    )
    guild_state.MoveMember(user, moderator, channel, conn) ->
      move_member(step, ctx, user, moderator, channel, conn)
    guild_state.GetVoiceState(user) -> #(
      step,
      Ok(json.object([#("voice_state", voice_state_of(step, user))])),
    )
    guild_state.SwitchRegion(channel) ->
      case region.switch_region(step, ctx, channel) {
        Ok(step) -> #(step, Ok(success([])))
        Error(_) -> #(step, Error(error.RpcInternalError))
      }
  }
}

fn voice_state_of(step: Step, user: UserId) -> Json {
  guild_state.user_entries(step.state, user)
  |> list.map(fn(pair) { pair.1 })
  |> list.sort(fn(a, b) { int.compare(entry.version(b), entry.version(a)) })
  |> list.first
  |> option.from_result
  |> json.nullable(entry.to_json(_, entry.External))
}

fn success(extra: List(#(String, Json))) -> Json {
  json.object([#("success", json.bool(True)), ..extra])
}

fn ignored() -> Json {
  success([#("ignored", json.bool(True))])
}

fn failure(reason: GatewayError) -> Json {
  json.object([
    #("success", json.bool(False)),
    #("error", json.string(normalize(reason))),
  ])
}

fn normalize(reason: GatewayError) -> String {
  case reason {
    error.VoiceUserNotInVoice -> "user_not_in_voice"
    error.VoiceChannelNotFound -> "channel_not_found"
    error.VoiceChannelNotVoice -> "channel_not_voice"
    error.VoiceModeratorMissingConnect -> "moderator_missing_connect"
    error.VoicePermissionDenied -> "target_missing_connect"
    error.VoiceConnectionNotFound | error.VoiceMissingConnectionId ->
      "connection_not_found"
    other -> atom_name(other)
  }
}

fn atom_name(reason: GatewayError) -> String {
  case reason {
    error.VoiceConnectionNotFound -> "voice_connection_not_found"
    error.VoiceChannelNotFound -> "voice_channel_not_found"
    error.VoiceChannelNotVoice -> "voice_channel_not_voice"
    error.VoiceMemberNotFound -> "voice_member_not_found"
    error.VoiceUserNotInVoice -> "voice_user_not_in_voice"
    error.VoiceGuildNotFound -> "voice_guild_not_found"
    error.VoicePermissionDenied -> "voice_permission_denied"
    error.VoiceMemberTimedOut -> "voice_member_timed_out"
    error.VoiceChannelFull -> "voice_channel_full"
    error.VoiceConnectionLimitReached -> "voice_connection_limit_reached"
    error.VoiceMissingConnectionId -> "voice_missing_connection_id"
    error.VoiceInvalidUserId -> "voice_invalid_user_id"
    error.VoiceInvalidChannelId -> "voice_invalid_channel_id"
    error.VoiceInvalidState -> "voice_invalid_state"
    error.VoiceUserMismatch -> "voice_user_mismatch"
    error.VoiceTokenFailed -> "voice_token_failed"
    error.VoiceGuildIdMissing -> "voice_guild_id_missing"
    error.VoiceInvalidGuildId -> "voice_invalid_guild_id"
    error.VoiceModeratorMissingConnect -> "voice_moderator_missing_connect"
    error.VoiceUnclaimedAccount -> "voice_unclaimed_account"
    error.VoiceAccountVerificationRequired ->
      "voice_account_verification_required"
    error.VoiceUpdateRateLimited -> "voice_update_rate_limited"
    error.VoiceNonceMismatch -> "voice_nonce_mismatch"
    error.VoicePendingExpired -> "voice_pending_expired"
    error.VoiceCameraUserLimit -> "voice_camera_user_limit"
    error.VoiceE2eeRequired -> "voice_e2ee_required"
    error.VoiceP2pConsentRequired -> "voice_p2p_consent_required"
    error.VoiceP2pUnavailable -> "voice_p2p_unavailable"
    error.DmChannelNotFound -> "dm_channel_not_found"
    error.DmNotRecipient -> "dm_not_recipient"
    error.DmInvalidChannelType -> "dm_invalid_channel_type"
    error.ValidationInvalidSnowflake -> "validation_invalid_snowflake"
    error.ValidationNullSnowflake -> "validation_null_snowflake"
    error.ValidationInvalidSnowflakeList -> "validation_invalid_snowflake_list"
    error.ValidationExpectedList -> "validation_expected_list"
    error.ValidationExpectedMap -> "validation_expected_map"
    error.ValidationMissingField -> "validation_missing_field"
    error.ValidationInvalidParams -> "validation_invalid_params"
    error.InternalError -> "internal_error"
    error.TimeoutError -> "timeout"
    error.UnknownError | error.UnlistedError -> "unknown_error"
  }
}

fn remove_with_recent(
  step: Step,
  ctx: Ctx,
  user: UserId,
  removed: List(#(VoiceKey, Entry)),
) -> Step {
  step
  |> lifecycle.cache_recent(removed, ctx.now_ms)
  |> lifecycle.remove_states(removed)
  |> lifecycle.forget_keys_for(removed)
  |> guild_state.broadcast_disconnects(removed)
  |> lifecycle.cleanup_access(ctx, user)
}

fn disconnect_if_in_channel(
  step: Step,
  ctx: Ctx,
  user: UserId,
  channel: ChannelId,
  conn: Option(ConnectionId),
) -> #(Step, Result(Json, RpcError)) {
  case conn {
    None -> {
      let matching =
        guild_state.user_entries(step.state, user)
        |> list.filter(fn(pair) { voice_key.channel(pair.0) == channel })
      case matching {
        [] -> #(
          step
            |> lifecycle.clear_pending_for_user_channel(ctx, user, channel)
            |> lifecycle.forget_key_if_idle(Some(channel)),
          Ok(ignored()),
        )
        _ -> #(remove_with_recent(step, ctx, user, matching), Ok(success([])))
      }
    }
    Some(conn) ->
      case store.by_connection(step.state.states, conn) {
        Error(Nil) -> {
          let in_channel = case store.key_of(step.state.pending, conn) {
            Ok(key) -> voice_key.channel(key) == channel
            Error(Nil) -> False
          }
          let step = case in_channel {
            True -> lifecycle.clear_pending_connection(step, ctx, conn, user)
            False -> step
          }
          #(step, Ok(ignored()))
        }
        Ok(#(key, value)) ->
          case
            voice_key.user(key) == user && voice_key.channel(key) == channel
          {
            True -> #(
              remove_with_recent(step, ctx, user, [#(key, value)]),
              Ok(success([])),
            )
            False -> #(step, Ok(ignored()))
          }
      }
  }
}

fn confirm(
  step: Step,
  ctx: Ctx,
  conn: Option(ConnectionId),
  nonce: Option(String),
) -> #(Step, Result(Json, RpcError)) {
  case conn {
    None -> #(step, Ok(failure(error.VoiceMissingConnectionId)))
    Some(conn) ->
      case guild_state.pending_of(step.state, conn) {
        Ok(pending) ->
          case ctx.now_ms >= pending.expires_at, nonce == pending.nonce {
            True, _ -> #(step, Ok(failure(error.VoicePendingExpired)))
            False, False -> #(step, Ok(failure(error.VoiceNonceMismatch)))
            False, True -> {
              let value = case guild_state.connected(step.state, conn) {
                Ok(current) ->
                  entry.set_routing(
                    current,
                    entry.region_id(pending.entry),
                    entry.server_id(pending.entry),
                  )
                Error(Nil) -> pending.entry
              }
              case admissible(ctx, step, value) {
                Error(reason) -> #(
                  lifecycle.clear_pending_connection(
                    step,
                    ctx,
                    conn,
                    pending.user,
                  ),
                  Ok(failure(reason)),
                )
                Ok(Nil) ->
                  case pending_limit(ctx, step, pending, value) {
                    Error(reason) -> #(
                      lifecycle.clear_pending_connection(
                        step,
                        ctx,
                        conn,
                        pending.user,
                      ),
                      Ok(failure(reason)),
                    )
                    Ok(Nil) ->
                      case invariant(step, conn, value) {
                        Error(reason) -> #(step, Ok(failure(reason)))
                        Ok(Nil) -> #(
                          connection.activate_pending(
                            step,
                            guild_state.Pending(..pending, entry: value),
                          ),
                          Ok(success([])),
                        )
                      }
                  }
              }
            }
          }
        Error(Nil) ->
          case store.has_connection(step.state.states, conn) {
            True -> #(step, Ok(success([])))
            False -> restore_recent(step, ctx, conn)
          }
      }
  }
}

fn invariant(
  step: Step,
  conn: ConnectionId,
  value: Entry,
) -> Result(Nil, GatewayError) {
  case entry.channel(value) {
    Some(channel) ->
      [
        value,
        ..guild_state.channel_entries(step.state, channel)
        |> list.filter(fn(other) { entry.connection_id(other) != conn })
      ]
      |> p2p.invariant_violation(None)
    None -> Ok(Nil)
  }
}

fn restore_recent(
  step: Step,
  ctx: Ctx,
  conn: ConnectionId,
) -> #(Step, Result(Json, RpcError)) {
  let restore_ms = recent_restore_ms()
  case store.by_connection(step.state.recent, conn) {
    Ok(#(_, recent)) if ctx.now_ms - recent.disconnected_at < restore_ms ->
      case admissible(ctx, step, recent.entry) {
        Error(reason) -> #(
          lifecycle.clear_recent(step, [conn]),
          Ok(failure(reason)),
        )
        Ok(Nil) ->
          case within_limit(ctx, step, recent.entry) {
            Error(reason) -> #(
              lifecycle.clear_recent(step, [conn]),
              Ok(failure(reason)),
            )
            Ok(Nil) -> restore_checked(step, conn, recent)
          }
      }
    _ -> #(step, Ok(failure(error.VoiceConnectionNotFound)))
  }
}

fn admissible(ctx: Ctx, step: Step, value: Entry) -> Result(Nil, GatewayError) {
  case option.then(entry.channel(value), ctx.channel) {
    None -> Error(error.VoiceChannelNotFound)
    Some(_) ->
      case allowed(ctx, step, value) {
        True -> Ok(Nil)
        False -> Error(error.VoicePermissionDenied)
      }
  }
}

fn allowed(ctx: Ctx, step: Step, value: Entry) -> Bool {
  let user = entry.user(value)
  case entry.channel(value), ctx.member(user) {
    Some(channel), Some(_) ->
      permissions.has_view_and_connect(ctx, step.state, user, channel)
    _, _ -> False
  }
}

fn pending_limit(
  ctx: Ctx,
  step: Step,
  pending: guild_state.Pending,
  value: Entry,
) -> Result(Nil, GatewayError) {
  case pending.moved {
    True -> Ok(Nil)
    False -> within_limit(ctx, step, value)
  }
}

fn within_limit(
  ctx: Ctx,
  step: Step,
  value: Entry,
) -> Result(Nil, GatewayError) {
  case entry.channel(value) {
    Some(channel) ->
      permissions.check_user_limit(ctx, step.state, entry.user(value), channel)
    None -> Ok(Nil)
  }
}

fn restore_checked(
  step: Step,
  conn: ConnectionId,
  recent: guild_state.Recent,
) -> #(Step, Result(Json, RpcError)) {
  case invariant(step, conn, recent.entry) {
    Error(reason) -> #(step, Ok(failure(reason)))
    Ok(Nil) -> {
      let step =
        guild_state.map_state(step, fn(state) {
          State(..state, recent: store.delete_connection(state.recent, conn))
          |> guild_state.place(recent.entry, recent.geo)
        })
      #(
        guild_state.broadcast(step, recent.entry, entry.channel(recent.entry)),
        Ok(success([])),
      )
    }
  }
}

fn channel_states(step: Step, channel: ChannelId) -> Json {
  guild_state.channel_entries(step.state, channel)
  |> json.array(fn(value) {
    let base = [
      #("connection_id", voice_key.connection_json(entry.connection_id(value))),
      #("user_id", id.user_json(entry.user(value))),
      #("channel_id", id.channel_json(channel)),
    ]
    let routing =
      [
        #("region_id", entry.region_id(value)),
        #("server_id", entry.server_id(value)),
      ]
      |> list.filter_map(fn(pair) {
        case pair.1 {
          Some(text) -> Ok(#(pair.0, json.string(text)))
          None -> Error(Nil)
        }
      })
    json.object(list.append(base, routing))
  })
}

fn move_member(
  step: Step,
  ctx: Ctx,
  user: UserId,
  moderator: UserId,
  channel: Option(ChannelId),
  conn: Option(ConnectionId),
) -> #(Step, Result(Json, RpcError)) {
  let mine = guild_state.user_entries(step.state, user)
  case mine, channel {
    [], None -> disconnect_moving(step, ctx, user, conn)
    [], Some(_) -> #(step, Ok(failure(error.VoiceUserNotInVoice)))
    _, _ -> {
      let selected = case conn {
        None -> mine
        Some(conn) ->
          list.filter(mine, fn(pair) { voice_key.connection(pair.0) == conn })
      }
      case selected, conn {
        [], None -> #(step, Ok(failure(error.VoiceUserNotInVoice)))
        [], Some(_) -> #(step, Ok(failure(error.VoiceConnectionNotFound)))
        _, _ ->
          case channel {
            None -> {
              let step =
                step
                |> lifecycle.remove_states(selected)
                |> lifecycle.forget_keys_for(selected)
                |> guild_state.broadcast_disconnects(selected)
                |> lifecycle.cleanup_access(ctx, user)
              #(step, Ok(success([])))
            }
            Some(target) ->
              move_to(step, ctx, user, moderator, target, selected)
          }
      }
    }
  }
}

fn disconnect_moving(
  step: Step,
  ctx: Ctx,
  user: UserId,
  conn: Option(ConnectionId),
) -> #(Step, Result(Json, RpcError)) {
  let selected = fn(found: ConnectionId) {
    case conn {
      Some(wanted) -> wanted == found
      None -> True
    }
  }
  let moving = fn(job: guild_state.Job) {
    case job {
      ReissueJob(user: owner, moved: True, base:, ..) ->
        owner == user && selected(entry.connection_id(base))
      _ -> False
    }
  }
  let pending =
    store.of_user(step.state.pending, user)
    |> list.filter(fn(pair) {
      { pair.1 }.moved && selected(voice_key.connection(pair.0))
    })
  let fetching =
    dict.values(step.state.fetches)
    |> list.any(fn(fetch) { moving(fetch.job) })
  case pending == [] && !fetching {
    True -> #(step, Ok(failure(error.VoiceUserNotInVoice)))
    False -> {
      list.each(pending, fn(pair) {
        guild_state.spawn_force_disconnect(step.state.guild, { pair.1 }.entry)
      })
      let step =
        step
        |> region.cancel_reissues(ctx, moving)
        |> list.fold(pending, _, fn(acc, pair) {
          lifecycle.clear_pending_connection(
            acc,
            ctx,
            voice_key.connection(pair.0),
            user,
          )
        })
      #(step, Ok(success([])))
    }
  }
}

fn move_to(
  step: Step,
  ctx: Ctx,
  user: UserId,
  moderator: UserId,
  target: ChannelId,
  selected: List(#(VoiceKey, Entry)),
) -> #(Step, Result(Json, RpcError)) {
  case ctx.channel(target) {
    None -> #(step, Ok(failure(error.VoiceChannelNotFound)))
    Some(channel) ->
      case guild_state.channel_facts(channel).is_voice {
        False -> #(step, Ok(failure(error.VoiceChannelNotVoice)))
        True ->
          case guild_state.can_view_and_connect(ctx, moderator, target) {
            False -> #(step, Ok(failure(error.VoiceModeratorMissingConnect)))
            True ->
              case guild_state.join_decision(step.state, False, target) {
                p2p.Reject(reason) -> #(step, Ok(failure(reason)))
                _ -> #(
                  execute_move(step, ctx, user, target, selected),
                  Ok(success([])),
                )
              }
          }
      }
  }
}

fn execute_move(
  step: Step,
  ctx: Ctx,
  user: UserId,
  target: ChannelId,
  selected: List(#(VoiceKey, Entry)),
) -> Step {
  let located = step.state
  let step =
    step
    |> connection.cancel_moves(
      ctx,
      user,
      lifecycle.connections_of(selected),
      None,
    )
    |> lifecycle.mark_transition(user, target)
    |> lifecycle.remove_states(selected)
    |> lifecycle.forget_keys_for(selected)
  let needs_grant =
    option.is_some(ctx.member(user))
    && !guild_state.can_view_and_connect(ctx, user, target)
  let step = case needs_grant {
    True -> lifecycle.grant_access(step, ctx, user, target)
    False -> step
  }
  let step = lifecycle.cleanup_access(step, ctx, user)
  let step =
    list.fold(selected, step, fn(acc, pair) {
      let #(_, value) = pair
      let moved = entry.set_channel(value, Some(target))
      guild_state.broadcast_move(acc, moved, entry.channel(value))
    })
  list.fold(selected, step, fn(acc, pair) {
    let #(key, value) = pair
    let conn = voice_key.connection(key)
    let perms = guild_state.voice_permissions(ctx, acc.state, user, target)
    region.reissue(
      acc,
      ctx,
      ReissueJob(
        user:,
        channel: target,
        session_id: entry.session_id(value),
        base: moved_base(acc, ctx, user, value),
        geo: guild_state.geo_of(located, conn),
        nonce: token.nonce(),
        moved: True,
        suppress: Some(!perms.can_speak),
      ),
    )
  })
}

fn moved_base(step: Step, ctx: Ctx, user: UserId, value: Entry) -> Entry {
  case ctx.member(user) {
    Some(current) -> {
      let #(mute, deaf) = guild_state.member_mute_deaf(step.state, current)
      value
      |> entry.set_flag(voice_state.Mute, mute)
      |> entry.set_flag(voice_state.Deaf, deaf)
      |> entry.set_member(Some(guild_state.member_raw(step.state, current)))
    }
    None -> value
  }
}
