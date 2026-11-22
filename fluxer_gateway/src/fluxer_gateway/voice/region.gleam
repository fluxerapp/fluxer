// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_state.{
  type Ctx, type Job, type Step, Pending, ReissueJob, State,
}
import fluxer_gateway/voice/lifecycle
import fluxer_gateway/voice/store
import fluxer_gateway/voice/token.{type Outcome, type Token}
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}

fn pending_ttl_ms() -> Int {
  durations.voice().region_pending_ttl_ms
}

fn convert_to_sfu(step: Step, ctx: Ctx, channel: ChannelId) -> #(Step, Bool) {
  let converted =
    store.in_channel(step.state.states, channel)
    |> list.filter(fn(pair) { entry.is_p2p(pair.1) })
    |> list.map(fn(pair) { #(pair.0, entry.to_sfu(pair.1)) })
  case guild_state.pinned(ctx, channel) || converted == [] {
    True -> #(step, False)
    False -> {
      let step =
        guild_state.map_state(step, fn(state) {
          State(
            ..state,
            states: list.fold(converted, state.states, fn(acc, pair) {
              store.put(acc, pair.0, pair.1)
            }),
          )
        })
      let step =
        list.fold(converted, step, fn(acc, pair) {
          guild_state.broadcast(acc, pair.1, entry.channel(pair.1))
        })
      #(step, True)
    }
  }
}

pub fn switch_to_sfu(step: Step, ctx: Ctx, channel: ChannelId) -> Step {
  case convert_to_sfu(step, ctx, channel) {
    #(step, True) -> reissue_channel(step, ctx, channel)
    #(step, False) -> step
  }
}

pub fn switch_region(
  step: Step,
  ctx: Ctx,
  channel: ChannelId,
) -> Result(Step, GatewayError) {
  case ctx.channel(channel) {
    None -> Error(error.VoiceChannelNotFound)
    Some(value) ->
      case guild_state.channel_facts(value).is_voice {
        False -> Error(error.VoiceChannelNotVoice)
        True -> {
          let #(step, _) = convert_to_sfu(step, ctx, channel)
          Ok(reissue_channel(step, ctx, channel))
        }
      }
  }
}

fn reissue_channel(step: Step, ctx: Ctx, channel: ChannelId) -> Step {
  guild_state.channel_entries(step.state, channel)
  |> list.filter(fn(value) { !entry.is_p2p(value) })
  |> list.fold(step, fn(acc, value) {
    case entry.session_id(value) {
      None -> acc
      Some(_) ->
        reissue(
          acc,
          ctx,
          ReissueJob(
            user: entry.user(value),
            channel:,
            session_id: entry.session_id(value),
            base: value,
            geo: guild_state.geo_of(acc.state, entry.connection_id(value)),
            nonce: token.nonce(),
            moved: False,
            suppress: None,
          ),
        )
    }
  })
}

pub fn reissue(step: Step, ctx: Ctx, job: Job) -> Step {
  case job {
    ReissueJob(user:, channel:, base:, geo:, nonce:, ..) -> {
      let request =
        token.Request(
          ..token.request(
            guild: Some(step.state.guild),
            channel:,
            user:,
            connection_id: Some(entry.connection_id(base)),
          ),
          geo:,
          permissions: Some(guild_state.voice_permissions(
            ctx,
            step.state,
            user,
            channel,
          )),
          nonce: Some(nonce),
        )
      guild_state.start_fetch(step, ctx, job, request)
    }
    _ -> step
  }
}

pub fn cancel_reissues(step: Step, ctx: Ctx, matches: fn(Job) -> Bool) -> Step {
  let cancelled =
    dict.filter(step.state.fetches, fn(_nonce, fetch) {
      case fetch.job {
        ReissueJob(..) -> matches(fetch.job)
        _ -> False
      }
    })
  let step =
    guild_state.map_state(step, fn(state) {
      State(..state, fetches: dict.drop(state.fetches, dict.keys(cancelled)))
    })
  dict.fold(cancelled, step, fn(acc, _nonce, fetch) {
    abandon_reissue(acc, ctx, fetch.job)
  })
}

pub fn reissue_ready(
  step: Step,
  ctx: Ctx,
  job: Job,
  result: Result(Outcome, GatewayError),
) -> Step {
  let live = case job {
    ReissueJob(moved: True, ..) -> True
    ReissueJob(user:, channel:, base:, ..) ->
      still_connected(step, user, channel, base)
    _ -> False
  }
  case job, result, ctx.channel(guild_state.job_target(job).1) {
    ReissueJob(
      user:,
      channel:,
      session_id:,
      base:,
      geo:,
      nonce:,
      moved:,
      suppress:,
    ),
      Ok(token.Granted(granted)),
      Some(_)
      if live
    -> {
      let flags = entry.flags(base)
      let flags = entry.Flags(..flags, suppress: option.unwrap(suppress, False))
      let #(mute, deaf, member_raw) = case ctx.member(user) {
        Some(current) -> {
          let #(mute, deaf) = guild_state.member_mute_deaf(step.state, current)
          #(mute, deaf, Some(guild_state.member_raw(step.state, current)))
        }
        None -> #(
          entry.has(base, voice_state.Mute),
          entry.has(base, voice_state.Deaf),
          entry.member(base),
        )
      }
      let fresh =
        entry.new(
          user:,
          guild: Some(step.state.guild),
          channel:,
          connection_id: granted.connection_id,
          session_id:,
          flags:,
          mute:,
          deaf:,
        )
        |> entry.set_e2ee_capable(entry.e2ee_capable(base))
        |> entry.set_member(member_raw)
        |> entry.set_routing(granted.region_id, granted.server_id)
      let pending =
        Pending(
          user:,
          channel:,
          session_id:,
          entry: fresh,
          geo:,
          nonce: Some(nonce),
          created_at: ctx.now_ms,
          expires_at: ctx.now_ms + pending_ttl_ms(),
          moved:,
        )
      case lifecycle.store_pending(step, pending) {
        Error(Nil) -> abandon_reissue(step, ctx, job)
        Ok(step) ->
          case session_id {
            Some(session) -> {
              let #(step, key) =
                room_key(
                  step,
                  channel,
                  entry.e2ee_capable(fresh) && e2ee.enabled_for_guild(ctx.facts),
                )
              guild_state.emit(
                step,
                guild_state.ToSession(
                  session,
                  "VOICE_SERVER_UPDATE",
                  server_update(step, channel, granted, key),
                ),
              )
            }
            None -> step
          }
      }
    }
    ReissueJob(..), _, _ -> abandon_reissue(step, ctx, job)
    _, _, _ -> step
  }
}

fn still_connected(
  step: Step,
  user: id.UserId,
  channel: ChannelId,
  base: Entry,
) -> Bool {
  case store.key_of(step.state.states, entry.connection_id(base)) {
    Ok(key) -> voice_key.user(key) == user && voice_key.channel(key) == channel
    Error(Nil) -> False
  }
}

pub fn room_key(
  step: Step,
  channel: ChannelId,
  wanted: Bool,
) -> #(Step, Option(String)) {
  case wanted {
    False -> #(step, None)
    True -> {
      let #(key, keys) = e2ee.room_key(step.state.room_keys, channel)
      #(
        guild_state.map_state(step, fn(state) {
          State(..state, room_keys: keys)
        }),
        Some(key),
      )
    }
  }
}

pub fn server_update(
  step: Step,
  channel: ChannelId,
  granted: Token,
  key: Option(String),
) -> RawJson {
  let base = [
    #("token", json.string(granted.token)),
    #("endpoint", json.string(granted.endpoint)),
    #("guild_id", id.guild_json(step.state.guild)),
    #("channel_id", id.channel_json(channel)),
    #("connection_id", voice_key.connection_json(granted.connection_id)),
  ]
  let fields = case key {
    Some(key) -> list.append(base, [#("e2ee_key", json.string(key))])
    None -> base
  }
  json.object(fields) |> raw_json.from_json
}

pub fn abandon_reissue(step: Step, ctx: Ctx, job: Job) -> Step {
  case job {
    ReissueJob(user:, channel:, base:, moved: True, ..) -> {
      let conn = entry.connection_id(base)
      let step = case store.has_connection(step.state.states, conn) {
        True -> step
        False -> {
          guild_state.spawn_force_disconnect(step.state.guild, base)
          guild_state.broadcast(
            step,
            entry.disconnected(entry.set_channel(base, Some(channel))),
            Some(channel),
          )
        }
      }
      step
      |> lifecycle.forget_key_if_idle(Some(channel))
      |> lifecycle.abandon_transition(ctx, user, channel)
    }
    ReissueJob(user:, channel:, ..) ->
      lifecycle.abandon_transition(step, ctx, user, channel)
    _ -> step
  }
}
