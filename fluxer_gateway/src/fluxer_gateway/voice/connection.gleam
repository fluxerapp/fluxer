// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId, type VoiceKey}
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/client_update
import fluxer_gateway/voice/e2ee
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/guild_state.{
  type Ctx, type Job, type Pending, type Step, type Update, JoinJob, MoveJob,
  Pending, State,
}
import fluxer_gateway/voice/lifecycle
import fluxer_gateway/voice/metrics as voice_metrics
import fluxer_gateway/voice/p2p
import fluxer_gateway/voice/permissions
import fluxer_gateway/voice/region
import fluxer_gateway/voice/store
import fluxer_gateway/voice/stream_key
import fluxer_gateway/voice/token.{type Outcome, type Token}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

fn pending_ttl_ms() -> Int {
  durations.voice().connection_pending_ttl_ms
}

pub fn update(step: Step, ctx: Ctx, update: Update) -> Step {
  let session = guild_state.session_text(update.session)
  let user = update.session.user
  case update.channel {
    guild_state.Leave ->
      disconnect(step, ctx, update.connection_id, Some(session), user)
    guild_state.Join(channel_id) ->
      case ctx.member(user), ctx.channel(channel_id) {
        None, _ ->
          guild_state.send_error(step, session, error.VoiceMemberNotFound)
        Some(_), None ->
          guild_state.send_error(step, session, error.VoiceChannelNotFound)
        Some(value), Some(channel) ->
          case update.connection_id {
            None -> join(step, ctx, update, value, channel_id, channel)
            Some(conn) ->
              existing(step, ctx, update, value, channel_id, channel, conn)
          }
      }
  }
}

fn disconnect(
  step: Step,
  ctx: Ctx,
  connection_id: Option(ConnectionId),
  session: Option(String),
  user: UserId,
) -> Step {
  case connection_id, session {
    None, None -> step
    None, Some(_) -> disconnect_user(step, ctx, user, session)
    Some(conn), _ ->
      case store.by_connection(step.state.states, conn) {
        Error(Nil) ->
          step
          |> cancel_reissues_of_connection(ctx, user, conn)
          |> lifecycle.clear_pending_connection(ctx, conn, user)
        Ok(#(key, old)) ->
          case voice_key.user(key) == user {
            False -> reject(step, session, error.VoiceUserMismatch)
            True ->
              step
              |> cancel_reissues_of_connection(ctx, user, conn)
              |> remove_one(ctx, key, old)
          }
      }
  }
}

fn cancel_reissues_of_connection(
  step: Step,
  ctx: Ctx,
  user: UserId,
  conn: ConnectionId,
) -> Step {
  region.cancel_reissues(step, ctx, fn(job) {
    case job {
      guild_state.ReissueJob(user: owner, base:, ..) ->
        owner == user && entry.connection_id(base) == conn
      _ -> False
    }
  })
}

fn reject(step: Step, session: Option(String), reason: GatewayError) -> Step {
  case session {
    Some(session) -> guild_state.send_error(step, session, reason)
    None -> step
  }
}

pub fn remove_one(step: Step, ctx: Ctx, key: VoiceKey, old: Entry) -> Step {
  guild_state.spawn_force_disconnect(step.state.guild, old)
  let removed = [#(key, old)]
  step
  |> lifecycle.remove_states(removed)
  |> lifecycle.clear_recent([voice_key.connection(key)])
  |> lifecycle.forget_key_if_idle(entry.channel(old))
  |> guild_state.broadcast_disconnects(removed)
  |> lifecycle.cleanup_after_disconnect(
    ctx,
    entry.user(old),
    entry.channel(old),
  )
}

pub fn disconnect_user(
  step: Step,
  ctx: Ctx,
  user: UserId,
  session: Option(String),
) -> Step {
  let step =
    step
    |> cancel_joins(user, session)
    |> region.cancel_reissues(ctx, fn(job) {
      case job {
        guild_state.ReissueJob(user: owner, session_id:, ..) ->
          owner == user && lifecycle.session_matches(session_id, session)
        _ -> False
      }
    })
  let matching =
    guild_state.user_entries(step.state, user)
    |> list.filter(fn(pair) {
      lifecycle.session_matches(entry.session_id(pair.1), session)
    })
  case matching {
    [] -> lifecycle.clear_pending_for_user(step, ctx, user, session)
    _ -> {
      list.each(matching, fn(pair) {
        guild_state.spawn_force_disconnect(step.state.guild, pair.1)
      })
      step
      |> lifecycle.remove_states(matching)
      |> lifecycle.forget_keys_for(matching)
      |> lifecycle.clear_recent(lifecycle.connections_of(matching))
      |> guild_state.broadcast_disconnects(matching)
      |> lifecycle.clear_pending_for_user(ctx, user, session)
      |> lifecycle.cleanup_if_gone(ctx, user)
    }
  }
}

fn cancel_joins(step: Step, user: UserId, session: Option(String)) -> Step {
  guild_state.map_state(step, fn(state) {
    State(
      ..state,
      fetches: dict.filter(state.fetches, fn(_nonce, fetch) {
        case fetch.job {
          JoinJob(update:, ..) ->
            update.session.user != user
            || !lifecycle.session_matches(
              Some(guild_state.session_text(update.session)),
              session,
            )
          _ -> True
        }
      }),
    )
  })
}

fn resolve_keys(
  step: Step,
  update: Update,
  channel: ChannelId,
  existing: List(String),
) -> Result(List(String), GatewayError) {
  case update.viewer_keys {
    client_update.KeepKeys -> Ok(existing)
    client_update.ClearKeys -> Ok([])
    client_update.BadKeys -> Error(error.VoiceInvalidState)
    client_update.Keys(keys) ->
      stream_key.validate(
        keys,
        Some(step.state.guild),
        channel,
        step.state.states,
      )
  }
}

fn e2ee_check(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
) -> Result(Nil, GatewayError) {
  case e2ee.enabled_for_guild(ctx.facts) {
    False -> Ok(Nil)
    True ->
      e2ee.check_join(
        guild_state.entries(step.state),
        channel,
        capable: update.e2ee_capable,
        bot: update.bot,
      )
  }
}

fn join(
  step: Step,
  ctx: Ctx,
  update: Update,
  value: Member,
  channel_id: ChannelId,
  channel: Channel,
) -> Step {
  let user = update.session.user
  let session = guild_state.session_text(update.session)
  let keys = resolve_keys(step, update, channel_id, [])
  let checks = {
    use Nil <- result.try(permissions.check(
      ctx,
      step.state,
      user,
      channel_id,
      channel,
      False,
    ))
    use Nil <- result.try(e2ee_check(step, ctx, update, channel_id))
    use Nil <- result.try(permissions.check_camera(
      step.state,
      user,
      channel_id,
      update.flags.self_video,
    ))
    keys
  }
  case checks {
    Error(reason) -> guild_state.send_error(step, session, reason)
    Ok(keys) -> {
      let step = lifecycle.clear_transition(step, user, channel_id)
      let decision =
        guild_state.join_decision(
          step.state,
          p2p.agreed(update.p2p, update.bot),
          channel_id,
        )
      case decision {
        p2p.Reject(reason) -> guild_state.send_error(step, session, reason)
        _ -> {
          let perms =
            guild_state.voice_permissions(ctx, step.state, user, channel_id)
          let nonce = token.nonce()
          let job =
            JoinJob(
              update:,
              channel: channel_id,
              member: value,
              keys:,
              perms:,
              nonce:,
              decision:,
              e2ee: update.e2ee_capable && e2ee.enabled_for_guild(ctx.facts),
            )
          request_token(
            step,
            ctx,
            job,
            update,
            channel_id,
            perms,
            nonce,
            decision,
            None,
          )
        }
      }
    }
  }
}

fn request_token(
  step: Step,
  ctx: Ctx,
  job: Job,
  update: Update,
  channel: ChannelId,
  perms: token.Permissions,
  nonce: String,
  decision: p2p.Decision,
  connection_id: Option(ConnectionId),
) -> Step {
  let request =
    token.Request(
      ..token.request(
        guild: Some(step.state.guild),
        channel:,
        user: update.session.user,
        connection_id:,
      ),
      geo: update.geo,
      permissions: Some(perms),
      nonce: Some(nonce),
      decision:,
      country_code: update.country_code,
    )
  guild_state.start_fetch(step, ctx, job, request)
}

pub fn token_ready(
  step: Step,
  ctx: Ctx,
  job: Job,
  result: Result(Outcome, GatewayError),
) -> Step {
  let nonce = guild_state.job_nonce(job)
  case dict.get(step.state.fetches, nonce) {
    Error(Nil) -> step
    Ok(fetch) -> {
      voice_metrics.token_result(
        metrics_job(job),
        is_granted(result),
        ctx.now_ms - fetch.started_at,
      )
      let step =
        guild_state.map_state(step, fn(state) {
          State(..state, fetches: dict.delete(state.fetches, nonce))
        })
      case job {
        JoinJob(..) -> join_ready(step, ctx, job, result)
        MoveJob(..) -> move_ready(step, ctx, job, result)
        guild_state.ReissueJob(..) ->
          region.reissue_ready(step, ctx, job, result)
      }
    }
  }
}

fn is_granted(fetched: Result(Outcome, GatewayError)) -> Bool {
  case fetched {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn metrics_job(job: Job) -> voice_metrics.Job {
  case job {
    JoinJob(..) -> voice_metrics.JoinJob
    MoveJob(..) -> voice_metrics.MoveJob
    guild_state.ReissueJob(..) -> voice_metrics.ReissueJob
  }
}

fn admission(
  decision: p2p.Decision,
  fetched: Result(Outcome, GatewayError),
) -> Result(#(p2p.Admission, Option(Token)), GatewayError) {
  use outcome <- result.try(fetched)
  case outcome {
    token.Granted(granted) ->
      Ok(#(p2p.admit(decision, p2p.SfuGrant), Some(granted)))
    token.GrantedP2p(grant) -> Ok(#(p2p.admit(decision, grant), None))
  }
}

fn checked_admission(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
  decision: p2p.Decision,
  fetched: Result(Outcome, GatewayError),
) -> Result(#(p2p.Admission, Option(Token)), GatewayError) {
  use fresh <- result.try(recheck(step, ctx, update, channel))
  use current <- result.try(case decision, fresh {
    p2p.P2p(..), p2p.Sfu -> Error(error.VoiceP2pUnavailable)
    _, _ -> Ok(fresh)
  })
  admission(current, fetched)
}

fn recheck(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
) -> Result(p2p.Decision, GatewayError) {
  case ctx.channel(channel) {
    None -> Error(error.VoiceChannelNotFound)
    Some(found) -> {
      let user = update.session.user
      use Nil <- result.try(permissions.check(
        ctx,
        step.state,
        user,
        channel,
        found,
        False,
      ))
      let agreed = p2p.agreed(update.p2p, update.bot)
      case guild_state.join_decision(step.state, agreed, channel) {
        p2p.Reject(reason) -> Error(reason)
        fresh -> Ok(fresh)
      }
    }
  }
}

fn build_entry(
  step: Step,
  update: Update,
  channel channel: ChannelId,
  member value: Member,
  keys keys: List(String),
  perms perms: token.Permissions,
  connection_id connection_id: ConnectionId,
  routing routing: Option(Token),
  e2ee_capable e2ee_capable: Bool,
) -> Entry {
  let #(mute, deaf) = guild_state.member_mute_deaf(step.state, value)
  entry.new(
    user: update.session.user,
    guild: Some(step.state.guild),
    channel:,
    connection_id:,
    session_id: Some(guild_state.session_text(update.session)),
    flags: guild_state.apply_permissions(update.flags, perms),
    mute:,
    deaf:,
  )
  |> entry.set_viewer_stream_keys(keys)
  |> entry.set_e2ee_capable(e2ee_capable)
  |> entry.set_routing(
    option.then(routing, fn(t) { t.region_id }),
    option.then(routing, fn(t) { t.server_id }),
  )
  |> entry.set_member(Some(guild_state.member_raw(step.state, value)))
}

fn join_ready(
  step: Step,
  ctx: Ctx,
  job: Job,
  result: Result(Outcome, GatewayError),
) -> Step {
  case job {
    JoinJob(
      update:,
      channel:,
      member: value,
      keys:,
      perms:,
      nonce:,
      decision:,
      e2ee:,
    ) ->
      admit_join(
        step,
        ctx,
        update,
        channel,
        value,
        keys,
        perms,
        nonce,
        decision,
        e2ee,
        result,
      )
    _ -> step
  }
}

fn admit_join(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
  value: Member,
  keys: List(String),
  perms: token.Permissions,
  nonce: String,
  decision: p2p.Decision,
  e2ee: Bool,
  result: Result(Outcome, GatewayError),
) -> Step {
  let session = guild_state.session_text(update.session)
  case checked_admission(step, ctx, update, channel, decision, result) {
    Error(reason) -> guild_state.send_error(step, session, reason)
    Ok(#(p2p.Refuse(reason), _)) ->
      guild_state.send_error(step, session, reason)
    Ok(#(p2p.AdmitP2p(conn, ice, _), _)) -> {
      let created =
        build_entry(
          step,
          update,
          channel:,
          member: value,
          keys:,
          perms:,
          connection_id: conn,
          routing: None,
          e2ee_capable: e2ee,
        )
      activate_p2p(step, created, update.geo, ice)
    }
    Ok(#(p2p.AdmitSfu, None)) ->
      guild_state.send_error(step, session, error.VoiceTokenFailed)
    Ok(#(p2p.AdmitSfu, Some(granted))) -> {
      let created =
        build_entry(
          step,
          update,
          channel:,
          member: value,
          keys:,
          perms:,
          connection_id: granted.connection_id,
          routing: Some(granted),
          e2ee_capable: e2ee,
        )
      sfu_pending(step, ctx, update, channel, created, nonce, granted, e2ee)
    }
  }
}

fn activate_p2p(
  step: Step,
  created: Entry,
  location: Geo,
  ice: List(RawJson),
) -> Step {
  case entry.channel(created) {
    Some(channel) ->
      activate_p2p_in(
        step,
        entry.set_p2p(created, True),
        location,
        channel,
        ice,
      )
    None -> step
  }
}

fn activate_p2p_in(
  step: Step,
  created: Entry,
  location: Geo,
  channel: ChannelId,
  ice: List(RawJson),
) -> Step {
  let conn = entry.connection_id(created)
  let step =
    step
    |> guild_state.map_state(guild_state.place(_, created, location))
    |> lifecycle.clear_transition(entry.user(created), channel)
    |> guild_state.broadcast(created, entry.channel(created))
  case entry.session_id(created) {
    Some(session) ->
      guild_state.emit(
        step,
        guild_state.ToSession(
          session,
          "VOICE_SERVER_UPDATE",
          p2p.voice_server_update(conn, channel, Some(step.state.guild), ice),
        ),
      )
    None -> step
  }
}

fn sfu_pending(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
  created: Entry,
  nonce: String,
  granted: Token,
  e2ee_on: Bool,
) -> Step {
  let session = guild_state.session_text(update.session)
  let pending =
    Pending(
      user: update.session.user,
      channel:,
      session_id: Some(session),
      entry: created,
      geo: update.geo,
      nonce: Some(nonce),
      created_at: ctx.now_ms,
      expires_at: ctx.now_ms + pending_ttl_ms(),
      moved: False,
    )
  case lifecycle.store_pending(step, pending) {
    Error(Nil) ->
      step
      |> lifecycle.abandon_transition(ctx, update.session.user, channel)
      |> guild_state.send_error(session, error.VoiceTokenFailed)
    Ok(step) -> {
      let #(step, key) = region.room_key(step, channel, e2ee_on)
      guild_state.emit(
        step,
        guild_state.ToSession(
          session,
          "VOICE_SERVER_UPDATE",
          region.server_update(step, channel, granted, key),
        ),
      )
    }
  }
}

fn existing(
  step: Step,
  ctx: Ctx,
  update: Update,
  value: Member,
  channel_id: ChannelId,
  channel: Channel,
  conn: ConnectionId,
) -> Step {
  let session = guild_state.session_text(update.session)
  let user = update.session.user
  case guild_state.connected(step.state, conn) {
    Ok(current) ->
      case entry.user(current) == user {
        False -> guild_state.send_error(step, session, error.VoiceUserMismatch)
        True ->
          continue_update(
            step,
            ctx,
            update,
            value,
            channel_id,
            channel,
            conn,
            current,
          )
      }
    Error(Nil) ->
      case restore_pending(step, ctx, conn, channel_id, user) {
        Error(reason) -> guild_state.send_error(step, session, reason)
        Ok(#(step, restored)) ->
          continue_update(
            step,
            ctx,
            update,
            value,
            channel_id,
            channel,
            conn,
            restored,
          )
      }
  }
}

fn restore_pending(
  step: Step,
  ctx: Ctx,
  conn: ConnectionId,
  channel: ChannelId,
  user: UserId,
) -> Result(#(Step, Entry), GatewayError) {
  use pending <- result.try(
    guild_state.pending_of(step.state, conn)
    |> result.replace_error(error.VoiceConnectionNotFound),
  )
  case pending.user == user && pending.channel == channel {
    False -> Error(error.VoiceConnectionNotFound)
    True ->
      case ctx.now_ms >= pending.expires_at {
        True -> Error(error.VoicePendingExpired)
        False -> Ok(#(activate_pending(step, pending), pending.entry))
      }
  }
}

pub fn activate_pending(step: Step, pending: Pending) -> Step {
  step
  |> guild_state.map_state(fn(state) {
    State(
      ..state,
      pending: store.delete_connection(
        state.pending,
        entry.connection_id(pending.entry),
      ),
    )
    |> guild_state.place(pending.entry, pending.geo)
  })
  |> lifecycle.clear_transition(pending.user, pending.channel)
  |> guild_state.broadcast(pending.entry, entry.channel(pending.entry))
}

fn continue_update(
  step: Step,
  ctx: Ctx,
  update: Update,
  value: Member,
  channel_id: ChannelId,
  channel: Channel,
  conn: ConnectionId,
  current: Entry,
) -> Step {
  let session = guild_state.session_text(update.session)
  let user = update.session.user
  let moving = !entry.in_channel(current, channel_id)
  let checks = {
    use Nil <- result.try(case moving {
      True ->
        permissions.check(ctx, step.state, user, channel_id, channel, False)
      False -> Ok(Nil)
    })
    use Nil <- result.try(case moving {
      True -> e2ee_check(step, ctx, update, channel_id)
      False -> Ok(Nil)
    })
    use Nil <- result.try(permissions.check_camera(
      step.state,
      user,
      channel_id,
      update.flags.self_video,
    ))
    resolve_keys(step, update, channel_id, entry.viewer_stream_keys(current))
  }
  case checks {
    Error(reason) -> guild_state.send_error(step, session, reason)
    Ok(keys) ->
      case moving {
        True -> client_move(step, ctx, update, value, channel_id, conn, current)
        False -> {
          let perms =
            guild_state.voice_permissions(ctx, step.state, user, channel_id)
          let step = same_channel(step, update, value, current, keys, perms)
          case p2p.declined(update.p2p) {
            True -> region.switch_to_sfu(step, ctx, channel_id)
            False -> step
          }
        }
      }
  }
}

fn same_channel(
  step: Step,
  update: Update,
  value: Member,
  current: Entry,
  keys: List(String),
  perms: token.Permissions,
) -> Step {
  let #(mute, deaf) = guild_state.member_mute_deaf(step.state, value)
  let flags = guild_state.apply_permissions(update.flags, perms)
  let changed =
    entry.flags(current) != flags
    || entry.has(current, voice_state.Mute) != mute
    || entry.has(current, voice_state.Deaf) != deaf
    || entry.viewer_stream_keys(current) != keys
  case changed {
    False -> step
    True -> {
      let updated =
        current
        |> entry.set_flags(flags)
        |> entry.set_flag(voice_state.Mute, mute)
        |> entry.set_flag(voice_state.Deaf, deaf)
        |> entry.set_viewer_stream_keys(keys)
        |> entry.bump
        |> entry.set_member(Some(guild_state.member_raw(step.state, value)))
      step
      |> guild_state.map_state(guild_state.put_entry(_, updated))
      |> guild_state.broadcast(updated, entry.channel(current))
    }
  }
}

fn client_move(
  step: Step,
  ctx: Ctx,
  update: Update,
  value: Member,
  channel: ChannelId,
  conn: ConnectionId,
  current: Entry,
) -> Step {
  let user = update.session.user
  let session = guild_state.session_text(update.session)
  let update =
    guild_state.Update(
      ..update,
      geo: geo.or(update.geo, guild_state.geo_of(step.state, conn)),
    )
  let step = lifecycle.mark_transition(step, user, channel)
  let decision =
    guild_state.join_decision(
      step.state,
      p2p.agreed(update.p2p, update.bot),
      channel,
    )
  case decision {
    p2p.Reject(reason) ->
      step
      |> lifecycle.abandon_transition(ctx, user, channel)
      |> guild_state.send_error(session, reason)
    _ -> {
      let perms = guild_state.voice_permissions(ctx, step.state, user, channel)
      let nonce = token.nonce()
      let job =
        MoveJob(
          update:,
          channel:,
          member: value,
          old: current,
          perms:,
          nonce:,
          decision:,
          e2ee: update.e2ee_capable && e2ee.enabled_for_guild(ctx.facts),
        )
      step
      |> cancel_moves(ctx, user, [conn], Some(channel))
      |> request_token(
        ctx,
        job,
        update,
        channel,
        perms,
        nonce,
        decision,
        Some(conn),
      )
    }
  }
}

pub fn cancel_moves(
  step: Step,
  ctx: Ctx,
  user: UserId,
  connections: List(ConnectionId),
  keep: Option(ChannelId),
) -> Step {
  let superseded =
    dict.filter(step.state.fetches, fn(_nonce, fetch) {
      case fetch.job {
        MoveJob(old:, ..) ->
          list.contains(connections, entry.connection_id(old))
        _ -> False
      }
    })
  let step =
    guild_state.map_state(step, fn(state) {
      State(..state, fetches: dict.drop(state.fetches, dict.keys(superseded)))
    })
  dict.fold(superseded, step, fn(acc, _nonce, fetch) {
    let #(_, target) = guild_state.job_target(fetch.job)
    case Some(target) == keep {
      True -> acc
      False -> lifecycle.abandon_transition(acc, ctx, user, target)
    }
  })
}

fn move_ready(
  step: Step,
  ctx: Ctx,
  job: Job,
  result: Result(Outcome, GatewayError),
) -> Step {
  case job {
    MoveJob(
      update:,
      channel:,
      member: value,
      old:,
      perms:,
      nonce:,
      decision:,
      e2ee:,
    ) -> {
      let user = update.session.user
      let old_conn = entry.connection_id(old)
      let still_connected =
        store.has_connection(step.state.states, old_conn)
        || store.has_connection(step.state.pending, old_conn)
      case still_connected {
        False -> lifecycle.abandon_transition(step, ctx, user, channel)
        True ->
          move_admitted(
            step,
            ctx,
            update,
            channel,
            value,
            old,
            perms,
            nonce,
            decision,
            e2ee,
            result,
          )
      }
    }
    _ -> step
  }
}

fn move_admitted(
  step: Step,
  ctx: Ctx,
  update: Update,
  channel: ChannelId,
  value: Member,
  old: Entry,
  perms: token.Permissions,
  nonce: String,
  decision: p2p.Decision,
  e2ee: Bool,
  result: Result(Outcome, GatewayError),
) -> Step {
  let user = update.session.user
  let session = guild_state.session_text(update.session)
  case checked_admission(step, ctx, update, channel, decision, result) {
    Error(reason) | Ok(#(p2p.Refuse(reason), _)) ->
      step
      |> lifecycle.abandon_transition(ctx, user, channel)
      |> guild_state.send_error(session, reason)
    Ok(#(p2p.AdmitP2p(conn, ice, _), _)) -> {
      let step = drop_old(step, ctx, old)
      let created =
        build_entry(
          step,
          update,
          channel:,
          member: value,
          keys: [],
          perms:,
          connection_id: conn,
          routing: None,
          e2ee_capable: e2ee,
        )
      activate_p2p(step, created, update.geo, ice)
    }
    Ok(#(p2p.AdmitSfu, None)) ->
      step
      |> lifecycle.abandon_transition(ctx, user, channel)
      |> guild_state.send_error(session, error.VoiceTokenFailed)
    Ok(#(p2p.AdmitSfu, Some(granted))) -> {
      let step = drop_old(step, ctx, old)
      let created =
        build_entry(
          step,
          update,
          channel:,
          member: value,
          keys: [],
          perms:,
          connection_id: granted.connection_id,
          routing: Some(granted),
          e2ee_capable: e2ee,
        )
      sfu_pending(step, ctx, update, channel, created, nonce, granted, e2ee)
    }
  }
}

fn drop_old(step: Step, ctx: Ctx, old: Entry) -> Step {
  let step = case
    store.by_connection(step.state.states, entry.connection_id(old))
  {
    Ok(#(key, current)) -> {
      let removed = [#(key, current)]
      step
      |> lifecycle.remove_states(removed)
      |> lifecycle.forget_key_if_idle(entry.channel(current))
      |> guild_state.broadcast(
        entry.disconnected(current),
        entry.channel(current),
      )
    }
    Error(Nil) -> step
  }
  lifecycle.cleanup_access(step, ctx, entry.user(old))
}

pub fn signal(
  step: Step,
  session: String,
  channel: ChannelId,
  to: String,
  data: RawJson,
) -> Step {
  let route =
    p2p.signal_route(
      guild_state.channel_entries(step.state, channel),
      session,
      to,
    )
  case route {
    Ok(#(sender, target)) ->
      case entry.session_id(target) {
        Some(target_session) ->
          guild_state.emit(
            step,
            guild_state.ToSession(
              target_session,
              "VOICE_SIGNAL",
              p2p.signal_payload(Some(step.state.guild), channel, sender, data),
            ),
          )
        None -> step
      }
    Error(Nil) -> step
  }
}
