// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/voice_key.{type VoiceKey}
import fluxer_gateway/core/voice_state
import fluxer_gateway/voice/connection
import fluxer_gateway/voice/entry.{type Entry}
import fluxer_gateway/voice/guild_state.{type Ctx, type Step, State}
import fluxer_gateway/voice/permission_sync
import fluxer_gateway/voice/store
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

pub fn update_member_voice(
  step: Step,
  ctx: Ctx,
  user: UserId,
  mute: Bool,
  deaf: Bool,
) -> Result(Step, GatewayError) {
  case ctx.member(user) {
    None -> Error(error.VoiceMemberNotFound)
    Some(_) -> {
      let step =
        guild_state.map_state(step, fn(state) {
          State(
            ..state,
            overrides: dict.insert(
              state.overrides,
              user,
              guild_state.VoiceOverride(mute:, deaf:),
            ),
          )
        })
      let mine = guild_state.user_entries(step.state, user)
      mine
      |> list.map(fn(pair) { voice_key.channel(pair.0) })
      |> list.unique
      |> list.each(fn(channel) { enforce(step, ctx, user, mute, deaf, channel) })
      let updated =
        list.map(mine, fn(pair) {
          #(pair.0, apply_voice(pair.1, mute, deaf) |> entry.bump)
        })
      let step =
        guild_state.map_state(step, fn(state) {
          State(
            ..state,
            states: list.fold(updated, state.states, fn(acc, pair) {
              store.put(acc, pair.0, pair.1)
            }),
            pending: list.fold(
              store.of_user(state.pending, user),
              state.pending,
              fn(acc, pair) {
                store.put(
                  acc,
                  pair.0,
                  guild_state.Pending(
                    ..pair.1,
                    entry: apply_voice({ pair.1 }.entry, mute, deaf),
                  ),
                )
              },
            ),
          )
        })
      Ok(
        list.fold(updated, step, fn(acc, pair) {
          guild_state.broadcast(acc, pair.1, entry.channel(pair.1))
        }),
      )
    }
  }
}

fn apply_voice(value: Entry, mute: Bool, deaf: Bool) -> Entry {
  value
  |> entry.set_member(
    option.map(entry.member(value), fn(raw) {
      entry.patch_member(raw, mute, deaf)
    }),
  )
  |> entry.set_flag(voice_state.Mute, mute)
  |> entry.set_flag(voice_state.Deaf, deaf)
}

fn enforce(
  step: Step,
  ctx: Ctx,
  user: UserId,
  mute: Bool,
  deaf: Bool,
  channel: ChannelId,
) -> Nil {
  permission_sync.enqueue_participant(
    step.state.guild,
    channel,
    user,
    mute,
    deaf,
    guild_state.voice_permissions(ctx, step.state, user, channel),
  )
}

pub fn on_member_change(step: Step, ctx: Ctx, user: UserId) -> Step {
  let step =
    guild_state.map_state(step, fn(state) {
      State(..state, overrides: dict.delete(state.overrides, user))
    })
  case ctx.member(user) {
    None -> connection.disconnect_user(step, ctx, user, None)
    Some(_) -> sync(step, ctx, guild_state.user_entries(step.state, user))
  }
}

pub fn on_permissions_change(step: Step, ctx: Ctx) -> Step {
  sync(step, ctx, store.to_list(step.state.states))
}

pub fn on_channels_change(
  step: Step,
  ctx: Ctx,
  channels: List(ChannelId),
) -> Step {
  channels
  |> list.unique
  |> list.flat_map(store.in_channel(step.state.states, _))
  |> sync(step, ctx, _)
}

pub fn on_users_change(step: Step, ctx: Ctx, users: List(UserId)) -> Step {
  users
  |> list.unique
  |> list.flat_map(guild_state.user_entries(step.state, _))
  |> sync(step, ctx, _)
}

fn sync(step: Step, ctx: Ctx, targets: List(#(VoiceKey, Entry))) -> Step {
  list.fold(targets, step, fn(acc, pair) {
    let #(key, value) = pair
    case store.has_key(acc.state.states, key), entry.channel(value) {
      False, _ | True, None -> acc
      True, Some(channel) -> {
        let user = voice_key.user(key)
        case guild_state.has_base_access(ctx, acc.state, user, channel) {
          False ->
            case entry.is_p2p(value) {
              True -> connection.remove_one(acc, ctx, key, value)
              False -> {
                guild_state.spawn_force_disconnect(acc.state.guild, value)
                acc
              }
            }
          True -> {
            let perms =
              guild_state.voice_permissions(ctx, acc.state, user, channel)
            let streaming =
              entry.has(value, voice_state.SelfStream)
              || entry.has(value, voice_state.SelfVideo)
            let acc = case !perms.can_stream && streaming {
              True -> {
                let cleared =
                  value
                  |> entry.set_flag(voice_state.SelfStream, False)
                  |> entry.set_flag(voice_state.SelfVideo, False)
                acc
                |> guild_state.map_state(fn(state) {
                  State(..state, states: store.put(state.states, key, cleared))
                })
                |> guild_state.broadcast(cleared, Some(channel))
              }
              False -> acc
            }
            permission_sync.enqueue_participant(
              acc.state.guild,
              channel,
              user,
              entry.has(value, voice_state.Mute),
              entry.has(value, voice_state.Deaf),
              perms,
            )
            acc
          }
        }
      }
    }
  })
}
