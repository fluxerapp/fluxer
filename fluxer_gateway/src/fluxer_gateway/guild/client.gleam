// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{
  type JoinReply, type JoinRequest, JoinOverloaded, JoinUnavailable,
} as core_guild
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import gleam/erlang/process.{type Subject}
import gleam/int

fn overloaded_slot_ms() -> Int {
  durations.guild().client_overloaded_slot_ms
}

fn unavailable_slot_ms() -> Int {
  durations.guild().client_unavailable_slot_ms
}

fn slot(base: Int) -> Int {
  base + int.random(base + 1)
}

pub fn join(
  guild: GuildId,
  request: JoinRequest,
  reply: Subject(JoinReply),
) -> Nil {
  case router.owner(router.GuildKey(guild), node.Guilds) {
    router.OwnerUnavailable(retry) ->
      process.send(reply, JoinUnavailable(retry, request.attempt))
    router.Local ->
      case registry.lookup(guild) {
        Ok(handle) ->
          process.send(handle.ref.inbox, core_guild.Join(request, reply))
        Error(Nil) ->
          case manager.send_local(guild, shard.Join(guild, request, reply)) {
            Ok(Nil) -> Nil
            Error(Nil) ->
              process.send(
                reply,
                JoinUnavailable(slot(unavailable_slot_ms()), request.attempt),
              )
          }
      }
    router.Remote(owner) ->
      case manager.join(owner, guild, request, reply) {
        Ok(Nil) -> Nil
        Error(remote.NoSuspend) ->
          process.send(
            reply,
            JoinOverloaded(slot(overloaded_slot_ms()), request.attempt),
          )
        Error(remote.NoConnect) ->
          process.send(
            reply,
            JoinUnavailable(slot(unavailable_slot_ms()), request.attempt),
          )
      }
  }
}
