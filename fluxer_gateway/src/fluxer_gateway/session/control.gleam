// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/wire
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/guild.{type GuildRef}
import fluxer_gateway/core/guild_create.{type GuildCreate}
import fluxer_gateway/core/id.{type ChannelId, type GuildId}
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/session.{
  type ResumeError, type SessionInbox, type SessionRef, type SocketRef,
}
import fluxer_gateway/presence/protocol.{type Input}
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/result

pub type Control {
  GuildJoin(guild: GuildId)
  GuildLeave(guild: GuildId)
  GuildUnavailable(guild: GuildId, hidden: Bool)
  VisitStart(guild: GuildId, operator: Bool)
  VisitEnd(guild: GuildId)
  VisitRejected(guild: GuildId)
  VisitPromoted(guild: GuildRef, guild_create: GuildCreate)
  GuildHealth(guild: GuildId, degraded: Bool)
  DmPartnerMutual(guild: GuildId, partners: PackedIds)
  TerminateMatching(auth_session_hashes: List(String), reply: Subject(Bool))
  VoiceRejected(error: GatewayError)
  CallMonitor(channel: ChannelId, call: Pid)
  CallUnmonitor(channel: ChannelId)
  HandOff(target: Node, reply: Subject(Bool))
  PresenceRejoinCheck
  PresenceMoved
  PresenceRebind(to: Subject(Input(Monitor)), stub: Pid)
  GuildRebind(guild: GuildRef, stub: Pid)
  Reattach(socket: SocketRef, seq: Int)
  Relayed(message: SessionInbox)
  RelayedControl(control: Control)
  MoveDone(guilds: List(GuildId), presence: Bool)
  Resume(
    token: String,
    seq: Int,
    socket: SocketRef,
    reply: Subject(Result(SessionRef, ResumeError)),
  )
}

pub fn subject(pid: Pid) -> Subject(Control) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_session_control")),
  )
}

pub fn guild_lost(
  session: SessionRef,
  guild: GuildId,
  retry_after_ms: Int,
) -> Nil {
  case process.subject_owner(session.inbox) {
    Ok(pid) ->
      case wire.pid_speaks(pid, wire.guild_lost_epoch) {
        True -> session.send(session, session.GuildLost(guild, retry_after_ms))
        False -> process.send(subject(pid), GuildUnavailable(guild, False))
      }
    Error(Nil) -> Nil
  }
}

pub fn send(session: SessionRef, control: Control) -> Nil {
  case process.subject_owner(session.inbox) {
    Ok(pid) -> process.send(subject(pid), control)
    Error(Nil) -> Nil
  }
}

pub fn call_monitor(session: SessionRef, channel: ChannelId, call: Pid) -> Nil {
  send(session, CallMonitor(channel, call))
}

pub fn call_unmonitor(session: SessionRef, channel: ChannelId) -> Nil {
  send(session, CallUnmonitor(channel))
}

pub fn hand_off(session: SessionRef, target: Node, timeout_ms: Int) -> Bool {
  case process.subject_owner(session.inbox) {
    Error(Nil) -> False
    Ok(pid) -> {
      let monitor = process.monitor(pid)
      let reply = process.new_subject()
      process.send(subject(pid), HandOff(target, reply))
      let answer =
        process.new_selector()
        |> process.select(reply)
        |> process.select_specific_monitor(monitor, fn(_) { False })
        |> process.selector_receive(timeout_ms)
      process.demonitor_process(monitor)
      result.unwrap(answer, False)
    }
  }
}
