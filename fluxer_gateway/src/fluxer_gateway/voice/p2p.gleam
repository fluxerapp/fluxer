// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/entry.{type Entry}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}

const max_participants = 4

pub type Mode {
  Empty
  AllP2p
  Mixed
  AllSfu
}

pub type Role {
  Initiator
  Joiner
}

pub type Decision {
  Sfu
  P2p(role: Role, participants: Int)
  Reject(GatewayError)
}

pub type Grant {
  P2pGrant(connection_id: ConnectionId, ice_servers: List(RawJson), max: Int)
  Declined(GatewayError)
  SfuGrant
}

pub type Admission {
  AdmitP2p(connection_id: ConnectionId, ice_servers: List(RawJson), max: Int)
  AdmitSfu
  Refuse(GatewayError)
}

pub fn agreed(p2p: Option(Bool), bot: Bool) -> Bool {
  case p2p, bot {
    Some(True), False -> True
    _, _ -> False
  }
}

pub fn declined(p2p: Option(Bool)) -> Bool {
  p2p == Some(False)
}

pub fn mode(states: List(Entry)) -> Mode {
  case states {
    [] -> Empty
    _ ->
      case list.all(states, entry.is_p2p), list.any(states, entry.is_p2p) {
        True, _ -> AllP2p
        False, True -> Mixed
        False, False -> AllSfu
      }
  }
}

pub fn join_decision(
  agreed agreed: Bool,
  states states: List(Entry),
  pending pending: Bool,
) -> Decision {
  case mode(states), pending {
    AllP2p, _ ->
      case agreed, list.length(states) + 1 {
        False, _ -> Reject(error.VoiceP2pConsentRequired)
        True, count if count > max_participants -> Reject(error.VoiceChannelFull)
        True, count -> P2p(Joiner, count)
      }
    Empty, False ->
      case agreed {
        True -> P2p(Initiator, 1)
        False -> Sfu
      }
    _, _ -> Sfu
  }
}

pub fn admit(decision: Decision, grant: Grant) -> Admission {
  case decision, grant {
    P2p(..), P2pGrant(connection_id, ice, max) ->
      AdmitP2p(connection_id, ice, max)
    P2p(..), Declined(reason) -> Refuse(reason)
    P2p(Initiator, _), SfuGrant -> AdmitSfu
    P2p(Joiner, _), SfuGrant -> Refuse(error.VoiceP2pUnavailable)
    _, P2pGrant(..) | _, Declined(_) -> Refuse(error.VoiceTokenFailed)
    _, SfuGrant -> AdmitSfu
  }
}

pub fn invariant_violation(
  states: List(Entry),
  max: Option(Int),
) -> Result(Nil, GatewayError) {
  case mode(states) {
    Mixed -> Error(error.VoiceP2pConsentRequired)
    AllP2p ->
      case list.length(states) > participant_cap(max) {
        True -> Error(error.VoiceChannelFull)
        False -> Ok(Nil)
      }
    _ -> Ok(Nil)
  }
}

pub fn participant_cap(max: Option(Int)) -> Int {
  case max {
    Some(value) if value >= 1 && value <= max_participants -> value
    _ -> max_participants
  }
}

pub fn reports_rejection(reason: GatewayError) -> Bool {
  case reason {
    error.VoicePermissionDenied
    | error.VoiceModeratorMissingConnect
    | error.VoiceMemberTimedOut
    | error.VoiceChannelFull
    | error.VoiceConnectionLimitReached
    | error.VoiceUnclaimedAccount
    | error.VoiceAccountVerificationRequired
    | error.VoiceCameraUserLimit
    | error.VoiceE2eeRequired
    | error.VoiceTokenFailed
    | error.VoiceP2pConsentRequired
    | error.VoiceP2pUnavailable -> True
    _ -> False
  }
}

pub fn token_fields(
  decision: Decision,
  country_code: Option(String),
) -> List(#(String, Json)) {
  case decision {
    P2p(role, count) -> {
      let base = [
        #("p2p", json.bool(True)),
        #("p2p_initiator", json.bool(role == Initiator)),
        #("p2p_participant_count", json.int(count)),
      ]
      case country_code {
        Some(code) -> list.append(base, [#("country_code", json.string(code))])
        None -> base
      }
    }
    _ -> []
  }
}

pub fn voice_server_update(
  connection_id: ConnectionId,
  channel: ChannelId,
  guild: Option(GuildId),
  ice_servers: List(RawJson),
) -> RawJson {
  [
    #("p2p", json.bool(True)),
    #("ice_servers", json.array(ice_servers, raw_json.to_json)),
    #("connection_id", voice_key.connection_json(connection_id)),
    #("channel_id", id.channel_json(channel)),
  ]
  |> with_guild(guild)
  |> json.object
  |> raw_json.from_json
}

pub fn session_gone(
  value: Entry,
  sessions_of: fn(UserId) -> List(String),
) -> Bool {
  case entry.is_p2p(value), entry.session_id(value) {
    True, Some(session) if session != "" ->
      !list.contains(sessions_of(entry.user(value)), session)
    _, _ -> False
  }
}

pub fn signal_route(
  states: List(Entry),
  session_id: String,
  to: String,
) -> Result(#(Entry, Entry), Nil) {
  let peers = list.filter(states, entry.is_p2p)
  let sender =
    list.find(peers, fn(peer) { entry.session_id(peer) == Some(session_id) })
  let target =
    list.find(peers, fn(peer) {
      voice_key.connection_text(entry.connection_id(peer)) == to
    })
  case sender, target {
    Ok(sender), Ok(target) if sender != target -> Ok(#(sender, target))
    _, _ -> Error(Nil)
  }
}

pub fn signal_payload(
  guild: Option(GuildId),
  channel: ChannelId,
  sender: Entry,
  data: RawJson,
) -> RawJson {
  [
    #("channel_id", id.channel_json(channel)),
    #("from", voice_key.connection_json(entry.connection_id(sender))),
    #("user_id", id.user_json(entry.user(sender))),
    #("data", raw_json.to_json(data)),
  ]
  |> with_guild(guild)
  |> json.object
  |> raw_json.from_json
}

fn with_guild(
  fields: List(#(String, Json)),
  guild: Option(GuildId),
) -> List(#(String, Json)) {
  case guild {
    Some(guild) ->
      list.append(fields, [
        #("guild_id", id.guild_json(guild)),
      ])
    None -> fields
  }
}
