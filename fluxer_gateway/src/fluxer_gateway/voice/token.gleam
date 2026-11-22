// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client.{type ApiError} as api
import fluxer_gateway/config/rollout
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type GatewayError}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/p2p.{type Decision, type Grant}
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Permissions {
  Permissions(can_speak: Bool, can_stream: Bool, can_video: Bool)
}

pub type Request {
  Request(
    guild: Option(GuildId),
    channel: ChannelId,
    user: UserId,
    connection_id: Option(ConnectionId),
    geo: Geo,
    permissions: Option(Permissions),
    nonce: Option(String),
    region: Option(String),
    decision: Decision,
    country_code: Option(String),
  )
}

pub type Token {
  Token(
    token: String,
    endpoint: String,
    connection_id: ConnectionId,
    region_id: Option(String),
    server_id: Option(String),
  )
}

pub type Outcome {
  Granted(Token)
  GrantedP2p(Grant)
}

pub fn nonce() -> String {
  crypto.strong_random_bytes(16)
  |> bit_array.base16_encode
  |> string.lowercase
}

pub fn request(
  guild guild: Option(GuildId),
  channel channel: ChannelId,
  user user: UserId,
  connection_id connection_id: Option(ConnectionId),
) -> Request {
  Request(
    guild:,
    channel:,
    user:,
    connection_id:,
    geo: geo.unknown(),
    permissions: None,
    nonce: None,
    region: None,
    decision: p2p.Sfu,
    country_code: None,
  )
}

fn fields(request: Request) -> List(#(String, Json)) {
  let base = [
    #("channel_id", id.channel_json(request.channel)),
    #("user_id", id.user_json(request.user)),
  ]
  list.flatten([
    base,
    optional("guild_id", option.map(request.guild, id.guild_id_to_string)),
    optional(
      "connection_id",
      option.map(request.connection_id, voice_key.connection_text),
    ),
    geolocation(request.geo),
    permission_fields(request.permissions),
    optional("token_nonce", request.nonce),
    optional("rtc_region", request.region),
    p2p.token_fields(request.decision, request.country_code),
  ])
}

fn optional(key: String, value: Option(String)) -> List(#(String, Json)) {
  case value {
    Some(text) -> [#(key, json.string(text))]
    None -> []
  }
}

fn geolocation(location: Geo) -> List(#(String, Json)) {
  case geo.coordinates(location) {
    Some(#(latitude, longitude)) -> [
      #("latitude", json.string(latitude)),
      #("longitude", json.string(longitude)),
    ]
    None -> []
  }
}

fn permission_fields(
  permissions: Option(Permissions),
) -> List(#(String, Json)) {
  case permissions {
    Some(perms) -> [
      #("can_speak", json.bool(perms.can_speak)),
      #("can_stream", json.bool(perms.can_stream)),
      #("can_video", json.bool(perms.can_video)),
    ]
    None -> []
  }
}

fn deadline() -> clock.Deadline {
  clock.deadline_in(rollout.rpc_request_timeout_ms())
}

pub fn api_send(
  kind: String,
  fields: List(#(String, Json)),
) -> Result(Nil, ApiError) {
  api.call(kind, fields, deadline()) |> result.replace(Nil)
}

pub fn api_read(
  kind: String,
  fields: List(#(String, Json)),
  decoder: Decoder(a),
) -> Result(Result(a, Nil), ApiError) {
  use data <- result.map(api.call(kind, fields, deadline()))
  decode.run(data, decoder) |> result.replace_error(Nil)
}

pub fn fetch(request: Request) -> Result(Outcome, GatewayError) {
  case api_read("voice_get_token", fields(request), outcome()) {
    Ok(Ok(outcome)) -> outcome
    Ok(Error(Nil)) -> Error(error.VoiceTokenFailed)
    Error(reason) -> Error(error_for(reason))
  }
}

pub fn fetch_sfu(request: Request) -> Result(Token, GatewayError) {
  use outcome <- result.try(fetch(request))
  case outcome {
    Granted(token) -> Ok(token)
    GrantedP2p(_) -> Error(error.VoiceTokenFailed)
  }
}

fn outcome() -> Decoder(Result(Outcome, GatewayError)) {
  use grant <- decode.then(grant())
  case grant {
    p2p.SfuGrant ->
      decode.one_of(
        decode.map(token_decoder(), fn(token) { Ok(Granted(token)) }),
        [
          decode.success(Error(error.VoiceTokenFailed)),
        ],
      )
    other -> decode.success(Ok(GrantedP2p(other)))
  }
}

fn token_decoder() -> Decoder(Token) {
  use token <- decode.field("token", decode.string)
  use endpoint <- decode.field("endpoint", decode.string)
  use connection_id <- decode.field("connectionId", voice_key.decoder())
  use region_id <- decode.optional_field("regionId", None, optional_text())
  use server_id <- decode.optional_field("serverId", None, optional_text())
  decode.success(Token(
    token:,
    endpoint:,
    connection_id:,
    region_id:,
    server_id:,
  ))
}

fn optional_text() -> Decoder(Option(String)) {
  decode.one_of(
    decode.map(decode.string, fn(text) {
      case text {
        "" -> None
        _ -> Some(text)
      }
    }),
    [
      decode.map(decode.int, fn(number) { Some(int.to_string(number)) }),
      decode.success(None),
    ],
  )
}

fn grant() -> Decoder(Grant) {
  let p2p_decoder = {
    use enabled <- decode.field("p2p", decode.bool)
    use connection_id <- decode.field("connectionId", voice_key.decoder())
    use servers <- decode.field("iceServers", decode.list(ice_server()))
    use max <- decode.optional_field(
      "maxParticipants",
      None,
      decode.optional(decode.int),
    )
    case enabled {
      True ->
        decode.success(p2p.P2pGrant(
          connection_id:,
          ice_servers: result.values(servers),
          max: p2p.participant_cap(max),
        ))
      False -> decode.failure(p2p.SfuGrant, "p2p")
    }
  }
  let declined_decoder = {
    use enabled <- decode.field("p2p", decode.bool)
    use reason <- decode.field("declineReason", decode.string)
    case enabled, reason {
      False, "channel_full" ->
        decode.success(p2p.Declined(error.VoiceChannelFull))
      _, _ -> decode.failure(p2p.SfuGrant, "declined")
    }
  }
  decode.one_of(p2p_decoder, [
    declined_decoder,
    decode.success(p2p.SfuGrant),
  ])
}

fn ice_server() -> Decoder(Result(RawJson, Nil)) {
  decode.optional_field("urls", None, ice_urls(), fn(urls) {
    case urls {
      Some(urls) -> json.object([#("urls", urls)])
      None -> json.object([])
    }
    |> raw_json.from_json
    |> Ok
    |> decode.success
  })
  |> decode.one_of([decode.success(Error(Nil))])
}

fn ice_urls() -> Decoder(Option(Json)) {
  decode.one_of(decode.map(decode.string, fn(url) { Some(json.string(url)) }), [
    decode.map(decode.list(decode.string), fn(urls) {
      Some(json.array(urls, json.string))
    }),
    decode.map(decode.optional(decode.failure(Nil, "Null")), fn(_) {
      Some(json.null())
    }),
    decode.success(None),
  ])
}

fn error_for(reason: ApiError) -> GatewayError {
  case reason {
    api.ApiRejected(_, message) -> account_error(message)
    _ -> error.VoiceTokenFailed
  }
}

pub fn account_error(body: String) -> GatewayError {
  let code_decoder =
    decode.one_of(decode.at(["code"], decode.string), [
      decode.at(["error", "code"], decode.string),
    ])
  case raw_json.read(raw_json.wrap_string(body), code_decoder) {
    Ok("ACCOUNT_VERIFICATION_REQUIRED") ->
      error.VoiceAccountVerificationRequired
    Ok("UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS")
    | Ok("UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS") ->
      error.VoiceUnclaimedAccount
    _ -> error.VoiceTokenFailed
  }
}
