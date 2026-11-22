// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/error.{type GatewayError, type RpcError}
import fluxer_gateway/core/guild
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/role
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/core/user
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/event
import fluxer_gateway/perms/snapshot
import fluxer_gateway/voice/client_update
import fluxer_gateway/voice/guild_handler.{
  type GuildVoice, type Successor, type VoiceContext, type VoiceEffect,
  type VoiceRequest,
}
import fluxer_gateway/voice/guild_state
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import support/voice_ids

pub const everyone_bits = 3_147_264

pub type VoiceChannel {
  VoiceChannel(
    id: Int,
    user_limit: Int,
    rtc_p2p: Bool,
    denied: List(Int),
    voice: Bool,
  )
}

pub type World {
  World(
    guild: Int,
    features: List(String),
    everyone: Int,
    channels: List(VoiceChannel),
    members: List(Int),
    sessions: Dict(Int, List(String)),
    timeouts: Dict(Int, Int),
  )
}

pub type View {
  View(
    connection_id: String,
    user: String,
    channel: Option(String),
    session_id: Option(String),
    p2p: Bool,
    e2ee_capable: Bool,
    self_video: Bool,
    self_stream: Bool,
    suppress: Bool,
  )
}

pub type Host {
  Host(subject: Subject(Message), pid: process.Pid)
}

pub opaque type Message {
  Voice(VoiceRequest)
  Client(SessionRef, session.ClientOp)
  Run(fn(GuildVoice, VoiceContext) -> #(GuildVoice, List(VoiceEffect)))
  SetWorld(World, reply: Subject(Nil))
  Inspect(reply: Subject(#(GuildVoice, List(VoiceEffect))))
  Freeze(reply: Subject(#(GuildVoice, Successor)))
  Forward
}

type HostState {
  HostState(
    voice: GuildVoice,
    world: World,
    handle: perms.Perms,
    reader: perms.Reader,
    inbox: Subject(VoiceRequest),
    effects: List(VoiceEffect),
    frozen: Option(#(Successor, List(VoiceRequest))),
  )
}

pub fn world() -> World {
  World(
    guild: 900,
    features: [],
    everyone: everyone_bits,
    channels: [
      VoiceChannel(10, 0, False, [], True),
      VoiceChannel(20, 0, False, [], True),
      VoiceChannel(30, 0, True, [], True),
      VoiceChannel(40, 0, False, [], False),
    ],
    members: [1, 2, 3, 4, 5],
    sessions: dict.new(),
    timeouts: dict.new(),
  )
}

pub fn guild_id(world: World) -> GuildId {
  let assert Ok(found) = id.guild_id(world.guild)
  found
}

pub fn user_id(value: Int) -> UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

pub fn channel_id(value: Int) -> ChannelId {
  let assert Ok(found) = id.channel_id(value)
  found
}

pub fn start(world: World) -> Host {
  let assert Ok(Nil) = nif.load()
  let facts =
    guild.facts(
      id: guild_id(world),
      owner: None,
      nsfw_level: guild.NsfwDefault,
      features: world.features,
      member_count: list.length(world.members),
    )
  launch(world, guild_handler.new(facts), False)
}

fn launch(world: World, voice: GuildVoice, resume: Bool) -> Host {
  let assert Ok(started) =
    actor.new_with_initialiser(5000, fn(self) {
      let #(handle, reader) = perms.new(guild_id(world), process.self())
      let assert Ok(_) = perms.load(handle, to_snapshot(world))
      let inbox = process.new_subject()
      let host =
        HostState(
          voice:,
          world:,
          handle:,
          reader:,
          inbox:,
          effects: [],
          frozen: None,
        )
      let host = case resume {
        True ->
          HostState(..host, voice: guild_handler.resume(voice, context(host)))
        False -> host
      }
      actor.initialised(host)
      |> actor.selecting(
        process.new_selector()
        |> process.select(self)
        |> process.select_map(inbox, Voice),
      )
      |> actor.returning(self)
      |> Ok
    })
    |> actor.on_message(handle)
    |> actor.start
  process.unlink(started.pid)
  Host(subject: started.data, pid: started.pid)
}

pub fn stop(host: Host) -> Nil {
  process.kill(host.pid)
}

fn changes(old: World, new: World) -> List(event.Event) {
  let removed_channels =
    list.filter(old.channels, fn(found) {
      !list.any(new.channels, fn(other) { other.id == found.id })
    })
  list.flatten([
    [event.roles([everyone_role(new)])],
    list.map(new.channels, fn(found) {
      event.channel_upsert(make_channel(found), event.Updated)
    }),
    list.map(removed_channels, fn(found) {
      event.channel_delete(channel_id(found.id))
    }),
    old.members
      |> list.filter(fn(found) { !list.contains(new.members, found) })
      |> list.map(fn(found) { event.member_remove(user_id(found)) }),
    new.members
      |> list.filter(fn(found) { !list.contains(old.members, found) })
      |> list.map(fn(found) { event.member_upsert(make_member(found)) }),
  ])
}

fn everyone_role(world: World) -> role.Role {
  let assert Ok(bits) = permissions.from_int(world.everyone)
  role.new(
    id: {
      let assert Ok(found) = id.role_id(world.guild)
      found
    },
    name: "everyone",
    permissions: bits,
    position: role.Position(0),
    hoist: role.NotHoisted,
    raw: raw_json.wrap(<<"{}">>),
  )
}

fn to_snapshot(world: World) -> snapshot.Snapshot {
  let source =
    snapshot.new(guild_id(world))
    |> snapshot.set_roles([everyone_role(world)])
  let source =
    list.fold(world.members, source, fn(acc, value) {
      snapshot.add_member(acc, make_member(value))
    })
  list.fold(world.channels, source, fn(acc, value) {
    snapshot.add_channel(acc, make_channel(value))
  })
}

pub fn make_member(value: Int) -> Member {
  make_member_until(value, None)
}

pub fn make_member_until(value: Int, timed_out_until: Option(Int)) -> Member {
  member.new(
    user: user.new(
      id: user_id(value),
      bot: False,
      flags: 0,
      restricted: False,
      wire: raw_json.wrap(<<"{}">>),
    ),
    nick: None,
    roles: [],
    joined_at_ms: None,
    timed_out_until_ms: timed_out_until,
    wire: raw_json.wrap(<<"{\"nick\":null}">>),
  )
}

pub fn make_channel(value: VoiceChannel) -> Channel {
  let raw =
    json.object([
      #("id", json.string(int.to_string(value.id))),
      #("user_limit", json.int(value.user_limit)),
      #("rtc_p2p", json.bool(value.rtc_p2p)),
    ])
    |> json.to_string
  let overwrites =
    list.map(value.denied, fn(denied) {
      let assert Ok(allow) = permissions.from_int(0)
      let assert Ok(deny) = permissions.from_int(1_048_576 + 1024)
      channel.overwrite(
        target: channel.MemberTarget(user_id(denied)),
        allow:,
        deny:,
      )
    })
  channel.new(
    id: channel_id(value.id),
    kind: case value.voice {
      True -> channel.Voice
      False -> channel.kind_from_int(0)
    },
    parent: None,
    position: 0,
    overwrites:,
    raw: raw_json.wrap(<<raw:utf8>>),
  )
  |> channel.set_voice(channel.voice_limits(
    user_limit: value.user_limit,
    connection_limit: 5,
    rtc_p2p: value.rtc_p2p,
  ))
}

fn context(host: HostState) -> VoiceContext {
  let world = host.world
  let members = world.members
  let channels = list.map(world.channels, make_channel)
  let sessions = world.sessions
  guild_handler.VoiceContext(
    facts: guild.facts(
      id: guild_id(world),
      owner: None,
      nsfw_level: guild.NsfwDefault,
      features: world.features,
      member_count: list.length(members),
    ),
    reader: host.reader,
    member: fn(found) {
      let value = id.user_id_to_int(found)
      case list.contains(members, value) {
        True ->
          Some(make_member_until(
            value,
            dict.get(world.timeouts, value) |> option.from_result,
          ))
        False -> None
      }
    },
    channel: fn(found) {
      list.find(channels, fn(value) { channel.id(value) == found })
      |> option.from_result
    },
    sessions_of: fn(found) {
      dict.get(sessions, id.user_id_to_int(found)) |> result.unwrap([])
    },
    self_voice: host.inbox,
    now_ms: now(),
  )
}

@external(erlang, "os", "system_time")
fn system_time(unit: Millisecond) -> Int

type Millisecond {
  Millisecond
}

fn now() -> Int {
  system_time(Millisecond)
}

fn handle(host: HostState, message: Message) -> actor.Next(HostState, Message) {
  case message {
    Voice(request) ->
      case host.frozen {
        Some(#(successor, buffered)) ->
          case guild_handler.is_read_only(request) {
            True -> actor.continue(step_voice(host, request))
            False ->
              actor.continue(
                HostState(
                  ..host,
                  frozen: Some(#(successor, [request, ..buffered])),
                ),
              )
          }
        None -> actor.continue(step_voice(host, request))
      }
    Client(ref, op) ->
      case guild_handler.decode_op(ref, op) {
        Ok(Some(request)) -> {
          process.send(host.inbox, request)
          actor.continue(host)
        }
        Ok(None) -> actor.continue(host)
        Error(_) -> actor.continue(host)
      }
    Run(body) -> {
      let #(voice, effects) = body(host.voice, context(host))
      actor.continue(
        HostState(..host, voice:, effects: list.append(host.effects, effects)),
      )
    }
    SetWorld(world, reply) -> {
      let assert Ok(_) = perms.apply(host.handle, changes(host.world, world))
      process.send(reply, Nil)
      actor.continue(HostState(..host, world:))
    }
    Inspect(reply) -> {
      process.send(reply, #(host.voice, host.effects))
      actor.continue(host)
    }
    Freeze(reply) -> {
      let #(voice, successor) = guild_handler.handoff(host.voice)
      process.send(reply, #(voice, successor))
      actor.continue(HostState(..host, frozen: Some(#(successor, []))))
    }
    Forward ->
      case host.frozen {
        None -> actor.continue(host)
        Some(#(successor, buffered)) -> {
          let requests =
            buffered
            |> list.reverse
            |> list.filter(guild_handler.follows_handoff)
          case requests {
            [] -> Nil
            _ ->
              case guild_handler.await_successor(successor, 2000) {
                Ok(target) -> list.each(requests, process.send(target, _))
                Error(Nil) -> Nil
              }
          }
          guild_handler.release(host.voice)
          actor.stop()
        }
      }
  }
}

fn step_voice(host: HostState, request: VoiceRequest) -> HostState {
  let #(voice, effects) =
    guild_handler.handle(host.voice, context(host), request)
  HostState(..host, voice:, effects: list.append(host.effects, effects))
}

pub fn client(host: Host, ref: SessionRef, data: Json) -> Nil {
  process.send(
    host.subject,
    Client(ref, session.VoiceStateUpdate(to_dynamic(data))),
  )
}

pub fn signal(host: Host, ref: SessionRef, data: Json) -> Nil {
  process.send(
    host.subject,
    Client(ref, session.VoiceSignal(raw_json.from_json(data))),
  )
}

pub fn client_update(data: Json) -> client_update.Update {
  let assert Ok(update) =
    json.parse(json.to_string(data), client_update.decoder())
  update
}

pub fn to_dynamic(data: Json) -> Dynamic {
  let assert Ok(value) = json.parse(json.to_string(data), decode.dynamic)
  value
}

pub fn rpc(host: Host, call: guild_state.RpcCall) -> Result(Json, RpcError) {
  let reply = process.new_subject()
  process.send(host.subject, Voice(guild_handler.rpc_request(call, reply)))
  let assert Ok(result) = process.receive(reply, 5000)
  result
}

pub fn rpc_text(host: Host, call: guild_state.RpcCall) -> String {
  case rpc(host, call) {
    Ok(value) -> json.to_string(value)
    Error(_) -> "rpc_error"
  }
}

pub fn run(
  host: Host,
  body: fn(GuildVoice, VoiceContext) -> #(GuildVoice, List(VoiceEffect)),
) -> Nil {
  process.send(host.subject, Run(body))
}

pub fn update_world(host: Host, world: World) -> Nil {
  process.call(host.subject, 5000, SetWorld(world, _))
}

pub fn inspect(host: Host) -> #(GuildVoice, List(VoiceEffect)) {
  process.call(host.subject, 5000, Inspect)
}

pub fn effects(host: Host) -> List(VoiceEffect) {
  inspect(host).1
}

pub fn states(host: Host) -> List(View) {
  let voice = inspect(host).0
  guild_handler.voice_states(voice)
  |> list.map(fn(found) {
    let assert Ok(view) =
      json.parse(
        json.to_string(guild_handler.voice_state_json(voice, found)),
        view_decoder(),
      )
    view
  })
}

pub fn view_decoder() -> decode.Decoder(View) {
  use connection_id <- decode.field("connection_id", decode.string)
  use user <- decode.field("user_id", decode.string)
  use channel <- decode.field("channel_id", decode.optional(decode.string))
  use session_id <- decode.field("session_id", decode.optional(decode.string))
  use p2p <- decode.field("p2p", decode.bool)
  use e2ee_capable <- decode.field("e2ee_capable", decode.bool)
  use self_video <- decode.field("self_video", decode.bool)
  use self_stream <- decode.field("self_stream", decode.bool)
  use suppress <- decode.field("suppress", decode.bool)
  decode.success(View(
    connection_id:,
    user:,
    channel:,
    session_id:,
    p2p:,
    e2ee_capable:,
    self_video:,
    self_stream:,
    suppress:,
  ))
}

pub fn freeze(host: Host) -> #(GuildVoice, Successor) {
  process.call(host.subject, 5000, Freeze)
}

pub fn forward(host: Host) -> Nil {
  process.send(host.subject, Forward)
}

pub fn adopt(world: World, voice: GuildVoice) -> Host {
  launch(world, voice, True)
}

pub fn session(user: Int) -> SessionRef {
  session.SessionRef(
    id: session.new_session_id(),
    user: user_id(user),
    inbox: process.new_subject(),
  )
}

pub fn session_text(ref: SessionRef) -> String {
  session.session_id_to_string(ref.id)
}

pub fn sent_to(
  effects: List(VoiceEffect),
  ref: SessionRef,
  event: String,
) -> List(Dynamic) {
  list.filter_map(effects, fn(effect) {
    case effect {
      guild_handler.SendToSession(target, found, payload)
        if target == ref.id && found == event
      ->
        json.parse_bits(raw_json.to_bits(payload), decode.dynamic)
        |> result.replace_error(Nil)
      _ -> Error(Nil)
    }
  })
}

pub fn errors_to(
  effects: List(VoiceEffect),
  ref: SessionRef,
) -> List(GatewayError) {
  list.filter_map(effects, fn(effect) {
    case effect {
      guild_handler.SendError(target, reason) if target == ref.id -> Ok(reason)
      _ -> Error(Nil)
    }
  })
}

pub fn broadcasts(effects: List(VoiceEffect)) -> List(View) {
  list.flat_map(effects, fn(effect) {
    case effect {
      guild_handler.Broadcast("VOICE_STATE_UPDATE", payload, _) -> [payload]
      guild_handler.BroadcastMove("VOICE_STATE_UPDATE", payload, left, _, _) -> [
        left,
        payload,
      ]
      _ -> []
    }
  })
  |> list.filter_map(fn(payload) {
    json.parse_bits(raw_json.to_bits(payload), view_decoder())
    |> result.replace_error(Nil)
  })
}

pub fn key_of(payload: Dynamic) -> Option(String) {
  decode.run(payload, decode.at(["e2ee_key"], decode.string))
  |> option.from_result
}

pub fn text_of(payload: Dynamic, key: String) -> Option(String) {
  decode.run(payload, decode.at([key], decode.string))
  |> option.from_result
}

pub fn join(channel: Int) -> Json {
  json.object([#("channel_id", json.string(int.to_string(channel)))])
}

pub fn join_with(channel: Int, extra: List(#(String, Json))) -> Json {
  json.object([#("channel_id", json.string(int.to_string(channel))), ..extra])
}

pub fn update(
  channel: Option(Int),
  connection_id: Option(String),
  extra: List(#(String, Json)),
) -> Json {
  json.object([
    #(
      "channel_id",
      json.nullable(option.map(channel, int.to_string), json.string),
    ),
    #("connection_id", json.nullable(connection_id, json.string)),
    ..extra
  ])
}

pub fn confirm(
  host: Host,
  connection_id: String,
  nonce: Option(String),
) -> String {
  rpc_text(
    host,
    guild_state.Confirm(Some(voice_ids.conn(connection_id)), nonce),
  )
}

pub fn nonce_of(calls: List(Dynamic)) -> Option(String) {
  list.find_map(calls, fn(body) {
    decode.run(body, decode.at(["token_nonce"], decode.string))
  })
  |> option.from_result
}
