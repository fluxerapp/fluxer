// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/inbox.{type Control, type Handle}
import fluxer_gateway/guild/server
import fluxer_gateway/nif
import fluxer_gateway/util/decode as id_decode
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string

pub type Guild {
  Guild(id: GuildId, handle: Handle)
}

const barrier_ms = 60_000

fn await_guild(guild: Guild, reply: Subject(a)) -> Result(a, Nil) {
  let monitor = process.monitor(guild.handle.pid)
  let found =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(barrier_ms)
  process.demonitor_process(monitor)
  case found {
    Ok(Ok(value)) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn ask_guild(
  guild: Guild,
  query: inbox.Query,
) -> Result(inbox.QueryReply, Nil) {
  let reply = process.new_subject()
  process.send(
    guild.handle.control,
    inbox.Ask(query, clock.monotonic_ms() + barrier_ms, reply),
  )
  await_guild(guild, reply)
}

pub type Client {
  Client(ref: SessionRef, inbox: Subject(SessionInbox))
}

pub type Frame {
  Frame(event: String, payload: Dynamic, text: String)
}

pub fn parse(text: String) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<text:utf8>>)
  value
}

pub fn guild_id(value: Int) -> GuildId {
  let assert Ok(found) = id.guild_id(value)
  found
}

pub fn user_id(value: Int) -> UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

pub fn role_json(role: Int, bits: Int, position: Int) -> String {
  "{\"id\":\""
  <> int.to_string(role)
  <> "\",\"name\":\"r"
  <> int.to_string(role)
  <> "\",\"permissions\":\""
  <> int.to_string(bits)
  <> "\",\"position\":"
  <> int.to_string(position)
  <> "}"
}

pub fn hoisted_role_json(role: Int, bits: Int, position: Int) -> String {
  "{\"id\":\""
  <> int.to_string(role)
  <> "\",\"name\":\"r"
  <> int.to_string(role)
  <> "\",\"permissions\":\""
  <> int.to_string(bits)
  <> "\",\"position\":"
  <> int.to_string(position)
  <> ",\"hoist\":true}"
}

pub fn channel_json(channel: Int, kind: Int, overwrites: String) -> String {
  "{\"id\":\""
  <> int.to_string(channel)
  <> "\",\"type\":"
  <> int.to_string(kind)
  <> ",\"name\":\"c"
  <> int.to_string(channel)
  <> "\",\"position\":0,\"permission_overwrites\":"
  <> overwrites
  <> "}"
}

pub fn overwrite_json(target: Int, kind: Int, allow: Int, deny: Int) -> String {
  "{\"id\":\""
  <> int.to_string(target)
  <> "\",\"type\":"
  <> int.to_string(kind)
  <> ",\"allow\":\""
  <> int.to_string(allow)
  <> "\",\"deny\":\""
  <> int.to_string(deny)
  <> "\"}"
}

pub fn member_json(user: Int, name: String, roles: List(Int)) -> String {
  "{\"user\":{\"id\":\""
  <> int.to_string(user)
  <> "\",\"username\":\""
  <> name
  <> "\"},\"roles\":["
  <> string.join(
    list.map(roles, fn(role) { "\"" <> int.to_string(role) <> "\"" }),
    ",",
  )
  <> "],\"joined_at\":\"2026-01-01T00:00:00Z\"}"
}

pub fn loaded(
  guild: Int,
  owner: Int,
  roles: List(String),
  channels: List(String),
  members: List(String),
) -> data.Loaded {
  let collected =
    dict.from_list([
      #(
        "guild",
        whole(
          "{\"id\":\""
          <> int.to_string(guild)
          <> "\",\"name\":\"g\",\"owner_id\":\""
          <> int.to_string(owner)
          <> "\"}",
        ),
      ),
      #("roles", whole("[" <> string.join(roles, ",") <> "]")),
      #("channels", whole("[" <> string.join(channels, ",") <> "]")),
      #("members", whole("[" <> string.join(members, ",") <> "]")),
    ])
  let assert Ok(found) = data_decode.collections(guild_id(guild), collected)
  found
}

pub fn start(
  guild: Int,
  owner: Int,
  roles: List(String),
  channels: List(String),
  members: List(String),
) -> Guild {
  let assert Ok(Nil) = nif.load()
  let found = guild_id(guild)
  let assert Ok(started) =
    server.start(server.StartArgs(
      guild: found,
      loaded: loaded(guild, owner, roles, channels, members),
      threads: None,
      voice: None,
      rejoining: [],
      arriving: None,
    ))
  Guild(id: found, handle: started.data)
}

pub fn reload(guild: Guild, found: data.Loaded) -> Nil {
  let reply = process.new_subject()
  process.send(guild.handle.control, inbox.Reload(found, reply))
  let assert Ok(Nil) = await_guild(guild, reply)
  Nil
}

pub fn dispatch_many(guild: Guild, events: List(#(String, String))) -> Nil {
  process.send(
    guild.handle.control,
    inbox.DispatchMany(
      list.map(events, fn(event) { #(event.0, parse(event.1)) }),
    ),
  )
}

pub fn stop(guild: Guild) -> Nil {
  let reply = process.new_subject()
  process.send(guild.handle.control, inbox.Shutdown(reply))
  let _ = process.receive(reply, 2000)
  Nil
}

pub fn join(guild: Guild, user: Int) -> Client {
  let inbox = process.new_subject()
  let ref =
    session.SessionRef(
      id: session.new_session_id(),
      user: user_id(user),
      inbox:,
    )
  let reply = process.new_subject()
  let request =
    core_guild.JoinRequest(
      session: ref,
      flags: session.no_flags(),
      class: core_guild.Focused,
      mode: core_guild.AsMember,
      presence: online(user),
      bot: False,
      staff: False,
      remaining_ms: 10_000,
      attempt: 1,
    )
  core_guild.send(guild.handle.ref, core_guild.Join(request, reply))
  let assert Ok(core_guild.Joined(..)) = process.receive(reply, 5000)
  let client = Client(ref:, inbox:)
  let _ = drain(client)
  client
}

pub fn online(user: Int) -> presence.Presence {
  status(user, presence.Online)
}

pub fn status(user: Int, found: presence.Status) -> presence.Presence {
  let wire =
    "{\"user\":{\"id\":\""
    <> int.to_string(user)
    <> "\"},\"status\":\""
    <> presence.status_to_wire(found)
    <> "\",\"mobile\":false,\"afk\":false}"
  presence.new(
    user: user_id(user),
    status: found,
    mobile: False,
    afk: False,
    custom_status: None,
    wire: raw_json.wrap(<<wire:utf8>>),
  )
}

pub fn set_presence(guild: Guild, found: presence.Presence) -> Nil {
  core_guild.send(guild.handle.ref, core_guild.MemberPresence(found))
}

pub fn lazy(guild: Guild, client: Client, request: String) -> Nil {
  core_guild.send(
    guild.handle.ref,
    core_guild.SessionOp(client.ref, session.LazyRequest(parse(request))),
  )
}

pub fn subscribe(
  guild: Guild,
  client: Client,
  channel: Int,
  ranges: List(#(Int, Int)),
) -> Nil {
  let ranges =
    ranges
    |> list.map(fn(range) {
      "[" <> int.to_string(range.0) <> "," <> int.to_string(range.1) <> "]"
    })
    |> string.join(",")
  lazy(
    guild,
    client,
    "{\"member_list_channels\":{\""
      <> int.to_string(channel)
      <> "\":["
      <> ranges
      <> "]}}",
  )
}

pub fn dispatch(guild: Guild, event: String, data: String) -> Nil {
  process.send(guild.handle.control, inbox.Dispatch(event, parse(data)))
}

pub fn control(guild: Guild) -> Subject(Control) {
  guild.handle.control
}

pub fn sync(guild: Guild) -> Nil {
  let assert Ok(_) = ask_guild(guild, inbox.ChannelCount)
  Nil
}

pub fn drain(client: Client) -> List(Frame) {
  collect(client, 50, [])
}

pub fn settle(guild: Guild, client: Client) -> List(Frame) {
  sync(guild)
  drain(client)
}

fn collect(client: Client, timeout: Int, acc: List(Frame)) -> List(Frame) {
  case process.receive(client.inbox, timeout) {
    Ok(session.GuildDispatch(_, event, payload, _)) ->
      collect(client, timeout, [frame(event, payload), ..acc])
    Ok(session.Dispatch(event, payload)) ->
      collect(client, timeout, [frame(event, payload), ..acc])
    Ok(_) -> collect(client, timeout, acc)
    Error(Nil) -> list.reverse(acc)
  }
}

fn frame(event: String, payload: raw_json.RawJson) -> Frame {
  let bits = raw_json.to_bits(payload)
  let assert Ok(text) = bit_array.to_string(bits)
  Frame(event:, payload: parse(text), text:)
}

pub fn named(frames: List(Frame), event: String) -> List(Frame) {
  list.filter(frames, fn(found) { found.event == event })
}

pub fn list_updates(frames: List(Frame)) -> List(Frame) {
  named(frames, "GUILD_MEMBER_LIST_UPDATE")
}

pub type Op {
  Op(op: String, range: List(Int), index: Int, items: List(Dynamic))
}

pub fn ops(found: Frame) -> List(Op) {
  let op_decoder = {
    use op <- decode.field("op", decode.string)
    use range <- decode.optional_field("range", [], decode.list(decode.int))
    use index <- decode.optional_field("index", -1, decode.int)
    use items <- decode.optional_field("items", [], decode.list(decode.dynamic))
    use item <- decode.optional_field(
      "item",
      [],
      decode.map(decode.dynamic, fn(one) { [one] }),
    )
    decode.success(Op(op:, range:, index:, items: list.append(items, item)))
  }
  let assert Ok(found) =
    decode.run(found.payload, decode.at(["ops"], decode.list(op_decoder)))
  found
}

pub fn list_counts(found: Frame) -> #(Int, Int) {
  let assert Ok(member_count) =
    decode.run(found.payload, decode.at(["member_count"], decode.int))
  let assert Ok(online_count) =
    decode.run(found.payload, decode.at(["online_count"], decode.int))
  #(member_count, online_count)
}

pub fn item_user_ids(items: List(Dynamic)) -> List(Int) {
  list.filter_map(items, fn(item) {
    decode.run(
      item,
      decode.at(["member", "user", "id"], decode.string)
        |> decode.map(int.parse),
    )
    |> fn(found) {
      case found {
        Ok(Ok(value)) -> Ok(value)
        _ -> Error(Nil)
      }
    }
  })
}

pub fn group_ids(items: List(Dynamic)) -> List(String) {
  list.filter_map(items, fn(item) {
    decode.run(item, decode.at(["group", "id"], decode.string))
    |> fn(found) {
      case found {
        Ok(value) -> Ok(value)
        Error(_) -> Error(Nil)
      }
    }
  })
}

pub fn channel_ids(frames: List(Frame), event: String) -> List(String) {
  frames
  |> named(event)
  |> list.filter_map(fn(found) {
    decode.run(found.payload, decode.at(["id"], decode.string))
    |> fn(result) {
      case result {
        Ok(value) -> Ok(value)
        Error(_) -> Error(Nil)
      }
    }
  })
}

pub fn ask(guild: Guild, query: inbox.Query) -> inbox.QueryReply {
  let assert Ok(found) = ask_guild(guild, query)
  found
}

pub fn json_text(found: json.Json) -> String {
  json.to_string(found)
}

pub fn last_list(frames: List(Frame), previous: Frame) -> Frame {
  case list.last(list_updates(frames)) {
    Ok(found) -> found
    Error(Nil) -> previous
  }
}

pub fn fresh_view(
  guild: Guild,
  user: Int,
  channel: Int,
  ranges: List(#(Int, Int)),
) -> Frame {
  let client = join(guild, user)
  subscribe(guild, client, channel, ranges)
  let assert Ok(found) = list.last(list_updates(settle(guild, client)))
  leave(guild, client)
  found
}

pub fn leave(guild: Guild, client: Client) -> Nil {
  core_guild.send(
    guild.handle.ref,
    core_guild.Leave(client.ref.id, process.self()),
  )
  sync(guild)
}

pub fn rows(found: Frame) -> List(String) {
  found
  |> ops
  |> list.flat_map(fn(op) { op.items })
  |> list.map(fn(item) {
    case group_ids([item]), item_user_ids([item]) {
      [group], _ -> "g:" <> group
      _, [user] -> int.to_string(user)
      _, _ -> "?"
    }
  })
}

pub fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
