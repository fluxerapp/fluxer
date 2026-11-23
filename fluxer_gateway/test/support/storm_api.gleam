// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import glisten
import support/isolation

pub type World {
  World(
    users: Int,
    user_base: Int,
    guild_base: Int,
    guilds: Int,
    per_user: Int,
    big: Bool,
  )
}

pub const member_page = 1000

pub fn user_id(world: World, n: Int) -> Int {
  world.user_base + n
}

pub fn token(n: Int) -> String {
  "storm:" <> int.to_string(n)
}

pub fn big_guild(world: World) -> Int {
  world.guild_base + world.guilds
}

pub fn guilds_of(world: World, n: Int) -> List(Int) {
  let small =
    int.range(0, int.min(world.per_user, world.guilds), [], fn(acc, j) {
      [world.guild_base + { n + j } % world.guilds, ..acc]
    })
  case world.big {
    True -> [big_guild(world), ..small]
    False -> small
  }
}

pub fn members_of(world: World, guild: Int) -> Int {
  case guild == big_guild(world) {
    True -> world.users
    False -> list.length(member_page_of(world, guild, -1, world.users))
  }
}

fn residues(world: World, index: Int) -> List(Int) {
  int.range(0, int.min(world.per_user, world.guilds), [], fn(acc, j) {
    [{ index - j + world.guilds } % world.guilds, ..acc]
  })
  |> list.unique
  |> list.sort(int.compare)
}

fn member_page_of(
  world: World,
  guild: Int,
  after: Int,
  limit: Int,
) -> List(Int) {
  case guild == big_guild(world) {
    True -> {
      let from = int.max(after + 1, 0)
      let to = int.min(from + limit, world.users)
      int.range(from, int.max(to, from), [], list.prepend) |> list.reverse
    }
    False -> {
      let index = guild - world.guild_base
      let found = residues(world, index)
      let start_block = int.max(after + 1, 0) / world.guilds
      small_page(world, found, start_block, after, limit, [])
    }
  }
}

fn small_page(
  world: World,
  found: List(Int),
  block: Int,
  after: Int,
  limit: Int,
  acc: List(Int),
) -> List(Int) {
  let base = block * world.guilds
  case limit <= 0 || base >= world.users {
    True -> list.reverse(acc)
    False -> {
      let #(acc, left) =
        list.fold(found, #(acc, limit), fn(state, residue) {
          let #(acc, left) = state
          let n = base + residue
          case left > 0 && n > after && n < world.users {
            True -> #([n, ..acc], left - 1)
            False -> state
          }
        })
      small_page(world, found, block + 1, after, left, acc)
    }
  }
}

@external(erlang, "persistent_term", "put")
fn put_term(key: Atom, value: a) -> Atom

@external(erlang, "persistent_term", "get")
fn get_term(key: Atom) -> a

fn world_key() -> Atom {
  atom.create("fluxer_gateway_storm_api_world")
}

pub fn start(world: World) -> Int {
  put_term(world_key(), world)
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    isolation.keep_across_tests()
    let listener = process.new_name("storm_api_listener")
    let assert Ok(_) =
      glisten.new(fn(_) { #(<<>>, None) }, serve)
      |> glisten.bind("127.0.0.1")
      |> glisten.with_pool_size(64)
      |> glisten.with_listener_name(listener)
      |> glisten.start(0)
    process.send(ready, glisten.get_server_info(listener, 5000).port)
    process.sleep_forever()
  })
  let assert Ok(port) = process.receive(ready, 5000)
  port
}

pub fn endpoint(port: Int) -> String {
  "http://127.0.0.1:" <> int.to_string(port) <> "/internal/rpc"
}

fn serve(
  buffer: BitArray,
  message: glisten.Message(Nil),
  connection: glisten.Connection(Nil),
) -> glisten.Next(BitArray, glisten.Message(Nil)) {
  case message {
    glisten.User(Nil) -> glisten.continue(buffer)
    glisten.Packet(data) -> drain(bit_array.append(buffer, data), connection)
  }
}

fn drain(
  buffer: BitArray,
  connection: glisten.Connection(Nil),
) -> glisten.Next(BitArray, glisten.Message(Nil)) {
  case split_request(buffer) {
    Error(Nil) -> glisten.continue(buffer)
    Ok(#(body, rest)) -> {
      let #(status, text) = answer(get_term(world_key()), body)
      let head =
        "HTTP/1.1 "
        <> int.to_string(status)
        <> " X\r\ncontent-type: application/json\r\ncontent-length: "
        <> int.to_string(string.byte_size(text))
        <> "\r\n\r\n"
      let _ = glisten.send(connection, bytes_tree.from_string(head <> text))
      drain(rest, connection)
    }
  }
}

fn split_request(buffer: BitArray) -> Result(#(BitArray, BitArray), Nil) {
  use head_size <- result.try(head_end(buffer, 0))
  use head <- result.try(
    bit_array.slice(buffer, 0, head_size)
    |> result.try(bit_array.to_string),
  )
  let length = content_length(head)
  let after = bit_array.byte_size(buffer) - head_size - 4
  case after >= length {
    False -> Error(Nil)
    True -> {
      use body <- result.try(bit_array.slice(buffer, head_size + 4, length))
      use rest <- result.map(bit_array.slice(
        buffer,
        head_size + 4 + length,
        after - length,
      ))
      #(body, rest)
    }
  }
}

fn head_end(buffer: BitArray, at: Int) -> Result(Int, Nil) {
  case buffer {
    <<_:bytes-size(at), "\r\n\r\n", _:bits>> -> Ok(at)
    <<_:bytes-size(at), _, _:bits>> -> head_end(buffer, at + 1)
    _ -> Error(Nil)
  }
}

fn content_length(head: String) -> Int {
  head
  |> string.lowercase
  |> string.split("\r\n")
  |> list.find_map(fn(line) {
    case string.split_once(line, ":") {
      Ok(#("content-length", value)) -> int.parse(string.trim(value))
      _ -> Error(Nil)
    }
  })
  |> result.unwrap(0)
}

type Request {
  Request(
    kind: String,
    token: String,
    guild: Int,
    collection: String,
    after: Int,
    limit: Int,
  )
}

fn request_decoder() -> decode.Decoder(Request) {
  let text = fn(key, next) {
    decode.optional_field(key, "", decode.string, next)
  }
  use kind <- decode.then(decode.at(["type"], decode.string))
  use token <- text("token")
  use guild <- text("guild_id")
  use collection <- text("collection")
  use after <- text("after_user_id")
  use limit <- decode.optional_field("limit", member_page, decode.int)
  decode.success(Request(
    kind:,
    token:,
    guild: int.parse(guild) |> result.unwrap(0),
    collection:,
    after: int.parse(after) |> result.unwrap(-1),
    limit:,
  ))
}

fn answer(world: World, body: BitArray) -> #(Int, String) {
  case json.parse_bits(body, request_decoder()) {
    Error(_) -> #(400, "{}")
    Ok(request) ->
      case request.kind {
        "session" -> session(world, request.token)
        "guild_collection" -> collection(world, request)
        _ -> ok(json.object([]))
      }
  }
}

fn ok(data: Json) -> #(Int, String) {
  #(200, json.to_string(json.object([#("data", data)])))
}

fn session(world: World, token: String) -> #(Int, String) {
  case string.split_once(token, ":") {
    Ok(#("storm", digits)) ->
      case int.parse(digits) {
        Ok(n) if n >= 0 && n < world.users ->
          ok(
            json.object([
              #("user", user_json(user_id(world, n))),
              #(
                "guilds",
                json.array(guilds_of(world, n), fn(guild) {
                  json.object([#("id", json.string(int.to_string(guild)))])
                }),
              ),
            ]),
          )
        _ -> #(401, "{\"code\":\"UNAUTHORIZED\"}")
      }
    _ -> #(401, "{\"code\":\"UNAUTHORIZED\"}")
  }
}

fn user_json(user: Int) -> Json {
  json.object([
    #("id", json.string(int.to_string(user))),
    #("username", json.string("s" <> int.to_string(user))),
    #("bot", json.bool(False)),
  ])
}

fn is_guild(world: World, guild: Int) -> Bool {
  guild >= world.guild_base
  && guild < world.guild_base + world.guilds
  || { world.big && guild == big_guild(world) }
}

fn collection(world: World, request: Request) -> #(Int, String) {
  let guild = request.guild
  case is_guild(world, guild) {
    False -> #(404, "{\"code\":\"UNKNOWN_GUILD\"}")
    True ->
      case request.collection {
        "guild" ->
          ok(
            json.object([
              #(
                "guild",
                json.object([
                  #("id", json.string(int.to_string(guild))),
                  #("name", json.string("storm")),
                  #("owner_id", json.string(int.to_string(owner(world, guild)))),
                ]),
              ),
            ]),
          )
        "roles" ->
          ok(
            json.object([
              #(
                "roles",
                json.preprocessed_array([
                  json.object([
                    #("id", json.string(int.to_string(guild))),
                    #("name", json.string("@everyone")),
                    #("permissions", json.string(int.to_string(everyone_bits))),
                    #("position", json.int(0)),
                  ]),
                ]),
              ),
            ]),
          )
        "channels" ->
          ok(
            json.object([
              #(
                "channels",
                json.preprocessed_array([
                  channel(guild, guild + 1),
                  channel(guild, guild + 2),
                ]),
              ),
              #("thread_only_channels", json.preprocessed_array([])),
            ]),
          )
        "emojis" -> ok(json.object([#("emojis", json.preprocessed_array([]))]))
        "stickers" ->
          ok(json.object([#("stickers", json.preprocessed_array([]))]))
        "members" -> members(world, guild, request.after, request.limit)
        _ -> #(404, "{\"code\":\"UNKNOWN\"}")
      }
  }
}

const everyone_bits = 18_014_398_510_599_168

fn owner(world: World, guild: Int) -> Int {
  case member_page_of(world, guild, -1, 1) {
    [first, ..] -> user_id(world, first)
    [] -> world.user_base
  }
}

fn channel(guild: Int, channel_id: Int) -> Json {
  json.object([
    #("id", json.string(int.to_string(channel_id * 1000))),
    #("guild_id", json.string(int.to_string(guild))),
    #("type", json.int(0)),
    #("name", json.string("c")),
    #("position", json.int(0)),
    #("parent_id", json.null()),
    #("permission_overwrites", json.preprocessed_array([])),
  ])
}

fn members(world: World, guild: Int, after: Int, limit: Int) -> #(Int, String) {
  let cursor = case after < 0 {
    True -> -1
    False -> after - world.user_base
  }
  let page = member_page_of(world, guild, cursor, limit + 1)
  let has_more = list.length(page) > limit
  let page = list.take(page, limit)
  let next = case has_more, list.last(page) {
    True, Ok(last) -> json.string(int.to_string(user_id(world, last)))
    _, _ -> json.null()
  }
  ok(
    json.object([
      #(
        "members",
        json.preprocessed_array(
          list.map(page, fn(n) { member_json(user_id(world, n)) }),
        ),
      ),
      #("has_more", json.bool(has_more)),
      #("next_after_user_id", next),
    ]),
  )
}

fn member_json(user: Int) -> Json {
  json.object([
    #("user", user_json(user)),
    #("nick", json.null()),
    #("roles", json.preprocessed_array([])),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}
