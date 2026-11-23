// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/telemetry/process_registry
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

const default_limit = 20

const max_limit = 100

const max_state_fetch = 200

pub type GuildDescription {
  GuildDescription(
    name: String,
    icon: Option(String),
    member_count: Int,
    session_count: Int,
    presence_count: Int,
  )
}

pub type GuildMemory {
  GuildMemory(
    guild_id: Option(String),
    name: String,
    icon: Option(String),
    memory: Int,
    member_count: Int,
    session_count: Int,
    presence_count: Int,
  )
}

type Describer =
  fn(Pid) -> Result(GuildDescription, Nil)

@external(erlang, "persistent_term", "get")
fn stored_describer(key: Atom, default: Option(Describer)) -> Option(Describer)

type Stored

@external(erlang, "persistent_term", "put")
fn store_describer(key: Atom, value: Option(Describer)) -> Stored

fn describer_key() -> Atom {
  atom.create("fluxer_gateway_guild_memory_describer")
}

pub fn register_describer(describe: Describer) -> Nil {
  store_describer(describer_key(), Some(describe))
  Nil
}

fn clamp_limit(limit: Int) -> Int {
  case limit {
    _ if limit > max_limit -> max_limit
    _ if limit > 0 -> limit
    _ -> default_limit
  }
}

fn state_fetch_limit(limit: Int) -> Int {
  let limit = clamp_limit(limit)
  case limit > 50 {
    True -> int.min(max_state_fetch, limit + 50)
    False -> int.min(max_state_fetch, limit + limit)
  }
}

pub fn local(limit: Int) -> List(GuildMemory) {
  let limit = clamp_limit(limit)
  let describe = stored_describer(describer_key(), None)
  process_registry.entries(process_registry.guilds)
  |> list.filter_map(fn(entry) {
    let #(guild, pid) = entry
    process_info.memory(pid)
    |> result.map(fn(memory) { #(guild, pid, memory) })
  })
  |> list.sort(fn(a, b) { int.compare(b.2, a.2) })
  |> list.take(state_fetch_limit(limit))
  |> list.filter_map(fn(candidate) {
    let #(guild, pid, memory) = candidate
    describe_one(describe, id.write(guild), pid, memory)
  })
  |> list.sort(fn(a, b) { int.compare(b.memory, a.memory) })
  |> list.take(limit)
}

fn describe_one(
  describe: Option(Describer),
  id: String,
  pid: Pid,
  memory: Int,
) -> Result(GuildMemory, Nil) {
  case describe {
    None -> Ok(GuildMemory(Some(id), "Unknown", None, memory, 0, 0, 0))
    Some(describe) -> {
      use description <- result.map(describe(pid))
      GuildMemory(
        guild_id: Some(id),
        name: description.name,
        icon: description.icon,
        memory:,
        member_count: description.member_count,
        session_count: description.session_count,
        presence_count: description.presence_count,
      )
    }
  }
}

pub fn compare(a: GuildMemory, b: GuildMemory) -> order.Order {
  case int.compare(b.memory, a.memory) {
    order.Eq ->
      case a.guild_id, b.guild_id {
        None, None -> order.Eq
        None, Some(_) -> order.Lt
        Some(_), None -> order.Gt
        Some(left), Some(right) -> string.compare(left, right)
      }
    other -> other
  }
}

pub fn to_json(guild: GuildMemory, node_id: String) -> Json {
  json.object([
    #("guild_icon", json.nullable(guild.icon, json.string)),
    #("guild_id", json.nullable(guild.guild_id, json.string)),
    #("guild_name", json.string(guild.name)),
    #("member_count", json.int(guild.member_count)),
    #("memory", json.string(int.to_string(guild.memory))),
    #("node_id", json.string(node_id)),
    #("presence_count", json.int(guild.presence_count)),
    #("session_count", json.int(guild.session_count)),
  ])
}
