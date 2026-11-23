// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client.{type ApiError}
import fluxer_gateway/config/rollout
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId, type GuildId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/threads/thread.{type Thread, type ThreadMember}
import fluxer_gateway/threads/value.{type Object}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const member_page_limit = 1000

const member_pages_per_thread = 2

const member_thread_batch = 100

pub type Payload {
  Payload(
    threads: List(Thread),
    members: List(ThreadMember),
    parent_settings: List(#(ChannelId, RawJson)),
  )
}

pub fn fetch(guild: GuildId) -> Result(Payload, ApiError) {
  let fields = [
    #("guild_id", id.guild_json(guild)),
    #("config_version", json.int(0)),
    #("paged_members", json.bool(True)),
  ]
  use reply <- result.try(client.call_raw(
    "guild_thread_flip_data",
    fields,
    request_deadline(),
  ))
  let data = value.expand_object(raw_json.wrap(reply))
  case value.get(data, "threads"), value.has(data, "thread_members") {
    Ok(json_tree.Array(threads)), False ->
      with_paged_members(guild, threads, data)
    _, _ -> Ok(data)
  }
  |> result.map(payload)
}

fn request_deadline() -> clock.Deadline {
  clock.deadline_in(rollout.rpc_request_timeout_ms())
}

fn with_paged_members(
  guild: GuildId,
  threads: List(JsonTree),
  data: Object,
) -> Result(Object, ApiError) {
  let thread_ids =
    value.objects(json_tree.Array(threads))
    |> list.filter_map(fn(entry) {
      case value.get(entry, "id") {
        Ok(json_tree.String(thread_id)) -> Ok(thread_id)
        _ -> Error(Nil)
      }
    })
  use members <- result.map(fetch_thread_members(guild, thread_ids, []))
  value.insert(
    data,
    "thread_members",
    json_tree.Array(list.map(members, json_tree.Object)),
  )
}

fn fetch_thread_members(
  guild: GuildId,
  thread_ids: List(String),
  acc: List(List(Object)),
) -> Result(List(Object), ApiError) {
  case thread_ids {
    [] -> Ok(list.flatten(list.reverse(acc)))
    _ -> {
      let #(batch, rest) = list.split(thread_ids, member_thread_batch)
      let pages = member_pages_per_thread * list.length(batch)
      use acc <- result.try(fetch_member_pages(guild, batch, None, acc, pages))
      fetch_thread_members(guild, rest, acc)
    }
  }
}

type Cursor {
  Done
  Invalid
  Next(remaining: List(String), after: Option(String))
}

fn fetch_member_pages(
  guild: GuildId,
  thread_ids: List(String),
  after: Option(String),
  acc: List(List(Object)),
  pages_left: Int,
) -> Result(List(List(Object)), ApiError) {
  case thread_ids, pages_left {
    [], _ | _, 0 -> Ok(acc)
    _, _ -> {
      let fields = [
        #("guild_id", id.guild_json(guild)),
        #("thread_ids", json.array(thread_ids, json.string)),
        #("limit", json.int(member_page_limit)),
        ..case after {
          Some(cursor) -> [#("after_user_id", json.string(cursor))]
          None -> []
        }
      ]
      use reply <- result.try(client.call_raw(
        "list_thread_members",
        fields,
        request_deadline(),
      ))
      let page = value.expand_object(raw_json.wrap(reply))
      case value.get(page, "members") {
        Ok(json_tree.Array(members)) -> {
          let acc = [value.objects(json_tree.Array(members)), ..acc]
          case next_cursor(page, thread_ids) {
            Done -> Ok(acc)
            Invalid -> Error(client.ApiBadReply)
            Next(remaining, cursor) ->
              fetch_member_pages(guild, remaining, cursor, acc, pages_left - 1)
          }
        }
        _ -> Error(client.ApiBadReply)
      }
    }
  }
}

fn next_cursor(page: Object, thread_ids: List(String)) -> Cursor {
  case value.get(page, "has_more"), value.get(page, "next_thread_id") {
    Ok(json_tree.Bool(True)), Ok(json_tree.String(next_thread)) -> {
      let remaining =
        list.drop_while(thread_ids, fn(entry) { entry != next_thread })
      case remaining, value.get(page, "next_after_user_id") {
        [], _ -> Invalid
        _, Ok(json_tree.String(cursor)) -> Next(remaining, Some(cursor))
        _, Ok(json_tree.Null) -> Next(remaining, None)
        _, Error(Nil) -> Next(remaining, None)
        _, _ -> Invalid
      }
    }
    Ok(json_tree.Bool(True)), _ -> Invalid
    _, _ -> Done
  }
}

fn payload(object: Object) -> Payload {
  Payload(
    threads: value.object_list(object, "threads")
      |> list.filter_map(thread.from_event),
    members: value.object_list(object, "thread_members")
      |> list.filter_map(thread.member_from_object),
    parent_settings: value.object_list(object, "thread_parent_settings")
      |> list.filter_map(fn(settings) {
        use raw <- result.try(
          value.snowflake_field(settings, "channel_id") |> option.to_result(Nil),
        )
        use channel <- result.map(
          id.channel_id(raw) |> result.replace_error(Nil),
        )
        #(channel, value.to_raw(value.delete(settings, "channel_id")))
      }),
  )
}
