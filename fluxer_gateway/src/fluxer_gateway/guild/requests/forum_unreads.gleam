// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client
import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/channel
import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/data
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/perms
import fluxer_gateway/threads/store as thread_store
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/viewers as thread_viewers
import fluxer_gateway/util/spawn
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

const max_threads = 40

const max_count = 25

const lookup_grace_ms = 1000

fn api_timeout_ms() -> Int {
  durations.guild().forum_unreads_api_timeout_ms
}

type Scope {
  Stored(last_message: Option(Int))
  Unstored
  Foreign
}

type Local {
  Ready(fields: List(#(String, Json)))
  Remote(ack: Option(Int))
}

type Requested {
  Requested(thread: Int, ack: Option(Int))
}

pub fn handle(state: GuildState, ref: SessionRef, data: Dynamic) -> GuildState {
  let parsed = json_tree.from_term(data)
  let guild = guild_state.guild_id(state)
  case
    table.get(guild_state.sessions(state), ref.id),
    json_tree.snowflake_field(parsed, "guild_id"),
    json_tree.snowflake_field(parsed, "channel_id"),
    threads(parsed)
  {
    Some(found), Ok(guild_int), Ok(forum_int), [_, ..] as requested ->
      case
        guild_int == id.guild_id_to_int(guild)
        && viewer(found)
        && forum_visible(state, ref, forum_int)
      {
        True -> {
          answer(state, ref, forum_int, requested)
          state
        }
        False -> state
      }
    _, _, _, _ -> state
  }
}

fn viewer(found: entry.Entry) -> Bool {
  thread_viewers.is_viewer_session(entry.flags(found), entry.is_bot(found))
}

fn forum_visible(state: GuildState, ref: SessionRef, forum_int: Int) -> Bool {
  {
    use forum_id <- result.try(
      id.channel_id(forum_int) |> result.replace_error(Nil),
    )
    use row <- result.map(
      data.find_channel(guild_state.guild_data(state), forum_id)
      |> option.to_result(Nil),
    )
    channel.is_thread_only_kind(channel.kind(row))
    && perms.can_view(guild_state.perms_reader(state), ref.user, forum_id)
    == Ok(True)
  }
  |> result.unwrap(False)
}

fn threads(parsed: JsonTree) -> List(Requested) {
  let #(kept, _) =
    items(parsed, "threads")
    |> list.fold(#([], set.new()), fn(acc, item) {
      let #(kept, seen) = acc
      case
        list.length(kept) >= max_threads,
        json_tree.snowflake_field(item, "thread_id")
      {
        False, Ok(thread_int) ->
          case set.contains(seen, thread_int) {
            True -> acc
            False -> #(
              [
                Requested(
                  thread: thread_int,
                  ack: json_tree.snowflake_field(item, "ack_message_id")
                    |> option.from_result,
                ),
                ..kept
              ],
              set.insert(seen, thread_int),
            )
          }
        _, _ -> acc
      }
    })
  list.reverse(kept)
}

fn answer(
  state: GuildState,
  ref: SessionRef,
  forum_int: Int,
  requested: List(Requested),
) -> Nil {
  let store = guild_state.threads(state)
  let guild = guild_state.guild_id(state)
  let local =
    list.filter_map(requested, fn(request) {
      case scope(store, request.thread, forum_int) {
        Foreign -> Error(Nil)
        found_scope ->
          Ok(#(request.thread, local_entry(request.ack, found_scope)))
      }
    })
  let remote =
    list.filter_map(local, fn(pair) {
      case pair {
        #(thread_int, Remote(ack)) -> Ok(Requested(thread: thread_int, ack:))
        #(_, Ready(_)) -> Error(Nil)
      }
    })
  case remote {
    [] -> deliver(ref, guild, forum_int, local, dict.new())
    _ -> {
      process.spawn_unlinked(fn() {
        deliver(
          ref,
          guild,
          forum_int,
          local,
          guarded_fetch(guild, forum_int, ref.user, remote),
        )
      })
      Nil
    }
  }
}

fn guarded_fetch(
  guild: id.GuildId,
  forum_int: Int,
  user: id.UserId,
  remote: List(Requested),
) -> dict.Dict(Int, List(#(String, Json))) {
  let reply = process.new_subject()
  let #(worker, monitor) =
    spawn.monitored(fn() {
      process.send(reply, fetch(guild, forum_int, user, remote))
    })
  let found =
    process.new_selector()
    |> process.select(reply)
    |> process.select_specific_monitor(monitor, fn(_) { dict.new() })
    |> process.selector_receive(api_timeout_ms() + lookup_grace_ms)
  process.demonitor_process(monitor)
  case found {
    Ok(entries) -> entries
    Error(Nil) -> {
      process.kill(worker)
      dict.new()
    }
  }
}

fn scope(
  store: thread_store.ThreadStore,
  thread_int: Int,
  forum_int: Int,
) -> Scope {
  let stored = {
    use thread_id <- result.try(
      id.channel_id(thread_int) |> result.replace_error(Nil),
    )
    thread_store.thread(store, thread_id) |> option.to_result(Nil)
  }
  case stored {
    Error(Nil) -> Unstored
    Ok(found) ->
      case option.map(thread.parent(found), id.channel_id_to_int) {
        Some(parent_int) if parent_int == forum_int ->
          Stored(thread.last_message_id(found))
        _ -> Foreign
      }
  }
}

fn local_entry(ack: Option(Int), found_scope: Scope) -> Local {
  case ack, found_scope {
    None, Stored(_) -> Ready([#("missing", json.bool(True))])
    Some(acked), Stored(None) -> zero_or_remote(True, acked)
    Some(acked), Stored(Some(last)) -> zero_or_remote(last <= acked, acked)
    _, _ -> Remote(ack)
  }
}

fn zero_or_remote(read: Bool, acked: Int) -> Local {
  case read {
    True -> Ready([#("count", json.int(0))])
    False -> Remote(Some(acked))
  }
}

fn fetch(
  guild: id.GuildId,
  forum_int: Int,
  user: id.UserId,
  remote: List(Requested),
) -> dict.Dict(Int, List(#(String, Json))) {
  let fields = [
    #("guild_id", id.guild_json(guild)),
    #("channel_id", id.json(forum_int)),
    #("user_id", id.user_json(user)),
    #(
      "threads",
      json.preprocessed_array(
        list.map(remote, fn(request) {
          let thread_field = #("thread_id", id.json(request.thread))
          case request.ack {
            Some(ack) ->
              json.object([
                thread_field,
                #("ack_message_id", id.json(ack)),
              ])
            None -> json.object([thread_field])
          }
        }),
      ),
    ),
  ]
  let wanted = set.from_list(list.map(remote, fn(request) { request.thread }))
  case
    client.call_raw(
      "forum_unreads",
      fields,
      clock.deadline_in(api_timeout_ms()),
    )
  {
    Ok(reply) ->
      items(json_tree.from_raw(raw_json.wrap(reply)), "threads")
      |> list.filter_map(fn(item) {
        use thread_int <- result.try(json_tree.snowflake_field(
          item,
          "thread_id",
        ))
        case set.contains(wanted, thread_int), rpc_fields(item) {
          True, Some(found) -> Ok(#(thread_int, found))
          _, _ -> Error(Nil)
        }
      })
      |> dict.from_list
    Error(_) -> dict.new()
  }
}

fn items(tree: JsonTree, key: String) -> List(JsonTree) {
  json_tree.field(tree, key)
  |> result.try(json_tree.items)
  |> result.unwrap([])
}

fn rpc_fields(item: JsonTree) -> Option(List(#(String, Json))) {
  case
    json_tree.field(item, "missing") |> result.try(json_tree.bool) == Ok(True),
    json_tree.field(item, "count") |> result.try(json_tree.int)
  {
    True, _ -> Some([#("missing", json.bool(True))])
    False, Ok(count) if count >= 0 ->
      Some([#("count", json.int(int.min(count, max_count)))])
    _, _ -> None
  }
}

fn deliver(
  ref: SessionRef,
  guild: id.GuildId,
  forum_int: Int,
  local: List(#(Int, Local)),
  remote: dict.Dict(Int, List(#(String, Json))),
) -> Nil {
  let entries =
    list.filter_map(local, fn(pair) {
      let #(thread_int, found) = pair
      let fields = case found {
        Ready(fields) -> Ok(fields)
        Remote(_) -> dict.get(remote, thread_int)
      }
      use fields <- result.map(fields)
      json.object([#("thread_id", id.json(thread_int)), ..fields])
    })
  let payload =
    json.object([
      #("guild_id", id.guild_json(guild)),
      #("channel_id", id.json(forum_int)),
      #("threads", json.preprocessed_array(entries)),
    ])
  relay.send(
    [ref],
    session.guild_dispatch(guild, "FORUM_UNREADS", raw_json.from_json(payload)),
  )
}
