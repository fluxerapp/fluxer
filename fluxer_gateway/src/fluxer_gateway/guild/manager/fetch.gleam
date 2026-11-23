// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client.{type ApiError}
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock.{type Deadline}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/json_scan
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/data.{type Loaded}
import fluxer_gateway/guild/data/decode.{type Collection, Items, Members, Whole} as data_decode
import fluxer_gateway/util/spawn
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

pub fn fetch_timeout_ms() -> Int {
  durations.guild().fetch_timeout_ms
}

const member_page_limit = 1000

fn worker_stop_timeout_ms() -> Int {
  durations.guild().fetch_worker_stop_timeout_ms
}

const collections = ["guild", "roles", "channels", "emojis", "stickers"]

pub type FetchError {
  FetchApi(error: ApiError)
  FetchTimeout
  FetchCrashed(name: String)
  BadCollection(name: String)
}

type Collected(a) {
  Collected(name: String, result: Result(a, FetchError))
  WorkerDown(name: String, reason: process.ExitReason)
}

type Worker {
  Worker(pid: process.Pid, monitor: process.Monitor)
}

pub fn load(guild: GuildId, deadline: Deadline) -> Result(Loaded, FetchError) {
  use collected <- result.try(gather(
    ["members", ..collections],
    fn(name) { fetch_collection(guild, name, deadline) },
    deadline,
  ))
  data_decode.collections(guild, collected)
  |> result.replace_error(BadCollection("guild"))
}

pub fn gather(
  names: List(String),
  work: fn(String) -> Result(a, FetchError),
  deadline: Deadline,
) -> Result(dict.Dict(String, a), FetchError) {
  let replies = process.new_subject()
  let workers =
    list.fold(names, dict.new(), fn(workers, name) {
      let #(pid, monitor) =
        spawn.monitored(fn() {
          process.send(replies, Collected(name, work(name)))
        })
      dict.insert(workers, name, Worker(pid, monitor))
    })
  let selector =
    dict.fold(
      workers,
      process.new_selector() |> process.select(replies),
      fn(selector, name, worker) {
        process.select_specific_monitor(selector, worker.monitor, fn(down) {
          WorkerDown(name, down.reason)
        })
      },
    )
  let #(outcome, live) =
    collect(selector, dict.size(workers), deadline, dict.new(), workers)
  dict.each(live, fn(_, worker) { stop_worker(worker) })
  flush_results(replies)
  outcome
}

fn collect(
  selector: process.Selector(Collected(a)),
  left: Int,
  deadline: Deadline,
  acc: dict.Dict(String, a),
  live: dict.Dict(String, Worker),
) -> #(Result(dict.Dict(String, a), FetchError), dict.Dict(String, Worker)) {
  case left {
    0 -> #(Ok(acc), live)
    _ ->
      case process.selector_receive(selector, clock.remaining_ms(deadline)) {
        Error(Nil) -> #(Error(FetchTimeout), live)
        Ok(Collected(_, Error(error))) -> #(Error(error), live)
        Ok(Collected(name, Ok(value))) ->
          collect(
            selector,
            left - 1,
            deadline,
            dict.insert(acc, name, value),
            live,
          )
        Ok(WorkerDown(name, process.Normal)) ->
          collect(selector, left, deadline, acc, dict.delete(live, name))
        Ok(WorkerDown(name, _)) -> #(
          Error(FetchCrashed(name)),
          dict.delete(live, name),
        )
      }
  }
}

fn stop_worker(worker: Worker) -> Nil {
  process.kill(worker.pid)
  let down =
    process.new_selector()
    |> process.select_specific_monitor(worker.monitor, fn(_) { Nil })
  case process.selector_receive(down, worker_stop_timeout_ms()) {
    Ok(Nil) -> Nil
    Error(Nil) -> process.demonitor_process(worker.monitor)
  }
}

fn flush_results(replies: process.Subject(Collected(a))) -> Nil {
  case process.receive(replies, 0) {
    Ok(_) -> flush_results(replies)
    Error(Nil) -> Nil
  }
}

fn fetch_collection(
  guild: GuildId,
  name: String,
  deadline: Deadline,
) -> Result(Collection, FetchError) {
  case name {
    "members" -> fetch_members(guild, None, [], deadline)
    _ -> {
      use reply <- result.try(
        client.call_raw_with_retry(
          "guild_collection",
          [
            #("guild_id", id.guild_json(guild)),
            #("collection", json.string(name)),
          ],
          deadline,
        )
        |> result.map_error(FetchApi),
      )
      let fields = json_scan.object(reply) |> result.unwrap([])
      case name {
        "channels" -> channels_collection(fields)
        _ ->
          list.key_find(fields, name)
          |> result.map(Whole)
          |> result.replace_error(BadCollection(name))
      }
    }
  }
}

fn channels_collection(
  fields: List(#(String, BitArray)),
) -> Result(Collection, FetchError) {
  use channels <- result.try(
    list.key_find(fields, "channels")
    |> result.try(json_scan.array)
    |> result.replace_error(BadCollection("channels")),
  )
  let forums =
    list.key_find(fields, "thread_only_channels")
    |> result.try(json_scan.array)
    |> result.unwrap([])
  let known = list.map(channels, channel_id) |> set.from_list
  list.filter(forums, fn(forum) { !set.contains(known, channel_id(forum)) })
  |> list.append(channels, _)
  |> Items
  |> Ok
}

fn channel_id(channel: BitArray) -> Option(BitArray) {
  json_scan.field(channel, "id") |> option.from_result
}

fn fetch_members(
  guild: GuildId,
  after: Option(String),
  pages: List(Subject(List(Member))),
  deadline: Deadline,
) -> Result(Collection, FetchError) {
  let fields = [
    #("guild_id", id.guild_json(guild)),
    #("collection", json.string("members")),
    #("limit", json.int(member_page_limit)),
  ]
  let fields = case after {
    Some(cursor) -> [#("after_user_id", json.string(cursor)), ..fields]
    None -> fields
  }
  use reply <- result.try(
    client.call_raw_with_retry("guild_collection", fields, deadline)
    |> result.map_error(FetchApi),
  )
  let top = json_scan.object(reply) |> result.unwrap([])
  let page = list.key_find(top, "members") |> result.try(json_scan.array)
  let has_more = read(top, "has_more", decode.bool) |> result.unwrap(False)
  let next =
    read(top, "next_after_user_id", decode.optional(decode.string))
    |> result.unwrap(None)
  case page, has_more, next {
    Error(_), _, _ -> Error(BadCollection("members"))
    Ok([]), True, _ -> Error(BadCollection("members"))
    Ok(rows), True, Some(cursor) ->
      fetch_members(guild, Some(cursor), [decode_page(rows), ..pages], deadline)
    Ok(_), True, None -> Error(BadCollection("members"))
    Ok(rows), False, _ ->
      [decode_page(rows), ..pages]
      |> list.reverse
      |> list.try_map(process.receive(_, clock.remaining_ms(deadline)))
      |> result.map(fn(decoded) { Members(list.flatten(decoded)) })
      |> result.replace_error(FetchTimeout)
  }
}

fn decode_page(rows: List(BitArray)) -> Subject(List(Member)) {
  let reply = process.new_subject()
  let _ = process.spawn(fn() { process.send(reply, data_decode.members(rows)) })
  reply
}

fn read(
  fields: List(#(String, BitArray)),
  key: String,
  decoder: decode.Decoder(a),
) -> Result(a, Nil) {
  list.key_find(fields, key)
  |> result.try(fn(value) { raw_json.read(raw_json.wrap(value), decoder) })
}
