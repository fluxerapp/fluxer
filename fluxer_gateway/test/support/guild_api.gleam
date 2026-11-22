// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
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
import support/api_breaker
import support/fake_http

pub type FakeApi {
  FakeApi(store: Subject(StoreMessage), url: String, route: fake_http.Route)
}

pub type Reply {
  Data(Json)
  Status(Int, String)
  Delayed(Int, Reply)
  Hang
}

pub type Call {
  Call(kind: String, body: Dynamic)
}

pub opaque type StoreMessage {
  Put(guild: Int, name: String, value: Json)
  PutMembers(guild: Int, members: List(#(Int, Json)))
  SetPageSize(Int)
  Override(kind: String, collection: Option(String), reply: Reply)
  Answer(call: Call, reply: Subject(Reply))
  Calls(reply: Subject(List(Call)))
}

type Store {
  Store(
    collections: Dict(#(Int, String), Json),
    members: Dict(Int, List(#(Int, Json))),
    page_size: Int,
    overrides: Dict(#(String, Option(String)), Reply),
    calls: List(Call),
  )
}

pub fn with_api(body: fn(FakeApi) -> Nil) -> Nil {
  with_api_for(60_000, body)
}

pub fn with_api_for(within_ms: Int, body: fn(FakeApi) -> Nil) -> Nil {
  let previous = settings.get()
  let api = start()
  let _ =
    settings.store(
      settings.Settings(..previous, api_rpc_endpoint: Some(api.url)),
    )
  let _ = api_breaker.reset()
  let pid = process.spawn_unlinked(fn() { body(api) })
  let monitor = process.monitor(pid)
  let down =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down.reason })
  let reason = process.selector_receive(down, within_ms)
  let _ = settings.store(previous)
  process.kill(pid)
  stop(api)
  let assert Ok(process.Normal) = reason
  Nil
}

pub fn start() -> FakeApi {
  let assert Ok(started) =
    actor.new(Store(dict.new(), dict.new(), 1000, dict.new(), []))
    |> actor.on_message(handle_store)
    |> actor.start
  let store = started.data
  let route = fake_http.route(fn(request) { serve(store, request) })
  FakeApi(store:, url: fake_http.url(route) <> "/rpc", route:)
}

pub fn stop(api: FakeApi) -> Nil {
  fake_http.unroute(api.route)
}

pub fn put(api: FakeApi, guild: Int, name: String, value: Json) -> Nil {
  put_data(api, guild, name, json.object([#(name, value)]))
}

pub fn put_data(api: FakeApi, guild: Int, name: String, data: Json) -> Nil {
  process.send(api.store, Put(guild, name, data))
}

pub fn put_members(
  api: FakeApi,
  guild: Int,
  members: List(#(Int, Json)),
) -> Nil {
  process.send(api.store, PutMembers(guild, members))
}

pub fn set_page_size(api: FakeApi, size: Int) -> Nil {
  process.send(api.store, SetPageSize(size))
}

pub fn override(
  api: FakeApi,
  kind: String,
  collection: Option(String),
  reply: Reply,
) -> Nil {
  process.send(api.store, Override(kind, collection, reply))
}

pub fn calls(api: FakeApi) -> List(Call) {
  process.call(api.store, 5000, Calls)
}

pub fn collection_calls(api: FakeApi, name: String) -> List(Call) {
  calls(api)
  |> list.filter(fn(call) {
    call.kind == "guild_collection" && collection_of(call.body) == Some(name)
  })
}

fn handle_store(
  store: Store,
  message: StoreMessage,
) -> actor.Next(Store, StoreMessage) {
  case message {
    Put(guild, name, value) ->
      actor.continue(
        Store(
          ..store,
          collections: dict.insert(store.collections, #(guild, name), value),
        ),
      )
    PutMembers(guild, members) ->
      actor.continue(
        Store(
          ..store,
          members: dict.insert(
            store.members,
            guild,
            list.sort(members, fn(a, b) { int.compare(a.0, b.0) }),
          ),
        ),
      )
    SetPageSize(size) -> actor.continue(Store(..store, page_size: size))
    Override(kind, collection, reply) ->
      actor.continue(
        Store(
          ..store,
          overrides: dict.insert(store.overrides, #(kind, collection), reply),
        ),
      )
    Calls(reply) -> {
      process.send(reply, list.reverse(store.calls))
      actor.continue(store)
    }
    Answer(call, reply) -> {
      process.send(reply, answer(store, call))
      actor.continue(Store(..store, calls: [call, ..store.calls]))
    }
  }
}

fn answer(store: Store, call: Call) -> Reply {
  let collection = collection_of(call.body)
  let overridden =
    dict.get(store.overrides, #(call.kind, collection))
    |> result.lazy_or(fn() { dict.get(store.overrides, #(call.kind, None)) })
  case overridden {
    Ok(reply) -> reply
    Error(Nil) ->
      case call.kind, collection, guild_of(call.body) {
        "guild_collection", Some("members"), Some(guild) ->
          members_page(store, guild, call.body)
        "guild_collection", Some(name), Some(guild) ->
          case dict.get(store.collections, #(guild, name)) {
            Ok(value) -> Data(value)
            Error(Nil) -> Status(404, "{\"code\":\"UNKNOWN_GUILD\"}")
          }
        _, _, _ -> Status(404, "{\"code\":\"UNKNOWN\"}")
      }
  }
}

fn members_page(store: Store, guild: Int, body: Dynamic) -> Reply {
  case dict.get(store.members, guild) {
    Error(Nil) -> Status(404, "{\"code\":\"UNKNOWN_GUILD\"}")
    Ok(all) -> {
      let after =
        decode.run(body, decode.at(["after_user_id"], decode.string))
        |> result.replace_error(Nil)
        |> result.try(int.parse)
      let rest = case after {
        Ok(cursor) -> list.filter(all, fn(pair) { pair.0 > cursor })
        Error(Nil) -> all
      }
      let page = list.take(rest, store.page_size)
      let has_more = list.length(rest) > store.page_size
      let next = case has_more, list.last(page) {
        True, Ok(last) -> json.string(int.to_string(last.0))
        _, _ -> json.null()
      }
      Data(
        json.object([
          #(
            "members",
            json.preprocessed_array(list.map(page, fn(pair) { pair.1 })),
          ),
          #("has_more", json.bool(has_more)),
          #("next_after_user_id", next),
        ]),
      )
    }
  }
}

fn collection_of(body: Dynamic) -> Option(String) {
  decode.run(body, decode.at(["collection"], decode.string))
  |> option.from_result
}

fn guild_of(body: Dynamic) -> Option(Int) {
  decode.run(body, decode.at(["guild_id"], decode.string))
  |> result.replace_error(Nil)
  |> result.try(int.parse)
  |> option.from_result
}

fn serve(
  store: Subject(StoreMessage),
  request: fake_http.Request,
) -> fake_http.Answer {
  let call = case json.parse_bits(request.body, decode.dynamic) {
    Ok(value) ->
      Call(
        kind: decode.run(value, decode.at(["type"], decode.string))
          |> result.unwrap(""),
        body: value,
      )
    Error(_) -> Call("", dynamic.nil())
  }
  answer_with(process.call(store, 5000, Answer(call, _)))
}

fn answer_with(reply: Reply) -> fake_http.Answer {
  case reply {
    Delayed(ms, inner) -> {
      process.sleep(ms)
      answer_with(inner)
    }
    Hang -> {
      process.sleep(30_000)
      fake_http.Close
    }
    Data(value) ->
      fake_http.Answer(200, json.to_string(json.object([#("data", value)])))
    Status(status, text) -> fake_http.Answer(status, text)
  }
}
