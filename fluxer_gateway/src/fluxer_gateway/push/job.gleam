// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings as config
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/push/format
import fluxer_gateway/push/image_url
import fluxer_gateway/push/message.{type MessageCreate, type MessageData}
import fluxer_gateway/push/rule.{type Class}
import gleam/bit_array
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

const subject_message = "push.job.message"

const subject_clear = "push.job.clear"

const subject_ring = "push.job.ring"

const job_version = 1

const legacy_config_version = 0

const max_caller_name_bytes = 128

const users_key = "user_ids"

const dirty_users = 4096

pub type Kind {
  MessageJob
  ClearJob
  RingJob
}

pub type Job {
  Job(
    kind: Kind,
    class: Class,
    users: List(Int),
    channel: Int,
    message: Int,
    wire: Wire,
  )
}

pub opaque type Wire {
  Whole(body: BitArray)
  Listed(head: BitArray, tail: BitArray, body: BitArray)
}

type BodyError

pub opaque type Template {
  Template(head: BitArray, tail: BitArray, channel: Int, message: Int)
}

@external(erlang, "fluxer_gateway@nif", "push_job_body")
fn job_body_nif(
  head: BitArray,
  users: List(Int),
  tail: BitArray,
) -> Result(BitArray, BodyError)

@external(erlang, "fluxer_gateway@nif", "push_job_body_dirty")
fn job_body_dirty_nif(
  head: BitArray,
  users: List(Int),
  tail: BitArray,
) -> Result(BitArray, BodyError)

pub type Caller {
  Caller(id: Int, name: String, avatar: Option(String))
}

pub fn subject(kind: Kind) -> String {
  case kind {
    MessageJob -> subject_message
    ClearJob -> subject_clear
    RingJob -> subject_ring
  }
}

pub fn subject_label(subject: String) -> String {
  case subject {
    _ if subject == subject_message -> "message"
    _ if subject == subject_clear -> "clear"
    _ if subject == subject_ring -> "ring"
    _ -> "other"
  }
}

pub fn kind_label(kind: Kind) -> String {
  case kind {
    MessageJob -> "message"
    ClearJob -> "clear"
    RingJob -> "ring"
  }
}

pub fn body(job: Job) -> BitArray {
  case job.wire {
    Whole(body:) -> body
    Listed(body:, ..) -> body
  }
}

pub fn template(
  fields: List(#(String, Json)),
  channel channel: Int,
  message message: Int,
) -> Template {
  let #(before, after) =
    sorted(fields)
    |> list.partition(fn(field) {
      string.compare(field.0, users_key) == order.Lt
    })
  let head =
    list.append(list.map(before, member), [
      raw_json.wrap(<<"\"", users_key:utf8, "\":">>),
    ])
    |> list.intersperse(raw_json.wrap(<<",">>))
  let tail = case after {
    [] -> [raw_json.wrap(<<"}">>)]
    _ ->
      list.flatten([
        [raw_json.wrap(<<",">>)],
        list.map(after, member) |> list.intersperse(raw_json.wrap(<<",">>)),
        [raw_json.wrap(<<"}">>)],
      ])
  }
  Template(
    head: raw_json.to_bits(raw_json.concat([raw_json.wrap(<<"{">>), ..head])),
    tail: raw_json.to_bits(raw_json.concat(tail)),
    channel:,
    message:,
  )
}

fn member(field: #(String, Json)) -> RawJson {
  raw_json.concat([
    raw_json.from_json(json.string(field.0)),
    raw_json.wrap(<<":">>),
    raw_json.from_json(field.1),
  ])
}

pub fn size(template: Template, users: List(Int)) -> Int {
  bit_array.byte_size(template.head)
  + bit_array.byte_size(template.tail)
  + users_size(users, 0, 0)
}

fn users_size(users: List(Int), count: Int, digits: Int) -> Int {
  case users {
    [] if count == 0 -> 2
    [] -> digits + 3 * count + 1
    [user, ..rest] -> users_size(rest, count + 1, digits + decimal_width(user))
  }
}

fn decimal_width(value: Int) -> Int {
  case value {
    _ if value < 0 -> 1 + decimal_width(0 - value)
    _ if value >= 10_000_000_000_000_000_000 ->
      18 + decimal_width(value / 1_000_000_000_000_000_000)
    _ if value >= 1_000_000_000_000_000_000 -> 19
    _ -> width_below(value, 100_000_000_000_000_000, 18)
  }
}

fn width_below(value: Int, power: Int, digits: Int) -> Int {
  case digits <= 1 || value >= power {
    True -> digits
    False -> width_below(value, power / 10, digits - 1)
  }
}

fn encode_users(
  head: BitArray,
  users: List(Int),
  tail: BitArray,
) -> Result(BitArray, Nil) {
  let encoded = case list.length(users) > dirty_users {
    True -> job_body_dirty_nif(head, users, tail)
    False -> job_body_nif(head, users, tail)
  }
  result.replace_error(encoded, Nil)
}

pub fn for_users(
  template: Template,
  class: Class,
  users: List(Int),
) -> Result(Job, Nil) {
  let Template(head:, tail:, channel:, message:) = template
  use body <- result.map(encode_users(head, users, tail))
  Job(
    kind: MessageJob,
    class:,
    users:,
    channel:,
    message:,
    wire: Listed(head:, tail:, body:),
  )
}

pub fn with_users(job: Job, users: List(Int)) -> Job {
  case job.wire {
    Whole(_) -> Job(..job, users:)
    Listed(head:, tail:, ..) ->
      case encode_users(head, users, tail) {
        Ok(body) -> Job(..job, users:, wire: Listed(head:, tail:, body:))
        Error(Nil) -> job
      }
  }
}

pub fn message_template(create: MessageCreate) -> Template {
  let channel_text = message.id_text(create.data, create.channel)
  let message_text = message.id_text(create.data, create.message)
  let guild_text = guild_text(create.data, create.guild)
  template(
    [
      #("v", json.int(job_version)),
      #("config_version", json.int(legacy_config_version)),
      #("guild_id", json.string(guild_text)),
      #("channel_id", json.string(channel_text)),
      #("message_id", json.string(message_text)),
      #(
        "notification",
        sorted_object(notification(
          create,
          guild_text,
          channel_text,
          message_text,
        )),
      ),
      ..list.append(
        age_restricted_fields(create.data),
        thread_fields(create.data, create.channel_name),
      )
    ],
    channel: create.channel,
    message: create.message,
  )
}

fn age_restricted_fields(data: MessageData) -> List(#(String, Json)) {
  case data.age_restricted {
    True -> [#("age_restricted", json.bool(True))]
    False -> []
  }
}

fn guild_text(data: MessageData, guild: Int) -> String {
  case guild {
    0 -> "0"
    _ -> message.id_text(data, guild)
  }
}

fn whole(
  kind: Kind,
  fields: List(#(String, Json)),
  user: Int,
  channel: Int,
  message: Int,
) -> Job {
  Job(
    kind:,
    class: rule.Direct,
    users: [user],
    channel:,
    message:,
    wire: Whole(raw_json.to_bits(raw_json.from_json(sorted_object(fields)))),
  )
}

pub fn clear_job(
  user user: Int,
  channel channel: Int,
  message message: Int,
  after after: Option(Int),
) -> Job {
  let base = [
    #("v", json.int(job_version)),
    #("config_version", json.int(legacy_config_version)),
    #("user_id", id.json(user)),
    #("channel_id", id.json(channel)),
    #("message_id", id.json(message)),
  ]
  let fields = case after {
    Some(after) -> [#("after_message_id", id.json(after)), ..base]
    None -> base
  }
  whole(ClearJob, fields, user, channel, message)
}

pub fn ring_job(
  user user: Int,
  channel channel: Int,
  message message: Int,
  started_at_ms started_at_ms: Int,
  expires_at_ms expires_at_ms: Int,
  caller caller: Option(Caller),
) -> Job {
  let fields = [
    #("v", json.int(job_version)),
    #("config_version", json.int(legacy_config_version)),
    #("user_id", id.json(user)),
    #("channel_id", id.json(channel)),
    #("message_id", id.json(message)),
    #("started_at_ms", json.int(started_at_ms)),
    #("expires_at_ms", json.int(expires_at_ms)),
    ..caller_fields(caller)
  ]
  whole(RingJob, fields, user, channel, message)
}

fn caller_fields(caller: Option(Caller)) -> List(#(String, Json)) {
  case caller {
    Some(Caller(id: caller_id, name:, avatar:)) if name != "" -> {
      let id_text = id.write(caller_id)
      let avatar_url = case message.nonempty(avatar) {
        Some(hash) -> format.avatar_url(id_text, hash)
        None -> format.default_avatar_url(Some(caller_id))
      }
      [
        #("caller_id", json.string(id_text)),
        #(
          "caller_name",
          json.string(format.truncate_bytes(name, max_caller_name_bytes)),
        ),
        #("caller_avatar_url", json.string(avatar_url)),
      ]
    }
    _ -> []
  }
}

fn notification(
  create: MessageCreate,
  guild_text: String,
  channel_text: String,
  message_text: String,
) -> List(#(String, Json)) {
  let common = [
    #("tag", json.string("channel:" <> channel_text <> ":" <> message_text)),
    #("notification_tag", json.string("channel:" <> channel_text)),
    #(
      "url",
      json.string(format.channel_url(
        create.guild,
        guild_text,
        channel_text,
        message_text,
      )),
    ),
  ]
  list.append(common, message_fields(create)) |> with_badge
}

fn message_fields(create: MessageCreate) -> List(#(String, Json)) {
  let data = create.data
  let author_name = format.author_name(data, create.markdown)
  let preview = format.preview(data, create.markdown)
  let image = case image_url.extract(data) {
    Some(url) -> json.string(url)
    None -> json.null()
  }
  list.flatten([
    [
      #(
        "title",
        json.string(format.title(
          author_name,
          data,
          create.guild,
          create.guild_name,
          create.channel_name,
        )),
      ),
      #("body", json.string(preview.body)),
      #(
        "icon",
        json.string(
          format.author_avatar_url(data.author, message.id_text(data, _)),
        ),
      ),
      #("image_url", image),
    ],
    identity(author_name, data, create),
    preview.fields,
  ])
}

fn identity(
  author_name: String,
  data: MessageData,
  create: MessageCreate,
) -> List(#(String, Json)) {
  case create.guild {
    0 -> {
      let kind = case option.unwrap(data.channel_type, 1) {
        3 -> "group_dm"
        _ -> "dm"
      }
      let name = case kind {
        "group_dm" -> message.nonempty(create.channel_name)
        _ -> None
      }
      present([
        #("author_name", Some(author_name)),
        #("conversation_kind", Some(kind)),
        #("conversation_name", name),
      ])
    }
    _ ->
      present([
        #("author_name", Some(author_name)),
        #("conversation_kind", Some("guild")),
        #("guild_name", create.guild_name),
        #("conversation_name", format.channel_label(data, create.channel_name)),
      ])
  }
}

fn present(fields: List(#(String, Option(String)))) -> List(#(String, Json)) {
  list.filter_map(fields, fn(field) {
    case message.nonempty(field.1) {
      Some(text) -> Ok(#(field.0, json.string(text)))
      None -> Error(Nil)
    }
  })
}

fn with_badge(fields: List(#(String, Json))) -> List(#(String, Json)) {
  case config.get().self_hosted {
    True -> fields
    False -> [
      #(
        "badge",
        json.string(format.static_asset_url(
          "marketing/branding/symbol-white.svg",
        )),
      ),
      ..fields
    ]
  }
}

fn thread_fields(
  data: MessageData,
  channel_name: Option(String),
) -> List(#(String, Json)) {
  case data.thread {
    None -> []
    Some(thread) -> {
      let names = case channel_name {
        Some(name) -> [
          #("channel_name", json.string(name)),
          ..present([#("parent_name", thread.parent_name)])
        ]
        None -> present([#("parent_name", thread.parent_name)])
      }
      let kind = case thread.forum_thread_created {
        True -> "forum_thread_created"
        False -> "thread"
      }
      [
        #("parent_id", id.json(thread.parent_id)),
        #("kind", json.string(kind)),
        ..names
      ]
    }
  }
}

fn sorted(fields: List(#(String, Json))) -> List(#(String, Json)) {
  list.sort(fields, fn(a, b) { string.compare(a.0, b.0) })
}

fn sorted_object(fields: List(#(String, Json))) -> Json {
  json.object(sorted(fields))
}
