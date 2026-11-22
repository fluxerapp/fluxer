// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/job.{type Job}
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/rule
import gleam/bit_array
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/push_job_fixture as fx

const guild_message_body =
  "{\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"guild_id\":\"1400000000000000900\",\"message_id\":\"1400000000000000500\",\"notification\":{\"author_name\":\"Scribe\",\"badge\":\"http://localhost:8088/marketing/branding/symbol-white.svg\",\"body\":\"hey @Ally and @Bobby, **see** #general #linked ping @mods 😀\",\"conversation_kind\":\"guild\",\"conversation_name\":\"general\",\"guild_name\":\"Guild <One>\",\"icon\":\"http://localhost:8088/media/avatars/1400000000000000007/a_hash.png\",\"image_url\":null,\"notification_tag\":\"channel:1400000000000000100\",\"tag\":\"channel:1400000000000000100:1400000000000000500\",\"title\":\"Scribe (#general, Guild <One>)\",\"url\":\"/channels/1400000000000000900/1400000000000000100/1400000000000000500\"},\"user_ids\":[\"1400000000000000001\",\"1400000000000000002\"],\"v\":1}"

const thread_message_body =
  "{\"channel_id\":\"1400000000000000100\",\"channel_name\":\"a thread\",\"config_version\":0,\"guild_id\":\"1400000000000000900\",\"kind\":\"forum_thread_created\",\"message_id\":\"1400000000000000500\",\"notification\":{\"author_name\":\"The \\\"Writer\\\"\",\"badge\":\"http://localhost:8088/marketing/branding/symbol-white.svg\",\"body\":\"hey @alice and @Bobby, **see** #unknown-channel #linked ping @unknown-role 😀\",\"conversation_kind\":\"guild\",\"conversation_name\":\"a thread, #forum\",\"guild_name\":\"Guild\",\"icon\":\"http://localhost:8088/media/avatars/1400000000000000007/a_hash.png\",\"image_url\":null,\"notification_tag\":\"channel:1400000000000000100\",\"tag\":\"channel:1400000000000000100:1400000000000000500\",\"title\":\"The \\\"Writer\\\" (#a thread, #forum, Guild)\",\"url\":\"/channels/1400000000000000900/1400000000000000100/1400000000000000500\"},\"parent_id\":\"1400000000000000050\",\"parent_name\":\"forum\",\"user_ids\":[\"1400000000000000001\",\"1400000000000000002\"],\"v\":1}"

const age_restricted_message_body =
  "{\"age_restricted\":true,\"channel_id\":\"1400000000000000100\",\"channel_name\":\"a thread\",\"config_version\":0,\"guild_id\":\"1400000000000000900\",\"kind\":\"thread\",\"message_id\":\"1400000000000000500\",\"notification\":{\"author_name\":\"The \\\"Writer\\\"\",\"badge\":\"http://localhost:8088/marketing/branding/symbol-white.svg\",\"body\":\"hey @alice and @Bobby, **see** #unknown-channel #linked ping @unknown-role 😀\",\"conversation_kind\":\"guild\",\"conversation_name\":\"a thread, #forum\",\"guild_name\":\"Guild\",\"icon\":\"http://localhost:8088/media/avatars/1400000000000000007/a_hash.png\",\"image_url\":null,\"notification_tag\":\"channel:1400000000000000100\",\"tag\":\"channel:1400000000000000100:1400000000000000500\",\"title\":\"The \\\"Writer\\\" (#a thread, #forum, Guild)\",\"url\":\"/channels/1400000000000000900/1400000000000000100/1400000000000000500\"},\"parent_id\":\"1400000000000000050\",\"parent_name\":\"forum\",\"user_ids\":[\"1400000000000000001\",\"1400000000000000002\"],\"v\":1}"

const dm_message_body =
  "{\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"guild_id\":\"0\",\"message_id\":\"1400000000000000500\",\"notification\":{\"author_name\":\"Pal\",\"badge\":\"http://localhost:8088/marketing/branding/symbol-white.svg\",\"body\":\"hey @alice and @Bobby, **see** #unknown-channel #linked ping @unknown-role 😀\",\"conversation_kind\":\"dm\",\"icon\":\"http://localhost:8088/media/avatars/1400000000000000007/a_hash.png\",\"image_url\":null,\"notification_tag\":\"channel:1400000000000000100\",\"tag\":\"channel:1400000000000000100:1400000000000000500\",\"title\":\"Pal\",\"url\":\"/channels/@me/1400000000000000100/1400000000000000500\"},\"user_ids\":[\"1400000000000000001\",\"1400000000000000002\"],\"v\":1}"

const group_dm_attachment_body =
  "{\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"guild_id\":\"0\",\"message_id\":\"1400000000000000500\",\"notification\":{\"author_name\":\"writer\",\"badge\":\"http://localhost:8088/marketing/branding/symbol-white.svg\",\"body\":\"Attachment: cat.png\",\"conversation_kind\":\"group_dm\",\"conversation_name\":\"friends\",\"icon\":\"http://localhost:8088/avatars/3.png\",\"image_url\":\"https://cdn.example/a.png\",\"notification_tag\":\"channel:1400000000000000100\",\"preview_arg\":\"cat.png\",\"preview_kind\":\"attachment\",\"tag\":\"channel:1400000000000000100:1400000000000000500\",\"title\":\"writer (Group DM)\",\"url\":\"/channels/@me/1400000000000000100/1400000000000000500\"},\"user_ids\":[],\"v\":1}"

const clear_after_body =
  "{\"after_message_id\":\"1400000000000000050\",\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"message_id\":\"1400000000000000500\",\"user_id\":\"1400000000000000001\",\"v\":1}"

const clear_body =
  "{\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"message_id\":\"1400000000000000500\",\"user_id\":\"1400000000000000001\",\"v\":1}"

const ring_caller_avatar_body =
  "{\"caller_avatar_url\":\"http://localhost:8088/media/avatars/1400000000000000002/h.png\",\"caller_id\":\"1400000000000000002\",\"caller_name\":\"Bob 😀\",\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"expires_at_ms\":2000,\"message_id\":\"1400000000000000500\",\"started_at_ms\":1000,\"user_id\":\"1400000000000000001\",\"v\":1}"

const ring_caller_default_avatar_body =
  "{\"caller_avatar_url\":\"http://localhost:8088/avatars/4.png\",\"caller_id\":\"1400000000000000002\",\"caller_name\":\"Bob\",\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"expires_at_ms\":2000,\"message_id\":\"1400000000000000500\",\"started_at_ms\":1000,\"user_id\":\"1400000000000000001\",\"v\":1}"

const ring_without_caller_body =
  "{\"channel_id\":\"1400000000000000100\",\"config_version\":0,\"expires_at_ms\":2000,\"message_id\":\"1400000000000000500\",\"started_at_ms\":1000,\"user_id\":\"1400000000000000001\",\"v\":1}"

fn text(built: Job) -> String {
  let assert Ok(body) = bit_array.to_string(job.body(built))
  body
}

fn message(create: MessageCreate, class: rule.Class, users: List(Int)) -> Job {
  let assert Ok(built) =
    job.for_users(job.message_template(create), class, users)
  built
}

pub fn guild_message_body_is_unchanged_test() {
  assert text(message(fx.guild_message(), rule.Direct, [fx.alice, fx.bob]))
    == guild_message_body
}

pub fn thread_message_body_is_unchanged_test() {
  assert text(message(fx.thread_message(), rule.Bulk, [fx.alice, fx.bob]))
    == thread_message_body
}

pub fn age_restricted_message_is_flagged_not_masked_test() {
  assert text(
      message(fx.age_restricted_message(), rule.Direct, [fx.alice, fx.bob]),
    )
    == age_restricted_message_body
}

pub fn dm_message_body_is_unchanged_test() {
  assert text(message(fx.dm_message(), rule.Direct, [fx.alice, fx.bob]))
    == dm_message_body
}

pub fn group_dm_attachment_body_is_unchanged_test() {
  assert text(message(fx.group_dm_attachment(), rule.Direct, []))
    == group_dm_attachment_body
}

pub fn clear_bodies_are_unchanged_test() {
  assert text(job.clear_job(
      user: fx.alice,
      channel: fx.channel,
      message: fx.message_id,
      after: Some(fx.parent),
    ))
    == clear_after_body
  assert text(job.clear_job(
      user: fx.alice,
      channel: fx.channel,
      message: fx.message_id,
      after: None,
    ))
    == clear_body
}

fn ring(caller: option.Option(job.Caller)) -> Job {
  job.ring_job(
    user: fx.alice,
    channel: fx.channel,
    message: fx.message_id,
    started_at_ms: 1000,
    expires_at_ms: 2000,
    caller:,
  )
}

pub fn ring_bodies_are_unchanged_test() {
  assert text(
      ring(
        Some(job.Caller(id: fx.bob, name: "Bob \u{1F600}", avatar: Some("h"))),
      ),
    )
    == ring_caller_avatar_body
  assert text(ring(Some(job.Caller(id: fx.bob, name: "Bob", avatar: None))))
    == ring_caller_default_avatar_body
  assert text(ring(None)) == ring_without_caller_body
}

pub fn size_matches_the_written_body_test() {
  let template = job.message_template(fx.guild_message())
  list.each(
    [
      [],
      [1],
      [9, 10, 99, 100],
      [fx.alice, fx.bob, 999_999_999_999_999_999, 9_223_372_036_854_775_807],
    ],
    fn(users) {
      let assert Ok(built) = job.for_users(template, rule.Bulk, users)
      assert job.size(template, users) == bit_array.byte_size(job.body(built))
    },
  )
}

pub fn with_users_rewrites_only_the_user_list_test() {
  let built = message(fx.guild_message(), rule.Direct, [fx.alice, fx.bob])
  let trimmed = job.with_users(built, [fx.bob])
  assert trimmed.users == [fx.bob]
  assert text(trimmed)
    == string.replace(
      guild_message_body,
      "\"1400000000000000001\",\"1400000000000000002\"",
      "\"1400000000000000002\"",
    )
}

pub fn out_of_range_user_ids_are_refused_test() {
  let template = job.message_template(fx.guild_message())
  assert job.for_users(template, rule.Bulk, [-1]) == Error(Nil)
  assert job.for_users(template, rule.Bulk, [18_446_744_073_709_551_616])
    == Error(Nil)
}
