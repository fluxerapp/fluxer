// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type MessageId, type UserId}
import fluxer_gateway/push/job.{type Caller, type Job, type Template}
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/outbox
import fluxer_gateway/push/recipients.{type Recipient}
import fluxer_gateway/push/rule
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import logging

const nats_max_payload_bytes = 1_048_576

pub type PublishError {
  PayloadTooLarge(bytes: Int)
  InvalidUserIds
  NotQueued(outbox.EnqueueError)
}

pub fn publish_message(
  create: MessageCreate,
  recipients: List(Recipient),
) -> Result(Nil, PublishError) {
  let #(direct, bulk) =
    list.partition(recipients, fn(recipient) { recipient.class == rule.Direct })
  let users = fn(group: List(Recipient)) {
    list.map(group, fn(recipient) { recipient.user })
  }
  let template = job.message_template(create)
  let first = publish_class(template, rule.Direct, users(direct))
  let second = publish_class(template, rule.Bulk, users(bulk))
  use Nil <- result.try(first)
  second
}

fn publish_class(
  template: Template,
  class: rule.Class,
  users: List(Int),
) -> Result(Nil, PublishError) {
  case users {
    [] -> Ok(Nil)
    _ -> publish_users(template, class, users)
  }
}

fn publish_users(
  template: Template,
  class: rule.Class,
  users: List(Int),
) -> Result(Nil, PublishError) {
  case job.size(template, users) > nats_max_payload_bytes, users {
    True, [_, _, ..] -> {
      let #(left, right) = list.split(users, list.length(users) / 2)
      let left_result = publish_users(template, class, left)
      let right_result = publish_users(template, class, right)
      use Nil <- result.try(left_result)
      right_result
    }
    _, _ ->
      case job.for_users(template, class, users) {
        Ok(item) -> enqueue(item)
        Error(Nil) -> {
          logging.log(logging.Warning, "Push job has user ids out of range")
          outbox.record_dropped(job.MessageJob, class, "invalid_user_ids")
          Error(InvalidUserIds)
        }
      }
  }
}

fn enqueue(item: Job) -> Result(Nil, PublishError) {
  let bytes = bit_array.byte_size(job.body(item))
  case bytes > nats_max_payload_bytes {
    True -> {
      logging.log(logging.Warning, "Push job exceeds the NATS payload limit")
      outbox.record_dropped(item.kind, item.class, "payload_too_large")
      Error(PayloadTooLarge(bytes))
    }
    False ->
      case outbox.enqueue(item) {
        Ok(Nil) -> Ok(Nil)
        Error(outbox.OutboxUnavailable) ->
          Error(NotQueued(outbox.OutboxUnavailable))
        Error(outbox.EnqueueTimeout) -> {
          logging.log(logging.Warning, "Push job publish failed")
          outbox.record_dropped(item.kind, item.class, "enqueue_timeout")
          Error(NotQueued(outbox.EnqueueTimeout))
        }
      }
  }
}

pub fn publish_clear(
  user: UserId,
  channel: ChannelId,
  message: MessageId,
) -> Result(Nil, PublishError) {
  let clear =
    job.clear_job(
      user: id.user_id_to_int(user),
      channel: id.channel_id_to_int(channel),
      message: id.message_id_to_int(message),
      after: None,
    )
  enqueue(clear)
}

pub fn publish_ring(
  user user: UserId,
  channel channel: ChannelId,
  message message: MessageId,
  started_at_ms started_at_ms: Int,
  expires_at_ms expires_at_ms: Int,
  caller caller: Option(Caller),
) -> Result(Nil, PublishError) {
  let ring =
    job.ring_job(
      user: id.user_id_to_int(user),
      channel: id.channel_id_to_int(channel),
      message: id.message_id_to_int(message),
      started_at_ms:,
      expires_at_ms:,
      caller:,
    )
  enqueue(ring)
}
