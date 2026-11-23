// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/util/backoff
import fluxer_gateway/util/spawn
import fluxer_gateway/voice/registry
import fluxer_gateway/voice/token
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Name, type Pid, type Subject}
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.voice().permission_sync_init_timeout_ms
}

fn retry_base_ms() -> Int {
  durations.voice().permission_sync_retry_base_ms
}

fn retry_max_ms() -> Int {
  durations.voice().permission_sync_retry_max_ms
}

fn retry_jitter_ms() -> Int {
  durations.voice().permission_sync_retry_jitter_ms
}

const max_attempts = 4

type Key =
  #(Int, Int, Int)

pub opaque type Message {
  Enqueue(key: Key, fields: List(#(String, Json)))
  Finished(
    worker: Pid,
    key: Key,
    fields: List(#(String, Json)),
    attempt: Int,
    ok: Bool,
  )
  Retry(key: Key, fields: List(#(String, Json)), attempt: Int)
  WorkerDown(process.Down)
}

type Worker {
  Worker(
    monitor: Monitor,
    key: Key,
    fields: List(#(String, Json)),
    attempt: Int,
  )
}

type Queue {
  Queue(
    self: Subject(Message),
    inflight: Dict(Key, Option(List(#(String, Json)))),
    workers: Dict(Pid, Worker),
  )
}

@external(erlang, "erlang", "binary_to_atom")
fn fixed_name(text: String) -> Name(Message)

fn name() -> Name(Message) {
  fixed_name("fluxer_voice_permission_sync")
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  registry.create_tables()
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_monitors(WorkerDown)
    actor.initialised(Queue(self:, inflight: dict.new(), workers: dict.new()))
    |> actor.selecting(selector)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(name())
  |> actor.start
}

pub fn enqueue_participant(
  guild: GuildId,
  channel: ChannelId,
  user: UserId,
  mute: Bool,
  deaf: Bool,
  perms: token.Permissions,
) -> Nil {
  let key = #(
    id.guild_id_to_int(guild),
    id.channel_id_to_int(channel),
    id.user_id_to_int(user),
  )
  let fields = [
    #("guild_id", id.guild_json(guild)),
    #("channel_id", id.channel_json(channel)),
    #("user_id", id.user_json(user)),
    #("mute", json.bool(mute)),
    #("deaf", json.bool(deaf)),
    #("can_speak", json.bool(perms.can_speak)),
    #("can_stream", json.bool(perms.can_stream)),
    #("can_video", json.bool(perms.can_video)),
  ]
  process.send(process.named_subject(name()), Enqueue(key, fields))
}

fn handle(queue: Queue, message: Message) -> actor.Next(Queue, Message) {
  case message {
    Enqueue(key, fields) ->
      case dict.has_key(queue.inflight, key) {
        True ->
          actor.continue(
            Queue(
              ..queue,
              inflight: dict.insert(queue.inflight, key, Some(fields)),
            ),
          )
        False -> actor.continue(start_worker(queue, key, fields, 1))
      }
    Finished(worker, key, fields, attempt, ok) -> {
      let queue = case dict.get(queue.workers, worker) {
        Ok(found) -> {
          process.demonitor_process(found.monitor)
          Queue(..queue, workers: dict.delete(queue.workers, worker))
        }
        Error(Nil) -> queue
      }
      actor.continue(finish(queue, key, fields, attempt, ok))
    }
    WorkerDown(process.ProcessDown(pid:, ..)) ->
      case dict.get(queue.workers, pid) {
        Ok(found) ->
          Queue(..queue, workers: dict.delete(queue.workers, pid))
          |> finish(found.key, found.fields, found.attempt, False)
          |> actor.continue
        Error(Nil) -> actor.continue(queue)
      }
    WorkerDown(process.PortDown(..)) -> actor.continue(queue)
    Retry(key, fields, attempt) ->
      case dict.get(queue.inflight, key) {
        Ok(None) -> actor.continue(start_worker(queue, key, fields, attempt))
        Ok(Some(next)) -> actor.continue(start_worker(queue, key, next, 1))
        Error(Nil) -> actor.continue(queue)
      }
  }
}

fn finish(
  queue: Queue,
  key: Key,
  fields: List(#(String, Json)),
  attempt: Int,
  ok: Bool,
) -> Queue {
  case ok, dict.get(queue.inflight, key) {
    False, Ok(None) if attempt < max_attempts -> {
      let delay =
        backoff.delay_ms(
          attempt,
          retry_base_ms(),
          retry_max_ms(),
          retry_jitter_ms(),
        )
      process.send_after(queue.self, delay, Retry(key, fields, attempt + 1))
      queue
    }
    _, Ok(Some(next)) -> start_worker(queue, key, next, 1)
    _, _ -> Queue(..queue, inflight: dict.delete(queue.inflight, key))
  }
}

fn start_worker(
  queue: Queue,
  key: Key,
  fields: List(#(String, Json)),
  attempt: Int,
) -> Queue {
  let self = queue.self
  let #(worker, monitor) =
    spawn.monitored(fn() {
      let ok =
        token.api_send("voice_update_participant", fields) |> result.is_ok
      process.send(self, Finished(process.self(), key, fields, attempt, ok))
    })
  Queue(
    ..queue,
    inflight: dict.insert(queue.inflight, key, None),
    workers: dict.insert(
      queue.workers,
      worker,
      Worker(monitor:, key:, fields:, attempt:),
    ),
  )
}
