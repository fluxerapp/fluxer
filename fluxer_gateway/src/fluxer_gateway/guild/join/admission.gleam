// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{
  type JoinClass, type JoinReply, type JoinRequest, Background, Focused,
  JoinOverloaded, Resume,
}
import fluxer_gateway/core/session.{type SessionId}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

fn default_codel_interval_ms() -> Int {
  durations.guild().join_admission_codel_interval_ms
}

fn default_codel_target_ms() -> Int {
  durations.guild().join_admission_codel_target_ms
}

fn default_standing_max_age_ms() -> Int {
  durations.guild().join_admission_standing_max_age_ms
}

fn default_rate_sample_ms() -> Int {
  durations.guild().join_admission_rate_sample_ms
}

fn default_retry_min_ms() -> Int {
  durations.guild().join_admission_retry_min_ms
}

fn default_retry_max_ms() -> Int {
  durations.guild().join_admission_retry_max_ms
}

fn default_baseline_window_ms() -> Int {
  durations.guild().join_admission_baseline_window_ms
}

fn default_transit_ms() -> Int {
  durations.guild().join_admission_transit_ms
}

pub type Config {
  Config(
    batch_size: Int,
    initial_limit: Int,
    min_limit: Int,
    max_limit: Int,
    queue_factor: Int,
    background_every: Int,
    codel_interval_ms: Int,
    codel_target_ms: Int,
    standing_intervals: Int,
    standing_max_age_ms: Int,
    shed_per_step: Int,
    rate_sample_ms: Int,
    rate_floor_per_s: Int,
    retry_min_ms: Int,
    retry_max_ms: Int,
    retry_jitter_percent: Int,
    baseline_window_ms: Int,
    transit_ms: Int,
  )
}

pub fn default_config() -> Config {
  Config(
    batch_size: 64,
    initial_limit: 8,
    min_limit: 2,
    max_limit: 64,
    queue_factor: 4,
    background_every: 4,
    codel_interval_ms: default_codel_interval_ms(),
    codel_target_ms: default_codel_target_ms(),
    standing_intervals: 2,
    standing_max_age_ms: default_standing_max_age_ms(),
    shed_per_step: 64,
    rate_sample_ms: default_rate_sample_ms(),
    rate_floor_per_s: 500,
    retry_min_ms: default_retry_min_ms(),
    retry_max_ms: default_retry_max_ms(),
    retry_jitter_percent: 10,
    baseline_window_ms: default_baseline_window_ms(),
    transit_ms: default_transit_ms(),
  )
}

pub type DropReason {
  Expired
  Stale
}

pub type Effect {
  Reply(to: Subject(JoinReply), reply: JoinReply)
  Dropped(request: JoinRequest, reason: DropReason)
}

pub type Waiting {
  Waiting(
    request: JoinRequest,
    reply: Subject(JoinReply),
    deadline_ms: Int,
    enqueued_at: Int,
  )
}

pub type Batch {
  Batch(id: Int, class: JoinClass, members: List(Waiting), started_at: Int)
}

type Lane {
  Lane(front: List(Waiting), back: List(Waiting), stack: List(Waiting))
}

type Latest {
  Latest(attempt: Int, waiting: Int)
}

pub opaque type Gate {
  Gate(
    config: Config,
    resume: Lane,
    focused: Lane,
    background: Lane,
    queued: Int,
    latest: Dict(String, Latest),
    running: Dict(Int, Int),
    next_batch_id: Int,
    limit: Int,
    higher_batches: Int,
    window_min: Option(Int),
    previous_window_min: Option(Int),
    window_start: Int,
    interval_start: Int,
    interval_min: Option(Int),
    above_intervals: Int,
    standing: Bool,
    sample_start: Int,
    served_since_sample: Int,
    rate_per_s: Int,
    slot_cursor_us: Int,
    jitter: fn(Int, Int) -> Int,
  )
}

const empty_lane = Lane([], [], [])

pub fn new(config: Config, now_ms: Int) -> Gate {
  with_jitter(config, now_ms, random_jitter)
}

pub fn with_jitter(
  config: Config,
  now_ms: Int,
  jitter: fn(Int, Int) -> Int,
) -> Gate {
  Gate(
    config:,
    resume: empty_lane,
    focused: empty_lane,
    background: empty_lane,
    queued: 0,
    latest: dict.new(),
    running: dict.new(),
    next_batch_id: 1,
    limit: config.initial_limit,
    higher_batches: 0,
    window_min: None,
    previous_window_min: None,
    window_start: now_ms,
    interval_start: now_ms,
    interval_min: None,
    above_intervals: 0,
    standing: False,
    sample_start: now_ms,
    served_since_sample: 0,
    rate_per_s: 0,
    slot_cursor_us: 0,
    jitter:,
  )
}

fn random_jitter(value: Int, percent: Int) -> Int {
  let spread = value * percent / 100
  case spread > 0 {
    True -> value - spread + int.random(2 * spread + 1)
    False -> value
  }
}

pub fn queued(gate: Gate) -> Int {
  gate.queued
}

pub fn limit(gate: Gate) -> Int {
  gate.limit
}

pub fn hard_cap(gate: Gate) -> Int {
  gate.limit * gate.config.batch_size * gate.config.queue_factor
}

fn session_key(request: JoinRequest) -> String {
  session.session_id_to_string(request.session.id)
}

pub fn offer(
  gate: Gate,
  request: JoinRequest,
  reply: Subject(JoinReply),
  now_ms: Int,
) -> #(Gate, List(Effect)) {
  let gate = advance(gate, now_ms)
  let deadline_ms = now_ms + request.remaining_ms - gate.config.transit_ms
  case deadline_ms <= now_ms {
    True -> drop(gate, request, reply, Expired, now_ms)
    False ->
      case gate.queued >= hard_cap(gate) {
        True -> {
          let gate = note_attempt(gate, request, 0)
          let #(gate, effect) = reject(gate, request, reply, now_ms)
          #(gate, [effect])
        }
        False -> {
          let gate = note_attempt(gate, request, 1)
          let waiting = Waiting(request, reply, deadline_ms, now_ms)
          let lane = push(lane_of(gate, request.class), waiting, gate.standing)
          let gate =
            Gate(..put_lane(gate, request.class, lane), queued: gate.queued + 1)
          shed_aged(gate, now_ms)
        }
      }
  }
}

pub fn withdraw(gate: Gate, session: SessionId, owner: Pid) -> Gate {
  [Resume, Focused, Background]
  |> list.fold(gate, fn(acc, class) {
    let lane = lane_of(acc, class)
    let #(gone, stack) = list.partition(lane.stack, owned_by(_, session, owner))
    let #(gone_front, front) =
      list.partition(lane.front, owned_by(_, session, owner))
    let #(gone_back, back) =
      list.partition(lane.back, owned_by(_, session, owner))
    let removed = list.flatten([gone, gone_front, gone_back])
    let acc =
      Gate(
        ..put_lane(acc, class, Lane(front:, back:, stack:)),
        queued: acc.queued - list.length(removed),
      )
    list.fold(removed, acc, fn(inner, waiting) {
      release_attempt(inner, waiting.request)
    })
  })
}

fn owned_by(waiting: Waiting, session: SessionId, owner: Pid) -> Bool {
  waiting.request.session.id == session
  && process.subject_owner(waiting.request.session.inbox) == Ok(owner)
}

pub fn next_batch(
  gate: Gate,
  now_ms: Int,
) -> #(Gate, Option(Batch), List(Effect)) {
  let gate = advance(gate, now_ms)
  case dict.size(gate.running) >= gate.limit {
    True -> #(gate, None, [])
    False -> fill(gate, class_order(gate), now_ms, [])
  }
}

pub fn deliverable(waiting: Waiting, now_ms: Int) -> Bool {
  waiting.deadline_ms > now_ms
}

pub fn expire(
  gate: Gate,
  waiting: Waiting,
  now_ms: Int,
) -> #(Gate, List(Effect)) {
  drop(gate, waiting.request, waiting.reply, Expired, now_ms)
}

fn drop(
  gate: Gate,
  request: JoinRequest,
  reply: Subject(JoinReply),
  reason: DropReason,
  now_ms: Int,
) -> #(Gate, List(Effect)) {
  case reason {
    Stale -> #(gate, [
      Dropped(request, Stale),
      Reply(reply, JoinOverloaded(gate.config.retry_min_ms, request.attempt)),
    ])
    Expired -> {
      let #(gate, effect) = reject(gate, request, reply, now_ms)
      #(gate, [Dropped(request, Expired), effect])
    }
  }
}

pub fn batch_done(gate: Gate, batch: Batch, now_ms: Int) -> Gate {
  let latency = int.max(now_ms - batch.started_at, 1)
  let gate = roll_window(gate, now_ms)
  let window_min = Some(min_option(gate.window_min, latency))
  let base =
    min_of(window_min, gate.previous_window_min) |> option.unwrap(latency)
  let config = gate.config
  let limit = case latency * 10 <= base * 11 {
    True -> gate.limit + 1
    False -> gate.limit * base / latency
  }
  Gate(
    ..gate,
    running: dict.delete(gate.running, batch.id),
    window_min:,
    limit: int.clamp(limit, config.min_limit, config.max_limit),
  )
}

fn class_order(gate: Gate) -> List(JoinClass) {
  case gate.higher_batches >= gate.config.background_every {
    True -> [Background, Resume, Focused]
    False -> [Resume, Focused, Background]
  }
}

fn fill(
  gate: Gate,
  classes: List(JoinClass),
  now_ms: Int,
  effects: List(Effect),
) -> #(Gate, Option(Batch), List(Effect)) {
  case classes {
    [] -> #(gate, None, list.reverse(effects))
    [class, ..rest] -> {
      let #(gate, members, skips_left, effects) =
        take(
          gate,
          class,
          gate.config.batch_size,
          gate.config.shed_per_step,
          now_ms,
          [],
          effects,
        )
      case members {
        [] ->
          case
            skips_left <= 0
            && skips_left < gate.config.shed_per_step
            && !lane_empty(lane_of(gate, class))
          {
            True -> fill(gate, classes, now_ms, effects)
            False -> fill(gate, rest, now_ms, effects)
          }
        _ -> {
          let batch_id = gate.next_batch_id
          let batch = Batch(batch_id, class, list.reverse(members), now_ms)
          let higher = case class {
            Background -> 0
            Resume | Focused -> gate.higher_batches + 1
          }
          let gate =
            Gate(
              ..gate,
              next_batch_id: batch_id + 1,
              running: dict.insert(gate.running, batch_id, now_ms),
              higher_batches: higher,
            )
          #(gate, Some(batch), list.reverse(effects))
        }
      }
    }
  }
}

type Check {
  Keep
  Drop(DropReason)
  Shed
}

fn take(
  gate: Gate,
  class: JoinClass,
  left: Int,
  skips: Int,
  now_ms: Int,
  members: List(Waiting),
  effects: List(Effect),
) -> #(Gate, List(Waiting), Int, List(Effect)) {
  case left <= 0 || skips <= 0 {
    True -> #(gate, members, skips, effects)
    False ->
      case pop(lane_of(gate, class)) {
        Error(Nil) -> #(gate, members, skips, effects)
        Ok(#(waiting, lane)) -> {
          let gate =
            Gate(..put_lane(gate, class, lane), queued: gate.queued - 1)
          let outcome = check(gate, waiting, now_ms)
          let gate = release_attempt(gate, waiting.request)
          case outcome {
            Keep -> {
              let gate = note_sojourn(gate, now_ms - waiting.enqueued_at)
              let gate =
                Gate(..gate, served_since_sample: gate.served_since_sample + 1)
              take(
                gate,
                class,
                left - 1,
                skips,
                now_ms,
                [waiting, ..members],
                effects,
              )
            }
            Drop(reason) -> {
              let #(gate, dropped) =
                drop(gate, waiting.request, waiting.reply, reason, now_ms)
              take(
                gate,
                class,
                left,
                skips - 1,
                now_ms,
                members,
                list.append(list.reverse(dropped), effects),
              )
            }
            Shed -> {
              let #(gate, effect) =
                reject(gate, waiting.request, waiting.reply, now_ms)
              take(gate, class, left, skips - 1, now_ms, members, [
                effect,
                ..effects
              ])
            }
          }
        }
      }
  }
}

fn check(gate: Gate, waiting: Waiting, now_ms: Int) -> Check {
  let expired = waiting.deadline_ms <= now_ms
  let stale =
    waiting.request.attempt < latest_attempt(gate, session_key(waiting.request))
  let aged =
    gate.standing
    && now_ms - waiting.enqueued_at > gate.config.standing_max_age_ms
  case expired, stale, aged {
    True, _, _ -> Drop(Expired)
    False, True, _ -> Drop(Stale)
    False, False, True -> Shed
    False, False, False -> Keep
  }
}

fn shed_aged(gate: Gate, now_ms: Int) -> #(Gate, List(Effect)) {
  case gate.standing {
    False -> #(gate, [])
    True ->
      shed_lanes(
        gate,
        [Resume, Focused, Background],
        now_ms,
        [],
        gate.config.shed_per_step,
      )
  }
}

fn shed_lanes(
  gate: Gate,
  classes: List(JoinClass),
  now_ms: Int,
  effects: List(Effect),
  budget: Int,
) -> #(Gate, List(Effect)) {
  case classes, budget <= 0 {
    [], _ | _, True -> #(gate, list.reverse(effects))
    [class, ..rest], False -> {
      let lane = lane_of(gate, class)
      case lane.front {
        [head, ..tail]
          if now_ms - head.enqueued_at > gate.config.standing_max_age_ms
        -> {
          let gate =
            Gate(
              ..put_lane(gate, class, Lane(..lane, front: tail)),
              queued: gate.queued - 1,
            )
          let outcome = check(gate, head, now_ms)
          let gate = release_attempt(gate, head.request)
          let #(gate, dropped) = case outcome {
            Drop(reason) -> drop(gate, head.request, head.reply, reason, now_ms)
            _ -> {
              let #(gate, effect) =
                reject(gate, head.request, head.reply, now_ms)
              #(gate, [effect])
            }
          }
          shed_lanes(
            gate,
            classes,
            now_ms,
            list.append(list.reverse(dropped), effects),
            budget - 1,
          )
        }
        _ -> shed_lanes(gate, rest, now_ms, effects, budget)
      }
    }
  }
}

fn reject(
  gate: Gate,
  request: JoinRequest,
  reply: Subject(JoinReply),
  now_ms: Int,
) -> #(Gate, Effect) {
  let config = gate.config
  let rate = int.max(gate.rate_per_s, config.rate_floor_per_s)
  let now_us = now_ms * 1000
  let next_us = int.max(gate.slot_cursor_us, now_us) + 1_000_000 / rate
  let raw = { next_us - now_us } / 1000
  let retry_after =
    int.clamp(
      gate.jitter(raw, config.retry_jitter_percent),
      config.retry_min_ms,
      config.retry_max_ms,
    )
  #(
    Gate(..gate, slot_cursor_us: next_us),
    Reply(reply, JoinOverloaded(retry_after, request.attempt)),
  )
}

fn advance(gate: Gate, now_ms: Int) -> Gate {
  gate
  |> roll_interval(now_ms)
  |> roll_rate(now_ms)
}

fn roll_interval(gate: Gate, now_ms: Int) -> Gate {
  let config = gate.config
  case now_ms - gate.interval_start >= config.codel_interval_ms {
    False -> gate
    True -> {
      let above = case gate.interval_min {
        Some(sojourn) -> sojourn > config.codel_target_ms
        None -> gate.queued > 0
      }
      let above_intervals = case above {
        True -> gate.above_intervals + 1
        False -> 0
      }
      Gate(
        ..gate,
        interval_start: now_ms,
        interval_min: None,
        above_intervals:,
        standing: above_intervals >= config.standing_intervals,
      )
    }
  }
}

fn roll_rate(gate: Gate, now_ms: Int) -> Gate {
  let elapsed = now_ms - gate.sample_start
  case elapsed >= gate.config.rate_sample_ms && elapsed > 0 {
    False -> gate
    True -> {
      let measured = gate.served_since_sample * 1000 / elapsed
      Gate(
        ..gate,
        sample_start: now_ms,
        rate_per_s: case gate.queued > 0 {
          True -> measured
          False -> int.max(gate.rate_per_s, measured)
        },
        served_since_sample: 0,
      )
    }
  }
}

fn roll_window(gate: Gate, now_ms: Int) -> Gate {
  case now_ms - gate.window_start >= gate.config.baseline_window_ms {
    False -> gate
    True ->
      Gate(
        ..gate,
        window_start: now_ms,
        previous_window_min: gate.window_min,
        window_min: None,
      )
  }
}

fn note_sojourn(gate: Gate, sojourn: Int) -> Gate {
  Gate(..gate, interval_min: Some(min_option(gate.interval_min, sojourn)))
}

fn min_option(current: Option(Int), value: Int) -> Int {
  current |> option.map(int.min(_, value)) |> option.unwrap(value)
}

fn min_of(a: Option(Int), b: Option(Int)) -> Option(Int) {
  case a, b {
    Some(x), Some(y) -> Some(int.min(x, y))
    Some(_), None -> a
    None, _ -> b
  }
}

fn latest_attempt(gate: Gate, key: String) -> Int {
  case dict.get(gate.latest, key) {
    Ok(latest) -> latest.attempt
    Error(Nil) -> 0
  }
}

fn note_attempt(gate: Gate, request: JoinRequest, added: Int) -> Gate {
  let key = session_key(request)
  case dict.get(gate.latest, key), added {
    Ok(latest), _ ->
      Gate(
        ..gate,
        latest: dict.insert(
          gate.latest,
          key,
          Latest(
            attempt: int.max(request.attempt, latest.attempt),
            waiting: latest.waiting + added,
          ),
        ),
      )
    Error(Nil), 0 -> gate
    Error(Nil), _ ->
      Gate(
        ..gate,
        latest: dict.insert(
          gate.latest,
          key,
          Latest(attempt: request.attempt, waiting: added),
        ),
      )
  }
}

fn release_attempt(gate: Gate, request: JoinRequest) -> Gate {
  let key = session_key(request)
  case dict.get(gate.latest, key) {
    Ok(Latest(waiting:, ..)) if waiting <= 1 ->
      Gate(..gate, latest: dict.delete(gate.latest, key))
    Ok(latest) ->
      Gate(
        ..gate,
        latest: dict.insert(
          gate.latest,
          key,
          Latest(..latest, waiting: latest.waiting - 1),
        ),
      )
    Error(Nil) -> gate
  }
}

fn lane_of(gate: Gate, class: JoinClass) -> Lane {
  case class {
    Resume -> gate.resume
    Focused -> gate.focused
    Background -> gate.background
  }
}

fn put_lane(gate: Gate, class: JoinClass, lane: Lane) -> Gate {
  case class {
    Resume -> Gate(..gate, resume: lane)
    Focused -> Gate(..gate, focused: lane)
    Background -> Gate(..gate, background: lane)
  }
}

fn push(lane: Lane, waiting: Waiting, lifo: Bool) -> Lane {
  case lifo {
    True -> Lane(..lane, stack: [waiting, ..lane.stack])
    False -> Lane(..lane, back: [waiting, ..lane.back])
  }
}

fn lane_empty(lane: Lane) -> Bool {
  lane.stack == [] && lane.front == [] && lane.back == []
}

fn pop(lane: Lane) -> Result(#(Waiting, Lane), Nil) {
  case lane.stack, lane.front, lane.back {
    [top, ..rest], _, _ -> Ok(#(top, Lane(..lane, stack: rest)))
    [], [first, ..rest], _ -> Ok(#(first, Lane(..lane, front: rest)))
    [], [], [] -> Error(Nil)
    [], [], back ->
      case list.reverse(back) {
        [first, ..rest] -> Ok(#(first, Lane(front: rest, back: [], stack: [])))
        [] -> Error(Nil)
      }
  }
}
