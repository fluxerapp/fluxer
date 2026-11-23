// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key
import fluxer_gateway/config/durations
import fluxer_gateway/push/job.{type Job, type Kind}
import fluxer_gateway/push/rule.{type Class}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}

fn default_slot_deadline_ms() -> Int {
  durations.push().outbox_state_slot_deadline_ms
}

fn default_bulk_age_ms() -> Int {
  durations.push().outbox_state_bulk_age_ms
}

fn default_direct_age_ms() -> Int {
  durations.push().outbox_state_direct_age_ms
}

fn default_retry_base_ms() -> Int {
  durations.push().outbox_state_retry_base_ms
}

fn default_retry_max_ms() -> Int {
  durations.push().outbox_state_retry_max_ms
}

fn default_in_flight_floor_ms() -> Int {
  durations.push().outbox_state_in_flight_floor_ms
}

fn default_reads_max_age_ms() -> Int {
  durations.push().outbox_state_reads_max_age_ms
}

fn default_drain_window_ms() -> Int {
  durations.push().outbox_state_drain_window_ms
}

pub type Limits {
  Limits(
    max_records: Int,
    slots: Int,
    bulk_slots: Int,
    slot_deadline_ms: Int,
    request_timeout_ms: Int,
    bulk_age_ms: Int,
    direct_age_ms: Int,
    retry_base_ms: Int,
    retry_max_ms: Int,
    in_flight_floor_ms: Int,
    reads_max_age_ms: Int,
    drain_window_ms: Int,
  )
}

pub fn default_limits(request_timeout_ms: Int) -> Limits {
  Limits(
    max_records: 10_000,
    slots: 64,
    bulk_slots: 56,
    slot_deadline_ms: default_slot_deadline_ms(),
    request_timeout_ms:,
    bulk_age_ms: default_bulk_age_ms(),
    direct_age_ms: default_direct_age_ms(),
    retry_base_ms: default_retry_base_ms(),
    retry_max_ms: default_retry_max_ms(),
    in_flight_floor_ms: default_in_flight_floor_ms(),
    reads_max_age_ms: default_reads_max_age_ms(),
    drain_window_ms: default_drain_window_ms(),
  )
}

pub type SendResult {
  Delivered
  InFlightElsewhere
  Failed(reason: String)
}

pub type Effect {
  Start(attempt: Int, subject: String, body: BitArray, timeout_ms: Int)
  Kill(attempt: Int)
  ArmSlotDeadline(attempt: Int, after_ms: Int)
  ArmWindowEnd(attempt: Int, after_ms: Int)
  ArmRetry(seq: Int, after_ms: Int)
  Count(name: String, class: Class, by: Int)
  Drop(kind: Kind, class: Class, reason: String)
  HandOver(to: Target, carried: List(Carried))
}

pub type Target {
  Target(node: String, outbox: String)
}

pub type Carried {
  Carried(job: Job, age_ms: Int, attempts: Int, retry_in_ms: Int)
}

pub type ReadMark {
  ReadMark(user: Int, channel: Int, message: Int, age_ms: Int)
}

type Leaving {
  Holding(held: List(Carried))
  Handed(to: Target)
}

pub type Record {
  Record(seq: Int, ordinal: Int, job: Job, admitted_at_ms: Int, attempts: Int)
}

type AttemptStatus {
  InSlot
  Unknown
}

type Attempt {
  Attempt(
    record: Record,
    status: AttemptStatus,
    started_at_ms: Int,
    ends_at_ms: Int,
  )
}

type Queue {
  Queue(front: List(Int), back: List(Int))
}

pub opaque type Outbox {
  Outbox(
    limits: Limits,
    records: Dict(Int, Record),
    direct: Queue,
    bulk: Queue,
    by_channel: Dict(Int, Set(Int)),
    attempts: Dict(Int, Attempt),
    followups: Dict(Int, Dict(Int, Int)),
    reads: Dict(#(Int, Int), #(Int, Int)),
    active: Dict(Int, #(Int, Int)),
    next_seq: Int,
    next_ordinal: Int,
    next_attempt: Int,
    clears_enabled: Bool,
    leaving: Dict(Int, Leaving),
    all_to: Option(Target),
  )
}

pub type Stats {
  Stats(
    depth_direct: Int,
    depth_bulk: Int,
    inflight_direct: Int,
    inflight_bulk: Int,
    unknown_direct: Int,
    unknown_bulk: Int,
  )
}

pub fn new(limits: Limits, clears_enabled: Bool) -> Outbox {
  Outbox(
    limits:,
    records: dict.new(),
    direct: Queue([], []),
    bulk: Queue([], []),
    by_channel: dict.new(),
    attempts: dict.new(),
    followups: dict.new(),
    reads: dict.new(),
    active: dict.new(),
    next_seq: 0,
    next_ordinal: 0,
    next_attempt: 1,
    clears_enabled:,
    leaving: dict.new(),
    all_to: None,
  )
}

pub fn stats(outbox: Outbox) -> Stats {
  let #(depth_direct, depth_bulk) =
    dict.fold(outbox.records, #(0, 0), fn(acc, _, record) {
      case record.job.class {
        rule.Direct -> #(acc.0 + 1, acc.1)
        rule.Bulk -> #(acc.0, acc.1 + 1)
      }
    })
  let counts =
    dict.fold(outbox.attempts, #(0, 0, 0, 0), fn(acc, _, attempt) {
      case attempt.status, attempt.record.job.class {
        InSlot, rule.Direct -> #(acc.0 + 1, acc.1, acc.2, acc.3)
        InSlot, rule.Bulk -> #(acc.0, acc.1 + 1, acc.2, acc.3)
        Unknown, rule.Direct -> #(acc.0, acc.1, acc.2 + 1, acc.3)
        Unknown, rule.Bulk -> #(acc.0, acc.1, acc.2, acc.3 + 1)
      }
    })
  Stats(
    depth_direct:,
    depth_bulk:,
    inflight_direct: counts.0,
    inflight_bulk: counts.1,
    unknown_direct: counts.2,
    unknown_bulk: counts.3,
  )
}

fn push_queue(queue: Queue, seq: Int) -> Queue {
  Queue(..queue, back: [seq, ..queue.back])
}

fn pop_queue(queue: Queue) -> Result(#(Int, Queue), Nil) {
  case queue.front {
    [first, ..rest] -> Ok(#(first, Queue(..queue, front: rest)))
    [] ->
      case list.reverse(queue.back) {
        [] -> Error(Nil)
        [first, ..rest] -> Ok(#(first, Queue(front: rest, back: [])))
      }
  }
}

fn slots_used(outbox: Outbox) -> Int {
  dict.fold(outbox.attempts, 0, fn(acc, _, attempt) {
    case attempt.status {
      InSlot -> acc + 1
      Unknown -> acc
    }
  })
}

pub fn enqueue(
  outbox: Outbox,
  job: Job,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let #(outbox, effects) = admit(outbox, job, now_ms)
  let #(outbox, pumped) = pump(outbox, now_ms)
  #(outbox, list.append(effects, pumped))
}

fn admit(outbox: Outbox, job: Job, now_ms: Int) -> #(Outbox, List(Effect)) {
  let record =
    Record(
      seq: outbox.next_seq,
      ordinal: outbox.next_ordinal,
      job:,
      admitted_at_ms: now_ms,
      attempts: 0,
    )
  let outbox =
    Outbox(
      ..outbox,
      next_seq: outbox.next_seq + 1,
      next_ordinal: outbox.next_ordinal + 1,
    )
  case divert(outbox, carried(record, now_ms, 0)) {
    Diverted(outbox, effects) -> #(outbox, [
      Count("enqueued_total", job.class, 1),
      ..effects
    ])
    Kept -> {
      let outbox = insert_ready(outbox, record)
      let #(outbox, shed) = shed_to_capacity(outbox, [])
      #(outbox, [Count("enqueued_total", job.class, 1), ..shed])
    }
  }
}

type Diversion {
  Diverted(Outbox, List(Effect))
  Kept
}

pub fn bucket_of(found: Job) -> Int {
  key.bucket(found.channel)
}

fn divert(outbox: Outbox, found: Carried) -> Diversion {
  let bucket = bucket_of(found.job)
  case outbox.all_to, dict.get(outbox.leaving, bucket) {
    Some(to), _ | None, Ok(Handed(to)) ->
      Diverted(outbox, [HandOver(to, [found])])
    None, Ok(Holding(held)) ->
      Diverted(
        Outbox(
          ..outbox,
          leaving: dict.insert(outbox.leaving, bucket, Holding([found, ..held])),
        ),
        [],
      )
    None, Error(Nil) -> Kept
  }
}

fn carried(record: Record, now_ms: Int, retry_in_ms: Int) -> Carried {
  Carried(
    job: record.job,
    age_ms: int.max(0, now_ms - record.admitted_at_ms),
    attempts: record.attempts,
    retry_in_ms:,
  )
}

fn insert_ready(outbox: Outbox, record: Record) -> Outbox {
  let outbox = insert_record(outbox, record)
  case record.job.class {
    rule.Direct ->
      Outbox(..outbox, direct: push_queue(outbox.direct, record.seq))
    rule.Bulk -> Outbox(..outbox, bulk: push_queue(outbox.bulk, record.seq))
  }
}

fn insert_record(outbox: Outbox, record: Record) -> Outbox {
  let records = dict.insert(outbox.records, record.seq, record)
  let by_channel = case record.job.kind {
    job.MessageJob ->
      dict.upsert(outbox.by_channel, record.job.channel, fn(existing) {
        set.insert(option.unwrap(existing, set.new()), record.seq)
      })
    job.ClearJob | job.RingJob -> outbox.by_channel
  }
  Outbox(..outbox, records:, by_channel:)
}

fn remove_record(outbox: Outbox, record: Record) -> Outbox {
  let records = dict.delete(outbox.records, record.seq)
  let by_channel = case record.job.kind {
    job.MessageJob ->
      case dict.get(outbox.by_channel, record.job.channel) {
        Ok(seqs) -> {
          let seqs = set.delete(seqs, record.seq)
          case set.is_empty(seqs) {
            True -> dict.delete(outbox.by_channel, record.job.channel)
            False -> dict.insert(outbox.by_channel, record.job.channel, seqs)
          }
        }
        Error(Nil) -> outbox.by_channel
      }
    job.ClearJob | job.RingJob -> outbox.by_channel
  }
  Outbox(..outbox, records:, by_channel:)
}

type ShedRank {
  ShedRank(rank: Int, seq: Int)
}

fn shed_rank(record: Record) -> ShedRank {
  let rank = case record.job.kind, record.job.class {
    job.MessageJob, rule.Bulk -> 0
    job.ClearJob, _ -> 2
    _, _ -> 1
  }
  ShedRank(rank:, seq: record.seq)
}

fn shed_to_capacity(
  outbox: Outbox,
  effects: List(Effect),
) -> #(Outbox, List(Effect)) {
  case dict.size(outbox.records) > outbox.limits.max_records {
    False -> #(outbox, effects)
    True -> {
      let victim =
        dict.fold(outbox.records, None, fn(best, _, record) {
          let rank = shed_rank(record)
          case best {
            None -> Some(#(rank, record))
            Some(#(best_rank, _)) ->
              case compare_rank(rank, best_rank) {
                order.Lt -> Some(#(rank, record))
                _ -> best
              }
          }
        })
      case victim {
        None -> #(outbox, effects)
        Some(#(_, record)) ->
          shed_to_capacity(remove_record(outbox, record), [
            Drop(record.job.kind, record.job.class, "shed"),
            Count("sheds_total", record.job.class, 1),
            ..effects
          ])
      }
    }
  }
}

fn compare_rank(a: ShedRank, b: ShedRank) -> order.Order {
  case int.compare(a.rank, b.rank) {
    order.Eq -> int.compare(a.seq, b.seq)
    other -> other
  }
}

pub fn pump(outbox: Outbox, now_ms: Int) -> #(Outbox, List(Effect)) {
  pump_loop(outbox, now_ms, [])
}

fn pump_loop(
  outbox: Outbox,
  now_ms: Int,
  effects: List(Effect),
) -> #(Outbox, List(Effect)) {
  let used = slots_used(outbox)
  case used < outbox.limits.slots {
    False -> #(outbox, effects)
    True ->
      case take_ready(outbox, rule.Direct) {
        Ok(#(record, outbox)) -> {
          let #(outbox, started) = dispatch(outbox, record, now_ms)
          pump_loop(outbox, now_ms, list.append(effects, started))
        }
        Error(Nil) ->
          case used < outbox.limits.bulk_slots {
            False -> #(outbox, effects)
            True ->
              case take_ready(outbox, rule.Bulk) {
                Ok(#(record, outbox)) -> {
                  let #(outbox, started) = dispatch(outbox, record, now_ms)
                  pump_loop(outbox, now_ms, list.append(effects, started))
                }
                Error(Nil) -> #(outbox, effects)
              }
          }
      }
  }
}

fn take_ready(outbox: Outbox, class: Class) -> Result(#(Record, Outbox), Nil) {
  let queue = case class {
    rule.Direct -> outbox.direct
    rule.Bulk -> outbox.bulk
  }
  use #(seq, rest) <- result.try(pop_queue(queue))
  let outbox = case class {
    rule.Direct -> Outbox(..outbox, direct: rest)
    rule.Bulk -> Outbox(..outbox, bulk: rest)
  }
  case dict.get(outbox.records, seq) {
    Ok(record) -> Ok(#(record, remove_record(outbox, record)))
    Error(Nil) -> take_ready(outbox, class)
  }
}

type Prepared {
  Skip
  Send(Record)
}

fn prepare(outbox: Outbox, record: Record) -> #(Prepared, List(Effect)) {
  case record.job.kind {
    job.ClearJob | job.RingJob -> #(Send(record), [])
    job.MessageJob -> {
      let #(read, unread) =
        list.partition(record.job.users, fn(user) {
          is_read(outbox, user, record.job.channel, record.job.message)
        })
      let #(activated, kept) =
        list.partition(unread, fn(user) {
          became_active(outbox, user, record.seq)
        })
      let counts = [
        Count("truncations_total", record.job.class, list.length(read)),
        Count("skipped_active_total", record.job.class, list.length(activated)),
      ]
      case kept {
        [] -> #(Skip, counts)
        _ ->
          case read, activated {
            [], [] -> #(Send(record), counts)
            _, _ -> #(
              Send(Record(..record, job: job.with_users(record.job, kept))),
              counts,
            )
          }
      }
    }
  }
}

fn is_read(outbox: Outbox, user: Int, channel: Int, message: Int) -> Bool {
  case dict.get(outbox.reads, #(user, channel)) {
    Ok(#(read, _)) -> message <= read
    Error(Nil) -> False
  }
}

fn became_active(outbox: Outbox, user: Int, seq: Int) -> Bool {
  case dict.get(outbox.active, user) {
    Ok(#(active_seq, _)) -> active_seq > seq
    Error(Nil) -> False
  }
}

fn age_limit(outbox: Outbox, class: Class) -> Int {
  case class {
    rule.Direct -> outbox.limits.direct_age_ms
    rule.Bulk -> outbox.limits.bulk_age_ms
  }
}

fn is_expired(outbox: Outbox, record: Record, now_ms: Int) -> Bool {
  now_ms - record.admitted_at_ms >= age_limit(outbox, record.job.class)
}

fn dispatch(
  outbox: Outbox,
  record: Record,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case prepare(outbox, record) {
    #(Skip, counts) -> #(outbox, counts)
    #(Send(record), counts) ->
      case is_expired(outbox, record, now_ms) {
        True -> #(outbox, [
          Drop(record.job.kind, record.job.class, "expired"),
          ..counts
        ])
        False -> {
          let #(outbox, started) = start_attempt(outbox, record, now_ms)
          #(outbox, list.append(counts, started))
        }
      }
  }
}

fn start_attempt(
  outbox: Outbox,
  record: Record,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let id = outbox.next_attempt
  let remaining_age =
    age_limit(outbox, record.job.class) - { now_ms - record.admitted_at_ms }
  let window =
    int.max(1, int.min(outbox.limits.request_timeout_ms, remaining_age))
  let outbox =
    Outbox(
      ..outbox,
      next_attempt: id + 1,
      attempts: dict.insert(
        outbox.attempts,
        id,
        Attempt(record, InSlot, now_ms, now_ms + window),
      ),
    )
  #(outbox, [
    Start(
      attempt: id,
      subject: job.subject(record.job.kind),
      body: job.body(record.job),
      timeout_ms: outbox.limits.request_timeout_ms,
    ),
    ArmSlotDeadline(id, outbox.limits.slot_deadline_ms),
    ArmWindowEnd(id, window),
  ])
}

pub fn slot_deadline(
  outbox: Outbox,
  attempt: Int,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case dict.get(outbox.attempts, attempt) {
    Ok(Attempt(status: InSlot, ..) as found) -> {
      let outbox =
        Outbox(
          ..outbox,
          attempts: dict.insert(
            outbox.attempts,
            attempt,
            Attempt(..found, status: Unknown),
          ),
        )
      let #(outbox, pumped) = pump(outbox, now_ms)
      #(outbox, [Count("unknown_total", found.record.job.class, 1), ..pumped])
    }
    _ -> #(outbox, [])
  }
}

pub fn window_end(
  outbox: Outbox,
  attempt: Int,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case dict.has_key(outbox.attempts, attempt) {
    False -> #(outbox, [])
    True -> {
      let #(outbox, effects) = reply(outbox, attempt, Failed("timeout"), now_ms)
      #(outbox, [Kill(attempt), ..effects])
    }
  }
}

pub fn reply(
  outbox: Outbox,
  attempt: Int,
  result: SendResult,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case dict.get(outbox.attempts, attempt) {
    Error(Nil) -> #(outbox, [])
    Ok(found) -> {
      let late = case found.status {
        Unknown -> [Count("late_replies_total", found.record.job.class, 1)]
        InSlot -> []
      }
      let reads =
        dict.get(outbox.followups, attempt) |> result.lazy_unwrap(dict.new)
      let outbox =
        Outbox(
          ..outbox,
          attempts: dict.delete(outbox.attempts, attempt),
          followups: dict.delete(outbox.followups, attempt),
        )
      let #(outbox, handled) =
        handle_result(outbox, found.record, result, now_ms)
      let #(outbox, followed) =
        follow_up_clears(outbox, found.record, reads, now_ms)
      let #(outbox, pumped) = pump(outbox, now_ms)
      #(outbox, list.flatten([late, handled, followed, pumped]))
    }
  }
}

fn handle_result(
  outbox: Outbox,
  record: Record,
  result: SendResult,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case result {
    Delivered -> #(outbox, [Count("delivered_total", record.job.class, 1)])
    InFlightElsewhere | Failed(_) ->
      case is_expired(outbox, record, now_ms) {
        True ->
          case prepare(outbox, record) {
            #(Skip, counts) -> #(outbox, counts)
            #(Send(record), counts) -> #(outbox, [
              Drop(record.job.kind, record.job.class, "expired"),
              ..counts
            ])
          }
        False -> schedule_retry(outbox, record, result, now_ms)
      }
  }
}

fn retry_delay(limits: Limits, attempts: Int, jitter: fn(Int) -> Int) -> Int {
  let shifted =
    int.bitwise_shift_left(limits.retry_base_ms, int.min(attempts - 1, 16))
  let delay = int.min(limits.retry_max_ms, shifted)
  delay + jitter(int.max(1, delay / 4))
}

fn schedule_retry(
  outbox: Outbox,
  record: Record,
  result: SendResult,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let attempts = record.attempts + 1
  let retried = Record(..record, attempts:)
  let base = retry_delay(outbox.limits, attempts, fn(_) { 0 })
  let floor = case result {
    InFlightElsewhere -> outbox.limits.in_flight_floor_ms
    Delivered | Failed(_) -> 0
  }
  let delay = int.max(floor, base)
  case divert(outbox, carried(retried, now_ms, delay)) {
    Diverted(outbox, effects) -> #(outbox, [
      Count("retries_total", record.job.class, 1),
      ..effects
    ])
    Kept -> arm_retry(outbox, record, retried, delay)
  }
}

fn arm_retry(
  outbox: Outbox,
  record: Record,
  retried: Record,
  delay: Int,
) -> #(Outbox, List(Effect)) {
  let outbox = insert_record(outbox, retried)
  let #(outbox, shed) = shed_to_capacity(outbox, [])
  #(outbox, [
    ArmRetry(record.seq, delay),
    Count("retries_total", record.job.class, 1),
    ..shed
  ])
}

pub fn retry_due(
  outbox: Outbox,
  seq: Int,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case dict.get(outbox.records, seq) {
    Error(Nil) -> #(outbox, [])
    Ok(record) -> {
      let readied = Record(..record, ordinal: outbox.next_ordinal)
      let outbox = Outbox(..outbox, next_ordinal: outbox.next_ordinal + 1)
      let outbox = remove_record(outbox, record)
      let outbox = insert_ready(outbox, readied)
      pump(outbox, now_ms)
    }
  }
}

pub fn truncate_read(
  outbox: Outbox,
  user user: Int,
  channel channel: Int,
  message message: Int,
  now_ms now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let key = #(user, channel)
  let watermark = case dict.get(outbox.reads, key) {
    Ok(#(previous, _)) -> int.max(previous, message)
    Error(Nil) -> message
  }
  let outbox =
    Outbox(
      ..outbox,
      reads: dict.insert(outbox.reads, key, #(watermark, now_ms)),
    )
  let seqs = dict.get(outbox.by_channel, channel) |> result.lazy_unwrap(set.new)
  let #(outbox, effects) =
    set.fold(seqs, #(outbox, []), fn(acc, seq) {
      let #(outbox, effects) = acc
      case dict.get(outbox.records, seq) {
        Ok(record) ->
          case
            record.job.kind == job.MessageJob
            && record.job.message <= message
            && list.contains(record.job.users, user)
          {
            True -> {
              let remaining = list.filter(record.job.users, fn(u) { u != user })
              let effects = [
                Count("truncations_total", record.job.class, 1),
                ..effects
              ]
              case remaining {
                [] -> #(remove_record(outbox, record), effects)
                _ -> #(
                  Outbox(
                    ..outbox,
                    records: dict.insert(
                      outbox.records,
                      seq,
                      Record(
                        ..record,
                        job: job.with_users(record.job, remaining),
                      ),
                    ),
                  ),
                  effects,
                )
              }
            }
            False -> acc
          }
        Error(Nil) -> acc
      }
    })
  let followups =
    dict.fold(outbox.attempts, outbox.followups, fn(followups, id, attempt) {
      let record = attempt.record
      case
        record.job.kind == job.MessageJob
        && record.job.channel == channel
        && record.job.message <= message
        && list.contains(record.job.users, user)
      {
        True -> record_followup(followups, id, user, message)
        False -> followups
      }
    })
  #(Outbox(..outbox, followups:), effects)
}

fn record_followup(
  followups: Dict(Int, Dict(Int, Int)),
  attempt: Int,
  user: Int,
  message: Int,
) -> Dict(Int, Dict(Int, Int)) {
  dict.upsert(followups, attempt, fn(existing) {
    let reads = option.unwrap(existing, dict.new())
    let watermark = case dict.get(reads, user) {
      Ok(previous) -> int.max(previous, message)
      Error(Nil) -> message
    }
    dict.insert(reads, user, watermark)
  })
}

fn follow_up_clears(
  outbox: Outbox,
  record: Record,
  reads: Dict(Int, Int),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case dict.is_empty(reads) || !outbox.clears_enabled {
    True -> #(outbox, [])
    False ->
      dict.fold(reads, #(outbox, []), fn(acc, user, message) {
        let #(outbox, effects) = acc
        case pending_followup(outbox, user, record.job.channel) {
          Some(attempt) -> #(
            Outbox(
              ..outbox,
              followups: record_followup(
                outbox.followups,
                attempt,
                user,
                message,
              ),
            ),
            effects,
          )
          None -> {
            let clear =
              job.clear_job(
                user:,
                channel: record.job.channel,
                message:,
                after: Some(record.job.message),
              )
            let #(outbox, admitted) = admit(outbox, clear, now_ms)
            #(
              outbox,
              list.flatten([
                effects,
                admitted,
                [Count("followup_clears_total", rule.Direct, 1)],
              ]),
            )
          }
        }
      })
  }
}

fn pending_followup(outbox: Outbox, user: Int, channel: Int) -> Option(Int) {
  dict.fold(outbox.followups, None, fn(found, attempt, reads) {
    case found {
      Some(_) -> found
      None ->
        case dict.has_key(reads, user), dict.get(outbox.attempts, attempt) {
          True, Ok(Attempt(record:, ..)) if record.job.channel == channel ->
            Some(attempt)
          _, _ -> None
        }
    }
  })
}

pub fn session_active(outbox: Outbox, user: Int, now_ms: Int) -> Outbox {
  Outbox(
    ..outbox,
    active: dict.insert(outbox.active, user, #(outbox.next_seq, now_ms)),
    next_seq: outbox.next_seq + 1,
  )
}

pub fn prune(outbox: Outbox, now_ms: Int) -> Outbox {
  let cutoff = now_ms - outbox.limits.reads_max_age_ms
  Outbox(
    ..outbox,
    reads: dict.filter(outbox.reads, fn(_, entry) { entry.1 >= cutoff }),
    active: dict.filter(outbox.active, fn(_, entry) { entry.1 >= cutoff }),
  )
}

pub fn has_attempt(outbox: Outbox, attempt: Int) -> Bool {
  dict.has_key(outbox.attempts, attempt)
}

pub fn record_count(outbox: Outbox) -> Int {
  dict.size(outbox.records)
}

pub fn export(
  outbox: Outbox,
  bucket: Int,
  now_ms: Int,
) -> Result(#(Outbox, List(Carried), List(ReadMark)), Nil) {
  case dict.get(outbox.leaving, bucket) {
    Ok(Holding(_)) -> Error(Nil)
    _ -> Ok(export_idle(outbox, bucket, now_ms))
  }
}

fn export_idle(
  outbox: Outbox,
  bucket: Int,
  now_ms: Int,
) -> #(Outbox, List(Carried), List(ReadMark)) {
  let leaving =
    dict.values(outbox.records)
    |> list.filter(fn(record) { bucket_of(record.job) == bucket })
  let outbox =
    list.fold(leaving, outbox, remove_record)
    |> fn(outbox) {
      Outbox(
        ..outbox,
        leaving: dict.insert(outbox.leaving, bucket, Holding([])),
      )
    }
  let carried =
    leaving
    |> list.sort(fn(a, b) { int.compare(a.ordinal, b.ordinal) })
    |> list.filter_map(fn(record) {
      case unsent(outbox, record) {
        Ok(record) -> Ok(carried(record, now_ms, 0))
        Error(Nil) -> Error(Nil)
      }
    })
  #(outbox, carried, marks_for(outbox, carried, now_ms))
}

fn unsent(outbox: Outbox, record: Record) -> Result(Record, Nil) {
  case record.job.kind {
    job.ClearJob | job.RingJob -> Ok(record)
    job.MessageJob ->
      case
        list.filter(record.job.users, fn(user) {
          !is_read(outbox, user, record.job.channel, record.job.message)
          && !became_active(outbox, user, record.seq)
        })
      {
        [] -> Error(Nil)
        users if users == record.job.users -> Ok(record)
        users -> Ok(Record(..record, job: job.with_users(record.job, users)))
      }
  }
}

fn marks_for(
  outbox: Outbox,
  carried: List(Carried),
  now_ms: Int,
) -> List(ReadMark) {
  carried
  |> list.flat_map(fn(found) {
    list.map(found.job.users, fn(user) { #(user, found.job.channel) })
  })
  |> list.unique
  |> list.filter_map(fn(pair) {
    case dict.get(outbox.reads, pair) {
      Ok(#(message, at)) ->
        Ok(ReadMark(
          user: pair.0,
          channel: pair.1,
          message:,
          age_ms: int.max(0, now_ms - at),
        ))
      Error(Nil) -> Error(Nil)
    }
  })
}

pub fn reclaim(
  outbox: Outbox,
  bucket: Int,
  carried: List(Carried),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let held = case dict.get(outbox.leaving, bucket) {
    Ok(Holding(held)) -> list.reverse(held)
    _ -> []
  }
  let #(outbox, armed) =
    Outbox(..outbox, leaving: dict.delete(outbox.leaving, bucket))
    |> import_carried(list.append(carried, held), now_ms)
  let #(outbox, shed) = shed_to_capacity(outbox, [])
  let #(outbox, pumped) = pump(outbox, now_ms)
  #(outbox, list.flatten([armed, shed, pumped]))
}

pub fn hand(
  outbox: Outbox,
  bucket: Int,
  to: Target,
) -> #(Outbox, List(Effect)) {
  let held = case dict.get(outbox.leaving, bucket) {
    Ok(Holding(held)) -> list.reverse(held)
    _ -> []
  }
  let outbox =
    Outbox(..outbox, leaving: dict.insert(outbox.leaving, bucket, Handed(to)))
  case held {
    [] -> #(outbox, [])
    _ -> #(outbox, [HandOver(to, held)])
  }
}

pub fn hand_all(
  outbox: Outbox,
  to: Target,
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let records =
    dict.values(outbox.records)
    |> list.sort(fn(a, b) { int.compare(a.ordinal, b.ordinal) })
  let held =
    dict.values(outbox.leaving)
    |> list.flat_map(fn(found) {
      case found {
        Holding(held) -> list.reverse(held)
        Handed(_) -> []
      }
    })
  let #(outbox, capped) =
    list.fold(records, outbox, remove_record)
    |> fn(outbox) { Outbox(..outbox, leaving: dict.new(), all_to: Some(to)) }
    |> cap_attempts(now_ms)
  let moving =
    records
    |> list.filter_map(fn(record) {
      unsent(outbox, record) |> result.map(carried(_, now_ms, 0))
    })
    |> list.append(held)
  case moving {
    [] -> #(outbox, capped)
    _ -> #(outbox, [HandOver(to, moving), ..capped])
  }
}

pub fn undrained(outbox: Outbox) -> Outbox {
  Outbox(..outbox, all_to: None)
}

fn cap_attempts(outbox: Outbox, now_ms: Int) -> #(Outbox, List(Effect)) {
  let window = outbox.limits.drain_window_ms
  let #(attempts, capped) =
    dict.fold(outbox.attempts, #(outbox.attempts, []), fn(acc, id, attempt) {
      case attempt.ends_at_ms > now_ms + window {
        False -> acc
        True -> #(
          dict.insert(
            acc.0,
            id,
            Attempt(..attempt, ends_at_ms: now_ms + window),
          ),
          [ArmWindowEnd(id, window), ..acc.1],
        )
      }
    })
  #(Outbox(..outbox, attempts:), capped)
}

pub fn keep(
  outbox: Outbox,
  carried: List(Carried),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let #(outbox, armed) =
    import_here(Outbox(..outbox, all_to: None), carried, now_ms)
  let #(outbox, shed) = shed_to_capacity(outbox, [])
  let #(outbox, pumped) = pump(outbox, now_ms)
  #(outbox, list.flatten([armed, shed, pumped]))
}

pub fn arrive(outbox: Outbox, bucket: Int) -> Outbox {
  case dict.get(outbox.leaving, bucket) {
    Ok(Handed(_)) ->
      Outbox(..outbox, leaving: dict.delete(outbox.leaving, bucket))
    _ -> outbox
  }
}

pub fn adopt(
  outbox: Outbox,
  carried: List(Carried),
  marks: List(ReadMark),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  let outbox =
    list.fold(marks, outbox, fn(outbox, mark) {
      let key = #(mark.user, mark.channel)
      let watermark = case dict.get(outbox.reads, key) {
        Ok(#(previous, _)) -> int.max(previous, mark.message)
        Error(Nil) -> mark.message
      }
      Outbox(
        ..outbox,
        reads: dict.insert(outbox.reads, key, #(watermark, now_ms - mark.age_ms)),
      )
    })
    |> import_carried(carried, now_ms)
  let #(outbox, armed) = outbox
  let #(outbox, shed) = shed_to_capacity(outbox, [])
  let #(outbox, pumped) = pump(outbox, now_ms)
  let adopted = case carried {
    [] -> []
    _ -> [Count("adopted_total", rule.Direct, list.length(carried))]
  }
  #(outbox, list.flatten([adopted, armed, shed, pumped]))
}

fn import_carried(
  outbox: Outbox,
  carried: List(Carried),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  case outbox.all_to, carried {
    _, [] -> #(outbox, [])
    Some(to), _ -> #(outbox, [HandOver(to, carried)])
    None, _ -> import_here(outbox, carried, now_ms)
  }
}

fn import_here(
  outbox: Outbox,
  carried: List(Carried),
  now_ms: Int,
) -> #(Outbox, List(Effect)) {
  list.fold(carried, #(outbox, []), fn(acc, found) {
    let #(outbox, effects) = acc
    let record =
      Record(
        seq: outbox.next_seq,
        ordinal: outbox.next_ordinal,
        job: found.job,
        admitted_at_ms: now_ms - found.age_ms,
        attempts: found.attempts,
      )
    let outbox =
      Outbox(
        ..outbox,
        next_seq: outbox.next_seq + 1,
        next_ordinal: outbox.next_ordinal + 1,
      )
    case found.retry_in_ms > 0 {
      True -> #(insert_record(outbox, record), [
        ArmRetry(record.seq, found.retry_in_ms),
        ..effects
      ])
      False -> #(insert_ready(outbox, record), effects)
    }
  })
}

pub fn buckets(outbox: Outbox) -> List(Int) {
  let records = dict.values(outbox.records)
  let attempts =
    dict.values(outbox.attempts) |> list.map(fn(found) { found.record })
  list.append(records, attempts)
  |> list.map(fn(record) { bucket_of(record.job) })
  |> list.append(
    dict.to_list(outbox.leaving)
    |> list.filter_map(fn(pair) {
      case pair.1 {
        Holding([_, ..]) -> Ok(pair.0)
        _ -> Error(Nil)
      }
    }),
  )
  |> list.unique
}

pub fn pending(outbox: Outbox) -> Int {
  dict.size(outbox.records)
  + dict.size(outbox.attempts)
  + list.fold(dict.values(outbox.leaving), 0, fn(total, found) {
    case found {
      Holding(held) -> total + list.length(held)
      Handed(_) -> total
    }
  })
}

pub fn is_idle(outbox: Outbox) -> Bool {
  dict.is_empty(outbox.records)
  && dict.is_empty(outbox.attempts)
  && list.all(dict.values(outbox.leaving), fn(found) {
    case found {
      Holding(held) -> held == []
      Handed(_) -> True
    }
  })
}
