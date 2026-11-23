// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/nats/address.{type Address}
import fluxer_gateway/nats/connection.{type Connection}
import fluxer_gateway/nats/metrics as nats_metrics
import fluxer_gateway/nats/protocol
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.nats().pool_init_timeout_ms
}

fn connect_slack_ms() -> Int {
  durations.nats().pool_connect_slack_ms
}

const pool_size = 16

fn connect_timeout_ms() -> Int {
  durations.nats().pool_connect_timeout_ms
}

fn reconnect_base_ms() -> Int {
  durations.nats().pool_reconnect_base_ms
}

fn reconnect_max_ms() -> Int {
  durations.nats().pool_reconnect_max_ms
}

const breaker_threshold = 5

fn breaker_cooldown_ms() -> Int {
  durations.nats().pool_breaker_cooldown_ms
}

fn subscribe_retry_base_ms() -> Int {
  durations.nats().pool_subscribe_retry_base_ms
}

fn subscribe_retry_max_ms() -> Int {
  durations.nats().pool_subscribe_retry_max_ms
}

fn subscribe_timeout_ms() -> Int {
  durations.nats().pool_subscribe_timeout_ms
}

fn call_timeout_ms() -> Int {
  durations.nats().pool_call_timeout_ms
}

pub type NatsMessage {
  NatsMessage(subject: String, reply_to: Option(String), payload: BitArray)
}

pub type NatsError {
  NotConnected
  NatsTimeout
  NoResponders
  ConnectionClosed
  ProtocolError(String)
}

pub opaque type SubscriptionId {
  SubscriptionId(id: Int, pool: Pid)
}

pub opaque type Message {
  ConnectSlot(slot: Int)
  Connected(slot: Int, token: Int, result: Result(Connection, String))
  ConnectTimeout(slot: Int, token: Int)
  SlotDown(process.Down)
  Resubscribe(slot: Int)
  AddSubscription(
    subject: String,
    queue: Option(String),
    to: Subject(NatsMessage),
    every_slot: Bool,
    reply: Subject(SubscriptionId),
  )
  RemoveSubscriptionsTo(to: Subject(NatsMessage), reply: Subject(Nil))
}

type Slot {
  Idle
  Connecting(token: Int)
  Ready(connection: Connection, pid: Pid, monitor: Monitor)
}

type Subscription {
  Subscription(
    subject: String,
    queue: Option(String),
    to: Subject(NatsMessage),
    every_slot: Bool,
    placed: Dict(Int, Int),
    owner: Option(Pid),
  )
}

type State {
  State(
    self: Subject(Message),
    server: Result(Address, address.AddressError),
    auth_token: Option(String),
    slots: Dict(Int, Slot),
    failures: Dict(Int, Int),
    subscriptions: Dict(Int, Subscription),
    retries: Dict(Int, Int),
    next_id: Int,
    next_token: Int,
  )
}

type SlotTable

type Key {
  FluxerGatewayNatsSlots
}

type Stored

@external(erlang, "erlang", "list_to_tuple")
fn slot_table(slots: List(Option(Connection))) -> SlotTable

@external(erlang, "erlang", "element")
fn slot_at(index: Int, table: SlotTable) -> Option(Connection)

@external(erlang, "erlang", "tuple_size")
fn table_size(table: SlotTable) -> Int

@external(erlang, "persistent_term", "get")
fn get_table(key: Key, default: Option(SlotTable)) -> Option(SlotTable)

@external(erlang, "persistent_term", "put")
fn put_table(key: Key, value: Option(SlotTable)) -> Stored

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_nats_pool")
}

pub fn pool_pid() -> Result(Pid, Nil) {
  process.named(name())
}

pub fn subscription_pool(subscription: SubscriptionId) -> Pid {
  subscription.pool
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings) })
}

fn start(settings: Settings) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    put_table(FluxerGatewayNatsSlots, Some(slot_table(empty_slots(pool_size))))
    nats_metrics.watch_connections(slot_counts)
    let self = process.new_subject()
    list.each(slot_indices(), fn(slot) { process.send(self, ConnectSlot(slot)) })
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_monitors(SlotDown)
    State(
      self:,
      server: address.parse(settings.nats_core_url),
      auth_token: address.auth_token(settings.nats_auth_token),
      slots: dict.new(),
      failures: dict.new(),
      subscriptions: dict.new(),
      retries: dict.new(),
      next_id: 1,
      next_token: 1,
    )
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

fn slot_indices() -> List(Int) {
  indices(0, pool_size - 1, [])
}

fn indices(from: Int, to: Int, acc: List(Int)) -> List(Int) {
  case to < from {
    True -> acc
    False -> indices(from, to - 1, [to, ..acc])
  }
}

fn empty_slots(count: Int) -> List(Option(Connection)) {
  list.repeat(None, count)
}

fn slot_counts() -> #(Int, Int) {
  case get_table(FluxerGatewayNatsSlots, None) {
    None -> #(0, 0)
    Some(table) -> {
      let size = table_size(table)
      let ready =
        list.count(indices(1, size, []), fn(index) {
          option.is_some(slot_at(index, table))
        })
      #(ready, size - ready)
    }
  }
}

fn outcome(result: Result(a, NatsError)) -> String {
  case result {
    Ok(_) -> "ok"
    Error(NotConnected) -> "not_connected"
    Error(NatsTimeout) -> "timeout"
    Error(NoResponders) -> "no_responders"
    Error(ConnectionClosed) -> "closed"
    Error(ProtocolError(_)) -> "protocol_error"
  }
}

fn publish(subject: String, payload: BitArray) -> Result(Nil, NatsError) {
  case protocol.valid_token(subject) {
    False -> Error(ProtocolError("invalid subject"))
    True ->
      case pick() {
        Error(Nil) -> Error(NotConnected)
        Ok(found) ->
          case connection.publish(found, subject, None, payload) {
            Ok(Nil) -> Ok(Nil)
            Error(connection.TooLarge(..)) ->
              Error(ProtocolError("max_payload"))
            Error(connection.WriteFailed(_)) -> Error(ConnectionClosed)
          }
      }
  }
}

pub fn publish_reply(
  subject: String,
  payload: BitArray,
) -> Result(Nil, NatsError) {
  let published = publish(subject, payload)
  nats_metrics.published("reply", outcome(published))
  case published {
    Ok(Nil) -> Ok(Nil)
    Error(reason) -> {
      logging.log(
        logging.Warning,
        "Gateway NATS pool reply publish failed: subject="
          <> subject
          <> " reason="
          <> string.inspect(reason),
      )
      Error(reason)
    }
  }
}

pub fn request(
  subject: String,
  payload: BitArray,
  timeout_ms: Int,
) -> Result(BitArray, NatsError) {
  let answered = send_request(subject, payload, timeout_ms)
  nats_metrics.published("request", outcome(answered))
  answered
}

fn send_request(
  subject: String,
  payload: BitArray,
  timeout_ms: Int,
) -> Result(BitArray, NatsError) {
  case protocol.valid_token(subject), pick() {
    False, _ -> Error(ProtocolError("invalid subject"))
    True, Error(Nil) -> Error(NotConnected)
    True, Ok(found) ->
      case connection.request(found, subject, payload, timeout_ms) {
        Ok(body) -> Ok(body)
        Error(connection.RequestTimeout) -> Error(NatsTimeout)
        Error(connection.RequestNoResponders) -> Error(NoResponders)
        Error(connection.RequestClosed) -> Error(ConnectionClosed)
        Error(connection.RequestPublishFailed(connection.TooLarge(..))) ->
          Error(ProtocolError("max_payload"))
        Error(connection.RequestPublishFailed(connection.WriteFailed(_))) ->
          Error(ConnectionClosed)
      }
  }
}

pub fn subscribe(
  subject: String,
  queue: Option(String),
  to: Subject(NatsMessage),
) -> Result(SubscriptionId, NatsError) {
  add(subject, queue, to, False)
}

pub fn unsubscribe_all(to: Subject(NatsMessage)) -> Nil {
  let reply = process.new_subject()
  case remote.send_local(name(), RemoveSubscriptionsTo(to, reply)) {
    Ok(Nil) -> {
      let _ = process.receive(reply, call_timeout_ms())
      Nil
    }
    Error(Nil) -> Nil
  }
}

pub fn subscribe_every_slot(
  subject: String,
  queue: Option(String),
  to: Subject(NatsMessage),
) -> Result(SubscriptionId, NatsError) {
  add(subject, queue, to, True)
}

fn add(
  subject: String,
  queue: Option(String),
  to: Subject(NatsMessage),
  every_slot: Bool,
) -> Result(SubscriptionId, NatsError) {
  let valid_queue = case queue {
    Some(group) -> protocol.valid_token(group)
    None -> True
  }
  let reply = process.new_subject()
  case protocol.valid_token(subject) && valid_queue {
    False -> Error(ProtocolError("invalid subject"))
    True ->
      case
        remote.send_local(
          name(),
          AddSubscription(subject, queue, to, every_slot, reply),
        )
      {
        Error(Nil) -> Error(NotConnected)
        Ok(Nil) ->
          case process.receive(reply, call_timeout_ms()) {
            Ok(id) -> Ok(id)
            Error(Nil) -> Error(NatsTimeout)
          }
      }
  }
}

pub fn has_ready_connection() -> Bool {
  result.is_ok(pick())
}

fn pick() -> Result(Connection, Nil) {
  case get_table(FluxerGatewayNatsSlots, None) {
    None -> Error(Nil)
    Some(table) -> {
      let size = table_size(table)
      case size {
        0 -> Error(Nil)
        _ -> scan(table, size, phash2(process.self(), size), 0)
      }
    }
  }
}

fn scan(
  table: SlotTable,
  size: Int,
  start: Int,
  tried: Int,
) -> Result(Connection, Nil) {
  case tried >= size {
    True -> Error(Nil)
    False ->
      case slot_at({ start + tried } % size + 1, table) {
        Some(found) -> Ok(found)
        None -> scan(table, size, start, tried + 1)
      }
  }
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    ConnectSlot(slot) -> actor.continue(connect_slot(state, slot))
    Connected(slot, token, result) ->
      actor.continue(connected(state, slot, token, result))
    ConnectTimeout(slot, token) ->
      case dict.get(state.slots, slot) {
        Ok(Connecting(current)) if current == token -> {
          logging.log(
            logging.Warning,
            "Gateway NATS pool slot connect attempt timed out: slot="
              <> int.to_string(slot),
          )
          nats_metrics.connection_event("connect_failed")
          actor.continue(schedule_reconnect(
            State(..state, slots: dict.insert(state.slots, slot, Idle)),
            slot,
          ))
        }
        _ -> actor.continue(state)
      }
    SlotDown(process.ProcessDown(pid:, ..)) ->
      actor.continue(slot_down(state, pid))
    SlotDown(process.PortDown(..)) -> actor.continue(state)
    Resubscribe(slot) -> actor.continue(place_on_slot(state, slot))
    RemoveSubscriptionsTo(to, reply) -> {
      let state =
        dict.fold(state.subscriptions, state, fn(state, id, subscription) {
          case subscription.to == to {
            True -> remove_subscription(state, id)
            False -> state
          }
        })
      process.send(reply, Nil)
      actor.continue(state)
    }
    AddSubscription(subject, queue, to, every_slot, reply) ->
      case existing(state, subject, queue, to, every_slot) {
        Ok(id) -> {
          process.send(reply, SubscriptionId(id, process.self()))
          actor.continue(state)
        }
        Error(Nil) -> {
          let id = state.next_id
          let subscription =
            Subscription(
              subject:,
              queue:,
              to:,
              every_slot:,
              placed: dict.new(),
              owner: watch_owner(to),
            )
          let state =
            State(
              ..state,
              subscriptions: dict.insert(state.subscriptions, id, subscription),
              next_id: id + 1,
            )
          let state = place_subscription(state, id)
          nats_metrics.subscriptions_held(dict.size(state.subscriptions))
          process.send(reply, SubscriptionId(id, process.self()))
          actor.continue(state)
        }
      }
  }
}

fn existing(
  state: State,
  subject: String,
  queue: Option(String),
  to: Subject(NatsMessage),
  every_slot: Bool,
) -> Result(Int, Nil) {
  state.subscriptions
  |> dict.to_list
  |> list.find_map(fn(entry) {
    let #(id, found) = entry
    case
      found.subject == subject
      && found.queue == queue
      && found.to == to
      && found.every_slot == every_slot
    {
      True -> Ok(id)
      False -> Error(Nil)
    }
  })
}

fn watch_owner(to: Subject(NatsMessage)) -> Option(Pid) {
  case process.subject_owner(to) {
    Ok(owner) -> {
      let _ = process.monitor(owner)
      Some(owner)
    }
    Error(Nil) -> None
  }
}

fn remove_subscription(state: State, id: Int) -> State {
  case dict.get(state.subscriptions, id) {
    Ok(subscription) -> {
      dict.each(subscription.placed, fn(slot, sid) {
        case ready_connection(state, slot) {
          Ok(found) -> connection.unsubscribe(found, sid)
          Error(Nil) -> Nil
        }
      })
      let subscriptions = dict.delete(state.subscriptions, id)
      nats_metrics.subscriptions_held(dict.size(subscriptions))
      State(..state, subscriptions:)
    }
    Error(Nil) -> state
  }
}

fn owner_down(state: State, pid: Pid) -> State {
  dict.fold(state.subscriptions, state, fn(state, id, subscription) {
    case subscription.owner == Some(pid) {
      True -> remove_subscription(state, id)
      False -> state
    }
  })
}

fn connect_slot(state: State, slot: Int) -> State {
  case dict.get(state.slots, slot), state.server {
    Ok(Connecting(_)), _ | Ok(Ready(..)), _ -> state
    _, Error(reason) -> {
      logging.log(
        logging.Error,
        "Gateway NATS pool failed to parse URL: " <> string.inspect(reason),
      )
      schedule_reconnect(state, slot)
    }
    _, Ok(server) -> {
      let token = state.next_token
      let parent = state.self
      let auth_token = state.auth_token
      let owner = process.self()
      process.spawn_unlinked(fn() {
        let result =
          connection.start(server, auth_token, connect_timeout_ms(), owner)
        process.send(parent, Connected(slot, token, result))
      })
      process.send_after(
        state.self,
        connect_timeout_ms() + connect_slack_ms(),
        ConnectTimeout(slot, token),
      )
      State(
        ..state,
        slots: dict.insert(state.slots, slot, Connecting(token)),
        next_token: token + 1,
      )
    }
  }
}

fn connected(
  state: State,
  slot: Int,
  token: Int,
  result: Result(Connection, String),
) -> State {
  case dict.get(state.slots, slot), result {
    Ok(Connecting(current)), Ok(found) if current == token ->
      case connection.pid(found) {
        Ok(pid) -> {
          let monitor = process.monitor(pid)
          logging.log(
            logging.Info,
            "Gateway NATS pool slot connected: slot=" <> int.to_string(slot),
          )
          nats_metrics.connection_event("connected")
          State(
            ..state,
            slots: dict.insert(state.slots, slot, Ready(found, pid, monitor)),
            failures: dict.delete(state.failures, slot),
          )
          |> publish_slots
          |> place_on_slot(slot)
        }
        Error(Nil) ->
          schedule_reconnect(
            State(..state, slots: dict.insert(state.slots, slot, Idle)),
            slot,
          )
      }
    Ok(Connecting(current)), Error(reason) if current == token -> {
      logging.log(
        logging.Error,
        "Gateway NATS pool slot failed to connect: slot="
          <> int.to_string(slot)
          <> " reason="
          <> reason,
      )
      nats_metrics.connection_event("connect_failed")
      schedule_reconnect(
        State(..state, slots: dict.insert(state.slots, slot, Idle)),
        slot,
      )
    }
    _, Ok(stale) -> {
      connection.close(stale)
      state
    }
    _, Error(_) -> state
  }
}

fn slot_down(state: State, pid: Pid) -> State {
  let found =
    dict.fold(state.slots, None, fn(found, slot, entry) {
      case entry {
        Ready(pid: owner, ..) if owner == pid -> Some(slot)
        _ -> found
      }
    })
  case found {
    None -> owner_down(state, pid)
    Some(slot) -> {
      logging.log(
        logging.Warning,
        "Gateway NATS pool slot connection lost, reconnecting: slot="
          <> int.to_string(slot),
      )
      nats_metrics.connection_event("lost")
      let subscriptions =
        dict.map_values(state.subscriptions, fn(_, subscription) {
          Subscription(
            ..subscription,
            placed: dict.delete(subscription.placed, slot),
          )
        })
      let state =
        State(
          ..state,
          slots: dict.insert(state.slots, slot, Idle),
          subscriptions:,
        )
        |> publish_slots
      let state =
        dict.fold(state.subscriptions, state, fn(state, id, subscription) {
          case subscription.every_slot, dict.is_empty(subscription.placed) {
            False, True -> place_subscription(state, id)
            _, _ -> state
          }
        })
      schedule_reconnect(state, slot)
    }
  }
}

fn schedule_reconnect(state: State, slot: Int) -> State {
  let failures = failure_count(state, slot) + 1
  process.send_after(state.self, reconnect_delay(failures), ConnectSlot(slot))
  State(..state, failures: dict.insert(state.failures, slot, failures))
}

fn failure_count(state: State, slot: Int) -> Int {
  result.unwrap(dict.get(state.failures, slot), 0)
}

pub fn reconnect_delay(failures: Int) -> Int {
  case failures >= breaker_threshold {
    True -> breaker_cooldown_ms()
    False ->
      int.min(
        reconnect_max_ms(),
        reconnect_base_ms()
          * int.bitwise_shift_left(1, int.max(failures - 1, 0)),
      )
  }
}

pub fn subscribe_retry_delay(failures: Int) -> Int {
  case failures {
    count if count > 5 -> subscribe_retry_max_ms()
    count if count > 0 ->
      int.min(
        subscribe_retry_max_ms(),
        int.bitwise_shift_left(subscribe_retry_base_ms(), count - 1),
      )
    _ -> subscribe_retry_base_ms()
  }
}

fn publish_slots(state: State) -> State {
  let ready =
    list.map(slot_indices(), fn(slot) {
      case dict.get(state.slots, slot) {
        Ok(Ready(connection: found, ..)) -> Some(found)
        _ -> None
      }
    })
  put_table(FluxerGatewayNatsSlots, Some(slot_table(ready)))
  state
}

fn ready_connection(state: State, slot: Int) -> Result(Connection, Nil) {
  case dict.get(state.slots, slot) {
    Ok(Ready(connection: found, ..)) -> Ok(found)
    _ -> Error(Nil)
  }
}

fn ready_slots(state: State) -> List(Int) {
  list.filter(slot_indices(), fn(slot) {
    result.is_ok(ready_connection(state, slot))
  })
}

fn place_on_slot(state: State, slot: Int) -> State {
  case ready_connection(state, slot) {
    Error(Nil) -> state
    Ok(found) -> {
      let #(state, failed) =
        dict.fold(
          state.subscriptions,
          #(state, False),
          fn(acc, id, subscription) {
            let #(state, failed) = acc
            let wanted =
              subscription.every_slot || dict.is_empty(subscription.placed)
            case wanted && !dict.has_key(subscription.placed, slot) {
              False -> acc
              True ->
                case place(state, found, slot, id, subscription) {
                  Ok(state) -> #(state, failed)
                  Error(Nil) -> #(state, True)
                }
            }
          },
        )
      case failed {
        False -> State(..state, retries: dict.delete(state.retries, slot))
        True -> {
          let attempt = result.unwrap(dict.get(state.retries, slot), 0) + 1
          let delay = subscribe_retry_delay(attempt)
          logging.log(
            logging.Warning,
            "Gateway NATS pool slot will retry its subscriptions: slot="
              <> int.to_string(slot)
              <> " delay_ms="
              <> int.to_string(delay),
          )
          process.send_after(state.self, delay, Resubscribe(slot))
          State(..state, retries: dict.insert(state.retries, slot, attempt))
        }
      }
    }
  }
}

fn place_subscription(state: State, id: Int) -> State {
  case dict.get(state.subscriptions, id) {
    Error(Nil) -> state
    Ok(subscription) -> {
      let targets = case subscription.every_slot {
        True -> ready_slots(state)
        False -> list.take(ready_slots(state), 1)
      }
      list.fold(targets, state, fn(state, slot) { place_on_slot(state, slot) })
    }
  }
}

fn place(
  state: State,
  found: Connection,
  slot: Int,
  id: Int,
  subscription: Subscription,
) -> Result(State, Nil) {
  let to = subscription.to
  let deliver = fn(subject, reply_to, payload) {
    process.send(to, NatsMessage(subject:, reply_to:, payload:))
  }
  case
    connection.subscribe(
      found,
      subscription.subject,
      subscription.queue,
      deliver,
      subscribe_timeout_ms(),
    )
  {
    Error(Nil) -> {
      logging.log(
        logging.Error,
        "Gateway NATS pool slot failed to subscribe: subject="
          <> subscription.subject,
      )
      Error(Nil)
    }
    Ok(sid) -> {
      let subscription =
        Subscription(
          ..subscription,
          placed: dict.insert(subscription.placed, slot, sid),
        )
      Ok(
        State(
          ..state,
          subscriptions: dict.insert(state.subscriptions, id, subscription),
        ),
      )
    }
  }
}
