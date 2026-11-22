// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/guild.{MemberPresence, MemberUserUpdate}
import fluxer_gateway/presence/event.{type PresenceEvent}
import fluxer_gateway/presence/gate
import fluxer_gateway/presence/owner
import fluxer_gateway/presence/protocol.{
  type Subscriber, Foreign, GuildSubscriber, PresenceSubscriber,
}
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import fluxer_gateway/presence/version.{type Version}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

const wide_above = 128

pub type LaneMsg {
  Fan(target: Int, generation: Int, event: PresenceEvent)
  Forget(target: Int)
  Settle(target: Int, subscriber: Subscriber, last: Version)
}

type UniqueOption {
  Positive
  Monotonic
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

fn next_generation() -> Int {
  unique_integer([Positive, Monotonic])
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

fn init_timeout_ms() -> Int {
  durations.presence().fanout_init_timeout_ms
}

fn lane_name(lane: Int) -> Name(LaneMsg) {
  route.name(route.Lane, lane)
}

fn lane_table(lane: Int) -> Atom {
  route.table(route.LaneTable, lane)
}

fn width_table(target: Int) -> Atom {
  route.table(route.WidthTable, route.shard(target))
}

fn lane_of(subscriber: Subscriber) -> Int {
  phash2(subscriber, route.lane_count)
}

pub fn tables() -> List(Atom) {
  list.map(route.shards(), route.table(route.WidthTable, _))
}

pub fn bag_tables() -> List(Atom) {
  list.map(route.lanes(), lane_table)
}

pub fn deliver(subscriber: Subscriber, found: PresenceEvent) -> Nil {
  case subscriber {
    PresenceSubscriber(inbox:, ..) -> process.send(inbox, Foreign(found))
    GuildSubscriber(inbox:, ..) ->
      case event.user_id(found) {
        Ok(user) -> {
          case found.kind {
            event.UserUpdated ->
              process.send(inbox, MemberUserUpdate(found.user_wire))
            event.StatusChanged -> Nil
          }
          process.send(inbox, MemberPresence(event.to_presence(found, user)))
        }
        Error(Nil) -> Nil
      }
  }
}

type Width {
  Narrow(subscribers: Int)
  Wide(subscribers: Int, generation: Int)
}

type Lane {
  Lane(index: Int, cached: Dict(Int, #(Int, List(Subscriber))))
}

fn width(target: Int) -> Width {
  case table.lookup(width_table(target), target) {
    [#(_, subscribers, True, generation), ..] -> Wide(subscribers, generation)
    [#(_, subscribers, False, _), ..] -> Narrow(subscribers)
    [] -> Narrow(0)
  }
}

fn store_width(target: Int, found: Width) -> Nil {
  case found {
    Wide(subscribers, generation) ->
      table.insert(width_table(target), #(target, subscribers, True, generation))
    Narrow(subscribers) ->
      table.insert(width_table(target), #(target, subscribers, False, 0))
  }
}

pub fn fan_out(
  target: Int,
  event: PresenceEvent,
  subscribers: fn(Int) -> List(Subscriber),
) -> Int {
  case width(target) {
    Wide(subscribers:, generation:) -> {
      list.each(route.lanes(), fn(lane) {
        let _ =
          remote.send_registered(
            lane_name(lane),
            Fan(target, generation, event),
          )
        Nil
      })
      subscribers
    }
    Narrow(..) -> {
      let all = gate.screen(target, event.version, subscribers(target))
      list.each(all, deliver(_, event))
      list.length(all)
    }
  }
}

pub fn settle(target: Int, subscriber: Subscriber, last: Version) -> Nil {
  let _ =
    remote.send_registered(
      lane_name(lane_of(subscriber)),
      Settle(target, subscriber, last),
    )
  Nil
}

pub fn joined(
  target: Int,
  subscriber: Subscriber,
  subscribers: fn(Int) -> List(Subscriber),
) -> Nil {
  case width(target) {
    Wide(count, _) -> {
      table.insert(lane_table(lane_of(subscriber)), #(target, subscriber))
      store_width(target, Wide(count + 1, next_generation()))
    }
    Narrow(count) if count >= wide_above -> {
      list.each(subscribers(target), fn(one) {
        table.insert(lane_table(lane_of(one)), #(target, one))
      })
      store_width(target, Wide(count + 1, next_generation()))
    }
    Narrow(count) -> store_width(target, Narrow(count + 1))
  }
}

pub fn left(target: Int, subscriber: Subscriber) -> Nil {
  let found = width(target)
  case found {
    Wide(..) ->
      table.delete_row(lane_table(lane_of(subscriber)), #(target, subscriber))
    Narrow(..) -> Nil
  }
  case found {
    Wide(count, _) if count <= 1 -> {
      table.delete(width_table(target), target)
      list.each(route.lanes(), fn(lane) {
        let _ = remote.send_registered(lane_name(lane), Forget(target))
        Nil
      })
    }
    Narrow(count) if count <= 1 -> table.delete(width_table(target), target)
    Wide(count, _) -> store_width(target, Wide(count - 1, next_generation()))
    Narrow(count) -> store_width(target, Narrow(count - 1))
  }
}

pub fn supervised(lane: Int) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(_) {
      Ok(actor.initialised(Lane(index: lane, cached: dict.new())))
    })
    |> actor.named(lane_name(lane))
    |> actor.on_message(handle)
    |> actor.start
    |> owner.started_nil
  })
}

fn handle(lane: Lane, message: LaneMsg) -> actor.Next(Lane, LaneMsg) {
  case message {
    Fan(target:, generation:, event:) -> {
      let #(lane, subscribers) = members(lane, target, generation)
      gate.screen(target, event.version, subscribers)
      |> list.each(deliver(_, event))
      actor.continue(lane)
    }
    Forget(target) ->
      actor.continue(Lane(..lane, cached: dict.delete(lane.cached, target)))
    Settle(target:, subscriber:, last:) -> {
      gate.settle(target, subscriber, last)
      actor.continue(lane)
    }
  }
}

fn members(
  lane: Lane,
  target: Int,
  generation: Int,
) -> #(Lane, List(Subscriber)) {
  case dict.get(lane.cached, target) {
    Ok(#(cached, subscribers)) if cached == generation -> #(lane, subscribers)
    _ ->
      case table.values(lane_table(lane.index), target) {
        [] -> #(Lane(..lane, cached: dict.delete(lane.cached, target)), [])
        subscribers -> #(
          Lane(
            ..lane,
            cached: dict.insert(lane.cached, target, #(generation, subscribers)),
          ),
          subscribers,
        )
      }
  }
}
