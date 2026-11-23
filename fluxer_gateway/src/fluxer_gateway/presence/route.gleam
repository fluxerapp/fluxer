// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{type Unavailable, Unavailable}
import fluxer_gateway/core/node
import fluxer_gateway/util/table
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub const shard_count = 16

pub const lane_count = 32

const slot_count = 65_536

fn rebuild_interval_ms() -> Int {
  durations.presence().route_rebuild_interval_ms
}

fn unreachable_retry_ms() -> Int {
  durations.presence().route_unreachable_retry_ms
}

fn handoff_window_ms() -> Int {
  durations.presence().route_handoff_window_ms
}

pub type RouteBuilt {
  RouteBuilt
}

pub type Named {
  Manager
  Relay
  Registry
  RegistryCatchUp
  StateTable
  PidsTable
  LeftoversTable
  SubsTable
  InterestTable
  GravesTable
  GuildInterestTable
  SubsIndexTable
  WidthTable
  GateTable
  Lane
  LaneTable
}

const all_named = [
  Manager,
  Relay,
  Registry,
  RegistryCatchUp,
  StateTable,
  PidsTable,
  LeftoversTable,
  SubsTable,
  InterestTable,
  GravesTable,
  GuildInterestTable,
  SubsIndexTable,
  WidthTable,
  GateTable,
  Lane,
  LaneTable,
]

type Names

type NamesKey {
  FluxerPresenceNames
}

type SlotsKey {
  FluxerPresenceSlots
}

type SlotTable

type Slots {
  Slots(
    nodes: List(Atom),
    table: SlotTable,
    previous: Option(SlotTable),
    built_ms: Int,
  )
}

type Builder {
  Rebuild
  Topology(router.TopologyChanged)
}

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

@external(erlang, "erlang", "list_to_tuple")
fn to_slot_table(nodes: List(Atom)) -> SlotTable

@external(erlang, "erlang", "element")
fn slot_at(index: Int, table: SlotTable) -> Atom

@external(erlang, "persistent_term", "put")
fn put_slots(key: SlotsKey, value: Option(Slots)) -> Nil

@external(erlang, "persistent_term", "get")
fn get_slots(key: SlotsKey, default: Option(Slots)) -> Option(Slots)

@external(erlang, "persistent_term", "put")
fn put_names(key: NamesKey, value: Option(Names)) -> Nil

@external(erlang, "persistent_term", "get")
fn get_names(key: NamesKey, default: Option(Names)) -> Option(Names)

@external(erlang, "erlang", "list_to_tuple")
fn to_names(atoms: List(Atom)) -> Names

@external(erlang, "erlang", "element")
fn name_at(index: Int, names: Names) -> Atom

@external(erlang, "gleam_stdlib", "identity")
fn atom_name(atom: Atom) -> Name(message)

@external(erlang, "erlang", "nodes")
fn connected_nodes() -> List(Atom)

@external(erlang, "erlang", "binary_to_atom")
fn fixed_name(text: String) -> Name(message)

pub fn shard(user: Int) -> Int {
  shard_of_low(int.bitwise_and(user, 0xFFFFFFFF))
}

pub fn shard_of_low(low: Int) -> Int {
  int.bitwise_and(
    int.bitwise_exclusive_or(int.bitwise_shift_right(low, 22), low),
    shard_count - 1,
  )
}

pub fn shards() -> List(Int) {
  int.range(0, shard_count, [], list.prepend)
  |> list.reverse
}

pub fn lanes() -> List(Int) {
  int.range(0, lane_count, [], list.prepend)
  |> list.reverse
}

pub fn name(named: Named, index: Int) -> Name(message) {
  atom_name(table(named, index))
}

pub fn table(named: Named, index: Int) -> Atom {
  name_at(offset(named) + index + 1, names())
}

fn indexes(named: Named) -> List(Int) {
  case named {
    Lane | LaneTable -> lanes()
    _ -> shards()
  }
}

fn offset(named: Named) -> Int {
  case named {
    Lane -> 14 * shard_count
    LaneTable -> 14 * shard_count + lane_count
    _ -> position(named) * shard_count
  }
}

fn position(named: Named) -> Int {
  case named {
    Manager -> 0
    Relay -> 1
    Registry -> 2
    RegistryCatchUp -> 3
    StateTable -> 4
    PidsTable -> 5
    LeftoversTable -> 6
    SubsTable -> 7
    InterestTable -> 8
    GravesTable -> 9
    GuildInterestTable -> 10
    SubsIndexTable -> 11
    WidthTable -> 12
    GateTable -> 13
    Lane | LaneTable -> 14
  }
}

fn prefix(named: Named) -> String {
  case named {
    Manager -> "manager_"
    Relay -> "relay_"
    Registry -> "registry_"
    RegistryCatchUp -> "registry_catch_up_"
    StateTable -> "state_"
    PidsTable -> "pids_"
    LeftoversTable -> "leftovers_"
    SubsTable -> "subs_"
    InterestTable -> "interest_"
    GravesTable -> "graves_"
    GuildInterestTable -> "guild_interest"
    SubsIndexTable -> "subs_index_"
    Lane -> "lane_"
    LaneTable -> "lane_table_"
    WidthTable -> "width_"
    GateTable -> "gates_"
  }
}

fn names() -> Names {
  case get_names(FluxerPresenceNames, None) {
    Some(found) -> found
    None -> {
      let built = build_names()
      put_names(FluxerPresenceNames, Some(built))
      built
    }
  }
}

fn build_names() -> Names {
  all_named
  |> list.flat_map(fn(named) {
    list.map(indexes(named), fn(index) {
      let suffix = case named {
        GuildInterestTable -> ""
        _ -> int.to_string(index)
      }
      let text = "fluxer_presence_" <> prefix(named) <> suffix
      atom.create(text)
    })
  })
  |> to_names
}

pub fn local_name(text: String) -> Name(message) {
  fixed_name("fluxer_presence_" <> text)
}

pub fn self_atom() -> Atom {
  node.to_atom(node.self())
}

pub fn owner(user: Int) -> Result(Atom, Unavailable) {
  case router.bouncer_mode() {
    True ->
      case router.owner(router.PresenceKey(user), node.Presence) {
        router.Local -> Ok(self_atom())
        router.Remote(found) -> Ok(node.to_atom(found))
        router.OwnerUnavailable(retry_after_ms) ->
          Error(Unavailable(retry_after_ms:))
      }
    False -> slot_owner(user)
  }
}

fn slot_owner(user: Int) -> Result(Atom, Unavailable) {
  let slot = phash2(user, slot_count)
  let chosen = case get_slots(FluxerPresenceSlots, None) {
    Some(slots) -> Ok(slot_at(slot + 1, slots.table))
    None -> {
      ensure_builder()
      case presence_nodes() {
        [] -> Error(Unavailable(retry_after_ms: unreachable_retry_ms()))
        nodes -> Ok(pick(nodes, slot))
      }
    }
  }
  use found <- result.try(chosen)
  case reachable(found) {
    True -> Ok(found)
    False ->
      case list.filter(presence_nodes(), reachable) {
        [] -> Error(Unavailable(retry_after_ms: unreachable_retry_ms()))
        live -> Ok(pick(live, slot))
      }
  }
}

fn reachable(node: Atom) -> Bool {
  node == self_atom() || list.contains(connected_nodes(), node)
}

pub fn previous_owner(user: Int) -> Option(Atom) {
  case router.bouncer_mode(), get_slots(FluxerPresenceSlots, None) {
    True, _ -> None
    False, Some(Slots(previous: Some(table), built_ms:, ..)) ->
      case clock.monotonic_ms() - built_ms < handoff_window_ms() {
        True -> {
          let found = slot_at(phash2(user, slot_count) + 1, table)
          case reachable(found) {
            True -> Some(found)
            False -> None
          }
        }
        False -> None
      }
    _, _ -> None
  }
}

pub fn presence_nodes() -> List(Atom) {
  let known =
    router.nodes_with(node.Presence)
    |> list.map(node.to_atom)
  let with_self = case
    settings.role_runs(settings.get(), node.Presence)
    && !list.contains(known, self_atom())
  {
    True -> [self_atom(), ..known]
    False -> known
  }
  list.sort(with_self, fn(a, b) {
    string.compare(atom.to_string(a), atom.to_string(b))
  })
}

pub fn owner_among(user: Int, nodes: List(Atom)) -> Option(Atom) {
  case nodes {
    [] -> None
    _ -> Some(pick(nodes, phash2(user, slot_count)))
  }
}

fn weight(candidate: Atom, slot: Int) -> Int {
  phash2(#(candidate, slot), 4_294_967_296)
}

fn pick(nodes: List(Atom), slot: Int) -> Atom {
  case nodes {
    [] -> self_atom()
    [first, ..rest] ->
      list.fold(rest, #(first, weight(first, slot)), fn(best, candidate) {
        let score = weight(candidate, slot)
        case score > best.1 {
          True -> #(candidate, score)
          False -> best
        }
      }).0
  }
}

fn build(nodes: List(Atom)) -> Nil {
  case nodes {
    [] -> put_slots(FluxerPresenceSlots, None)
    _ -> {
      let table =
        int.range(0, slot_count, [], list.prepend)
        |> list.reverse
        |> list.map(pick(nodes, _))
        |> to_slot_table
      let previous =
        get_slots(FluxerPresenceSlots, None)
        |> option.map(fn(slots) { slots.table })
      put_slots(
        FluxerPresenceSlots,
        Some(Slots(nodes:, table:, previous:, built_ms: clock.monotonic_ms())),
      )
    }
  }
}

fn current_nodes() -> Option(List(Atom)) {
  get_slots(FluxerPresenceSlots, None) |> option.map(fn(slots) { slots.nodes })
}

fn built_watchers() -> table.Table {
  table.named("fluxer_presence_route_watchers")
}

pub fn subscribe_built(to subscriber: Subject(RouteBuilt)) -> Nil {
  case
    process.subject_owner(subscriber),
    table.ensure_public(built_watchers())
  {
    Ok(pid), True -> table.insert(built_watchers(), #(pid, subscriber))
    _, _ -> Nil
  }
  ensure_builder()
}

fn notify_built() -> Nil {
  case table.ensure_public(built_watchers()) {
    False -> Nil
    True -> {
      let rows: List(#(Pid, Subject(RouteBuilt))) =
        table.to_list(built_watchers())
      list.each(rows, fn(row) {
        let #(pid, subscriber) = row
        case process.is_alive(pid) {
          True -> process.send(subscriber, RouteBuilt)
          False -> table.delete(built_watchers(), pid)
        }
      })
    }
  }
}

fn builder_name() -> Name(Builder) {
  local_name("route")
}

fn ensure_builder() -> Nil {
  case process.named(builder_name()) {
    Ok(_) -> Nil
    Error(Nil) -> {
      let _ = process.spawn_unlinked(run_builder)
      Nil
    }
  }
}

fn run_builder() -> Nil {
  case process.register(process.self(), builder_name()) {
    Error(Nil) -> Nil
    Ok(Nil) -> {
      let inbox = process.named_subject(builder_name())
      let topology = process.new_subject()
      router.subscribe_topology(topology)
      let selector =
        process.new_selector()
        |> process.select(inbox)
        |> process.select_map(topology, Topology)
      rebuild()
      builder_loop(selector, inbox, clock.monotonic_ms(), False)
    }
  }
}

fn rebuild() -> Nil {
  let nodes = presence_nodes()
  case current_nodes() == Some(nodes) {
    True -> Nil
    False -> build(nodes)
  }
  notify_built()
}

fn builder_loop(
  selector: process.Selector(Builder),
  inbox: Subject(Builder),
  last_ms: Int,
  scheduled: Bool,
) -> Nil {
  case process.selector_receive_forever(selector) {
    Rebuild -> {
      let now = clock.monotonic_ms()
      rebuild()
      builder_loop(selector, inbox, now, False)
    }
    Topology(_) -> {
      let now = clock.monotonic_ms()
      let elapsed = now - last_ms
      case elapsed >= rebuild_interval_ms(), scheduled {
        True, _ -> {
          rebuild()
          builder_loop(selector, inbox, now, False)
        }
        False, True -> builder_loop(selector, inbox, last_ms, True)
        False, False -> {
          let _ =
            process.send_after(inbox, rebuild_interval_ms() - elapsed, Rebuild)
          builder_loop(selector, inbox, last_ms, True)
        }
      }
    }
  }
}
