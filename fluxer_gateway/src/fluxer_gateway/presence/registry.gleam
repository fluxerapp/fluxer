// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/presence/cache
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/digest.{type Digest}
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event.{type PresenceEvent}
import fluxer_gateway/presence/gate
import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/lanes
import fluxer_gateway/presence/owner
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/protocol.{
  type Catchup, type Subscriber, ChangedSince, Everything, GuildSubscriber,
  Joined, PresenceSubscriber, Resend, ResendNode, ResendSince,
}
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import fluxer_gateway/presence/table
import fluxer_gateway/presence/version
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Atom) -> Atom

fn init_timeout_ms() -> Int {
  durations.presence().registry_init_timeout_ms
}

pub type RegistryMsg {
  Subscribe(
    subscriber: Subscriber,
    targets: List(Int),
    catchup: Catchup,
    bulk: Bool,
  )
  SubscribeAll(subscriber: Subscriber, targets: IdSet, catchup: Catchup)
  Unsubscribe(subscriber: Subscriber, targets: List(Int), bulk: Bool)
  UnsubscribeAll(subscriber: Subscriber, targets: IdSet)
  InterestAdd(from: Atom, targets: List(Int), resend_to: Option(Subscriber))
  CatchUpSince(subscriber: Subscriber, targets: List(Int), since_ms: Int)
  InterestRemove(from: Atom, targets: List(Int))
  DigestOffer(from: Atom, digest: Digest)
  ResyncRequest(owner: Atom)
  InterestReplace(from: Atom, targets: List(Int), last: Bool)
  DigestTick
  SweepTick
  SweepStep(keys: List(Int))
  RouteBuilt
  Release(target: Int)
  LostOwners(targets: List(Int))
}

type Shard {
  Shard(
    index: Int,
    inbox: Subject(RegistryMsg),
    ticks: Subject(RegistryMsg),
    to_owner: Dict(Atom, Digest),
    from_node: Dict(Atom, Digest),
    replacing: Dict(Atom, List(Int)),
    nodes: List(Atom),
  )
}

const step_size = 1000

const replace_chunk = 10_000

fn digest_interval_ms() -> Int {
  durations.presence().registry_digest_interval_ms
}

fn sweep_interval_ms() -> Int {
  durations.presence().registry_sweep_interval_ms
}

fn grave_ttl_ms() -> Int {
  durations.presence().registry_grave_ttl_ms
}

fn lost_owner_grace_ms() -> Int {
  durations.presence().registry_lost_owner_grace_ms
}

fn shard_name(shard: Int) -> Name(RegistryMsg) {
  route.name(route.Registry, shard)
}

fn catch_up_tag(shard: Int) -> Name(RegistryMsg) {
  route.name(route.RegistryCatchUp, shard)
}

fn subs_table(shard: Int) -> Atom {
  route.table(route.SubsTable, shard)
}

fn subs_index_table(shard: Int) -> Atom {
  route.table(route.SubsIndexTable, shard)
}

fn interest_table(shard: Int) -> Atom {
  route.table(route.InterestTable, shard)
}

fn grave_table(shard: Int) -> Atom {
  route.table(route.GravesTable, shard)
}

fn guild_table() -> Atom {
  route.table(route.GuildInterestTable, 0)
}

pub fn tables() -> List(Atom) {
  list.flatten([
    list.map(route.shards(), subs_index_table),
    list.map(route.shards(), interest_table),
    list.map(route.shards(), grave_table),
  ])
}

pub fn ordered_tables() -> List(Atom) {
  [guild_table()]
}

pub fn bag_tables() -> List(Atom) {
  list.append(list.map(route.shards(), subs_table), gate.bag_tables())
}

pub fn local_subscribers(target: Int) -> List(Subscriber) {
  table.values(subs_table(route.shard(target)), target)
}

fn subscribed_targets(shard: Int) -> List(Int) {
  table.keys(subs_table(shard)) |> list.unique
}

pub fn interest(target: Int) -> List(Atom) {
  case table.lookup(interest_table(route.shard(target)), target) {
    [#(_, row), ..] -> row
    [] -> []
  }
}

fn subscription_rows() -> Int {
  list.fold(route.shards(), 0, fn(acc, shard) {
    acc + table.size(subs_table(shard))
  })
}

fn interest_rows() -> Int {
  list.fold(route.shards(), 0, fn(acc, shard) {
    acc + table.size(interest_table(shard))
  })
}

fn home(subscriber: Subscriber) -> Int {
  case subscriber {
    PresenceSubscriber(user:, ..) -> route.shard(user)
    GuildSubscriber(guild:, ..) -> route.shard(guild)
  }
}

fn route_all(
  state: Shard,
  targets: IdSet,
  owned: fn(List(Int)) -> RegistryMsg,
  run: fn(Shard, List(Int)) -> Shard,
) -> Shard {
  let groups = id_set.group_low(targets, route.shard_of_low)
  dict.each(groups, fn(shard, group) {
    case shard == state.index {
      True -> Nil
      False -> cast_local(shard, owned(group))
    }
  })
  case dict.get(groups, state.index) {
    Ok(group) -> bounded(state, group, owned, run)
    Error(Nil) -> state
  }
}

fn cast_local(shard: Int, message: RegistryMsg) -> Nil {
  let _ = remote.send(remote.self_atom(), shard_name(shard), message)
  Nil
}

pub fn bury(target: Int) -> Nil {
  table.insert(grave_table(route.shard(target)), #(target, clock.system_ms()))
}

pub fn revive(target: Int) -> Nil {
  table.delete(grave_table(route.shard(target)), target)
}

fn buried(target: Int) -> Bool {
  table.lookup(grave_table(route.shard(target)), target) != []
}

fn prune_graves(shard: Int) -> Nil {
  let oldest = clock.system_ms() - grave_ttl_ms()
  table.rows(grave_table(shard))
  |> list.each(fn(row: #(Int, Int)) {
    case row.1 < oldest || !counted(row.0) {
      True -> table.delete_row(grave_table(shard), row)
      False -> Nil
    }
  })
}

pub fn adopt_interest(target: Int, nodes: List(Atom)) -> Nil {
  list.each(nodes, fn(from) {
    cast_local(route.shard(target), InterestAdd(from, [target], None))
  })
}

pub fn release(target: Int) -> Nil {
  cast_local(route.shard(target), Release(target))
}

pub fn subscribe_many(
  subscriber: Subscriber,
  targets: IdSet,
  resend resend: Bool,
) -> Nil {
  let catchup = case resend {
    True -> Everything
    False -> ChangedSince(ms: clock.system_ms(), stamp: version.stamp())
  }
  cast_local(home(subscriber), SubscribeAll(subscriber, targets, catchup))
}

pub fn unsubscribe_many(subscriber: Subscriber, targets: IdSet) -> Nil {
  cast_local(home(subscriber), UnsubscribeAll(subscriber, targets))
}

const header = -1

@external(erlang, "ets", "select")
fn select_users(
  table: Atom,
  spec: List(
    #(
      #(#(Int, Atom), Subject(GuildRequest)),
      List(#(Atom, Atom, Int)),
      List(Atom),
    ),
  ),
) -> List(Int)

@external(erlang, "ets", "select_delete")
fn delete_rows(
  table: Atom,
  spec: List(#(#(#(Int, Atom), Subject(GuildRequest)), List(Nil), List(Bool))),
) -> Int

fn guild_owner(key: Int) -> Result(Subject(GuildRequest), Nil) {
  case table.lookup(guild_table(), #(key, header)) {
    [#(_, subject), ..] -> Ok(subject)
    [] -> Error(Nil)
  }
}

fn interested_users(key: Int, subject: Subject(GuildRequest)) -> IdSet {
  select_users(guild_table(), [
    #(
      #(#(key, atom.create("$1")), subject),
      [
        #(atom.create(">="), atom.create("$1"), 0),
      ],
      [atom.create("$1")],
    ),
  ])
  |> id_set.from_list
}

fn forget_rows(key: Int, subject: Subject(GuildRequest)) -> Nil {
  let _ =
    delete_rows(guild_table(), [
      #(#(#(key, atom.create("_")), subject), [], [True]),
    ])
  Nil
}

pub fn guild_interest(guild: GuildId) -> List(Int) {
  let key = id.guild_id_to_int(guild)
  case guild_owner(key) {
    Ok(subject) -> id_set.to_list(interested_users(key, subject))
    Error(Nil) -> []
  }
}

pub fn register_guild(
  guild: GuildId,
  members: PackedIds,
  deliver_to: Subject(GuildRequest),
) -> Nil {
  register_guild_with(guild, members, deliver_to, resend: False)
}

pub fn register_guild_with(
  guild: GuildId,
  members: PackedIds,
  deliver_to: Subject(GuildRequest),
  resend resend: Bool,
) -> Nil {
  let key = id.guild_id_to_int(guild)
  let subscriber = GuildSubscriber(guild: key, inbox: deliver_to)
  case guild_owner(key) {
    Ok(subject) if subject == deliver_to -> Nil
    Ok(subject) -> {
      let previous = interested_users(key, subject)
      forget_rows(key, subject)
      table.insert(guild_table(), #(#(key, header), deliver_to))
      unsubscribe_many(GuildSubscriber(guild: key, inbox: subject), previous)
    }
    Error(Nil) -> table.insert(guild_table(), #(#(key, header), deliver_to))
  }
  let added =
    packed_ids.to_list(members)
    |> list.filter(fn(user) {
      user >= 0 && table.insert_new(guild_table(), #(#(key, user), deliver_to))
    })
  case added {
    [] -> Nil
    _ -> subscribe_many(subscriber, id_set.from_list(added), resend:)
  }
}

fn owned_guild(key: Int, owner: Pid) -> Result(Subject(GuildRequest), Nil) {
  use subject <- result.try(guild_owner(key))
  case process.subject_owner(subject) == Ok(owner) {
    True -> Ok(subject)
    False -> Error(Nil)
  }
}

pub fn unregister_guild(guild: GuildId, members: PackedIds) -> Nil {
  let key = id.guild_id_to_int(guild)
  case owned_guild(key, process.self()) {
    Ok(inbox) -> {
      let removed =
        packed_ids.to_list(members)
        |> list.filter(fn(user) {
          user >= 0 && table.take_row(guild_table(), #(#(key, user), inbox))
        })
      case removed {
        [] -> Nil
        _ ->
          unsubscribe_many(
            GuildSubscriber(guild: key, inbox:),
            id_set.from_list(removed),
          )
      }
    }
    Error(Nil) -> Nil
  }
}

pub fn drop_guild(guild: GuildId) -> Nil {
  drop_guild_of(guild, process.self())
}

pub fn drop_guild_of(guild: GuildId, owner: Pid) -> Nil {
  let key = id.guild_id_to_int(guild)
  case owned_guild(key, owner) {
    Ok(inbox) -> {
      let previous = interested_users(key, inbox)
      forget_rows(key, inbox)
      unsubscribe_many(GuildSubscriber(guild: key, inbox:), previous)
    }
    Error(Nil) -> Nil
  }
}

pub fn supervised(shard: Int) -> ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(init_timeout_ms(), fn(inbox) {
      let built = process.new_subject()
      route.subscribe_built(built)
      let ticks = process.new_subject()
      let selector =
        process.new_selector()
        |> process.select(inbox)
        |> process.select(process.named_subject(catch_up_tag(shard)))
        |> process.select(ticks)
        |> process.select_map(built, fn(_) { RouteBuilt })
      let _ = process.send_after(ticks, digest_interval_ms(), DigestTick)
      let _ = process.send_after(ticks, sweep_interval_ms(), SweepTick)
      let state =
        Shard(
          index: shard,
          inbox:,
          ticks:,
          to_owner: dict.new(),
          from_node: dict.new(),
          replacing: dict.new(),
          nodes: route.presence_nodes(),
        )
        |> recompute
      Ok(actor.initialised(state) |> actor.selecting(selector))
    })
    |> actor.named(shard_name(shard))
    |> actor.on_message(handle)
    |> actor.start
    |> owner.started_nil
  })
}

fn handle(
  state: Shard,
  message: RegistryMsg,
) -> actor.Next(Shard, RegistryMsg) {
  set_priority(case is_bulk(message) {
    True -> "low"
    False -> "normal"
  })
  let next = case message {
    Subscribe(subscriber, targets, catchup, bulk) ->
      bounded(
        state,
        targets,
        Subscribe(subscriber, _, catchup, bulk),
        fn(state, batch) { subscribe(state, subscriber, batch, catchup) },
      )
    SubscribeAll(subscriber, targets, catchup) ->
      route_all(
        state,
        targets,
        Subscribe(subscriber, _, catchup, is_bulk(message)),
        fn(state, batch) { subscribe(state, subscriber, batch, catchup) },
      )
    UnsubscribeAll(subscriber, targets) ->
      route_all(
        state,
        targets,
        Unsubscribe(subscriber, _, is_bulk(message)),
        fn(state, batch) { unsubscribe(state, subscriber, batch) },
      )
    Unsubscribe(subscriber, targets, bulk) ->
      bounded(
        state,
        targets,
        Unsubscribe(subscriber, _, bulk),
        fn(state, batch) { unsubscribe(state, subscriber, batch) },
      )
    InterestAdd(from, targets, resend_to) ->
      bounded(
        state,
        targets,
        fn(rest) { InterestAdd(from, rest, resend_to) },
        fn(state, batch) {
          interest_add(state, from, batch)
          |> resend_everything(batch, resend_to)
        },
      )
    CatchUpSince(subscriber, targets, since_ms) ->
      bounded(
        state,
        targets,
        fn(rest) { CatchUpSince(subscriber, rest, since_ms) },
        fn(state, batch) {
          list.each(batch, catch_up_since(_, subscriber, since_ms))
          state
        },
      )
    InterestRemove(from, targets) -> interest_remove(state, from, targets)
    DigestOffer(from, offered) -> digest_offer(state, from, offered)
    ResyncRequest(owner) -> resync(state, owner)
    InterestReplace(from, targets, last) ->
      interest_replace(state, from, targets, last)
    DigestTick -> digest_tick(state)
    SweepTick -> {
      let _ = process.send_after(state.ticks, sweep_interval_ms(), SweepTick)
      prune_graves(state.index)
      process.send(state.inbox, SweepStep(subscribed_targets(state.index)))
      state
    }
    SweepStep(keys) -> sweep(state, keys)
    RouteBuilt -> topology_changed(state)
    Release(target) -> release_interest(state, target)
    LostOwners(targets) -> {
      announce_offline(targets)
      state
    }
  }
  actor.continue(next)
}

fn is_bulk(message: RegistryMsg) -> Bool {
  case message {
    SubscribeAll(targets:, ..) | UnsubscribeAll(targets:, ..) ->
      id_set.size(targets) > route.shard_count
    Subscribe(bulk:, ..) | Unsubscribe(bulk:, ..) -> bulk
    _ -> False
  }
}

fn set_priority(level: String) -> Nil {
  let _ = process_flag(atom.create("priority"), atom.create(level))
  Nil
}

fn bounded(
  state: Shard,
  targets: List(Int),
  rest_message: fn(List(Int)) -> RegistryMsg,
  run: fn(Shard, List(Int)) -> Shard,
) -> Shard {
  let #(batch, rest) = list.split(targets, step_size)
  case rest {
    [] -> Nil
    _ -> process.send(state.inbox, rest_message(rest))
  }
  run(state, batch)
}

fn owner_of(target: Int) -> Option(Atom) {
  route.owner(target) |> option.from_result
}

fn counted(target: Int) -> Bool {
  owner_of(target) == Some(remote.self_atom())
}

fn hosted(target: Int) -> Bool {
  result.is_ok(pids.lookup(target))
}

fn append_to(
  groups: Dict(Atom, List(Int)),
  key: Atom,
  value: Int,
) -> Dict(Atom, List(Int)) {
  dict.upsert(groups, key, fn(existing) {
    [value, ..option.unwrap(existing, [])]
  })
}

fn subscribe(
  state: Shard,
  subscriber: Subscriber,
  targets: List(Int),
  catchup: Catchup,
) -> Shard {
  let self = remote.self_atom()
  let #(state, interest_adds) =
    list.fold(targets, #(state, dict.new()), fn(acc, target) {
      let #(state, adds) = acc
      let fresh =
        !table.member(subs_index_table(state.index), #(target, subscriber))
      case owner_of(target) {
        Some(owner) if owner == self -> {
          join_local(state.index, target, subscriber, catchup, fresh)
          acc
        }
        Some(owner) -> {
          let was_empty =
            fresh && insert_subscription(state.index, target, subscriber)
          let state = case was_empty {
            True ->
              Shard(
                ..state,
                to_owner: update_digest(state.to_owner, owner, digest.add(
                  _,
                  target,
                )),
              )
            False -> state
          }
          #(state, append_to(adds, owner, target))
        }
        None -> {
          case fresh {
            True -> {
              let _ = insert_subscription(state.index, target, subscriber)
              Nil
            }
            False -> Nil
          }
          acc
        }
      }
    })
  dict.each(interest_adds, fn(owner, owned) {
    let owned = list.reverse(owned)
    case catchup {
      Everything ->
        send_shard(
          owner,
          state.index,
          InterestAdd(self, owned, Some(subscriber)),
        )
      ChangedSince(ms:, ..) -> {
        send_shard(owner, state.index, InterestAdd(self, owned, None))
        case
          remote.send_tagged(
            owner,
            shard_name(state.index),
            catch_up_tag(state.index),
            CatchUpSince(subscriber, owned, ms),
          )
        {
          Ok(Nil) -> Nil
          Error(_) -> counters.incr(counters.RemoteSendFailed)
        }
      }
    }
  })
  state
}

fn insert_subscription(
  shard: Int,
  target: Int,
  subscriber: Subscriber,
) -> Bool {
  let subs = subs_table(shard)
  let was_empty = !table.member(subs, target)
  table.insert(subs, #(target, subscriber))
  table.insert(subs_index_table(shard), #(#(target, subscriber)))
  lanes.joined(target, subscriber, local_subscribers)
  was_empty
}

fn join_local(
  shard: Int,
  target: Int,
  subscriber: Subscriber,
  catchup: Catchup,
  fresh: Bool,
) -> Nil {
  report_buried(target, subscriber)
  case fresh, catchup {
    True, _ -> {
      gate.hold(target, subscriber)
      let _ = insert_subscription(shard, target, subscriber)
      case pids.lookup(target) {
        Ok(inbox) -> process.send(inbox, Joined(subscriber, catchup))
        Error(Nil) -> gate.open(target, subscriber)
      }
    }
    False, Everything -> resend_local(target, subscriber)
    False, ChangedSince(..) -> Nil
  }
}

fn report_buried(target: Int, subscriber: Subscriber) -> Nil {
  case buried(target) {
    True -> lanes.deliver(subscriber, offline_event(target))
    False -> Nil
  }
}

fn catch_up_everything(target: Int, subscriber: Subscriber) -> Nil {
  report_buried(target, subscriber)
  resend_local(target, subscriber)
}

fn catch_up_since(target: Int, subscriber: Subscriber, since_ms: Int) -> Nil {
  report_buried(target, subscriber)
  case cache.published_ms(target) {
    Ok(published) if published < since_ms -> Nil
    _ -> resend_since_local(target, subscriber, since_ms)
  }
}

fn resend_everything(
  state: Shard,
  targets: List(Int),
  resend_to: Option(Subscriber),
) -> Shard {
  case resend_to {
    Some(subscriber) -> list.each(targets, catch_up_everything(_, subscriber))
    None -> Nil
  }
  state
}

fn resend_local(target: Int, subscriber: Subscriber) -> Nil {
  case pids.lookup(target) {
    Ok(inbox) -> process.send(inbox, Resend(subscriber))
    Error(Nil) -> Nil
  }
}

fn resend_since_local(
  target: Int,
  subscriber: Subscriber,
  since_ms: Int,
) -> Nil {
  case pids.lookup(target) {
    Ok(inbox) -> process.send(inbox, ResendSince(subscriber, since_ms))
    Error(Nil) -> Nil
  }
}

fn send_shard(owner: Atom, shard: Int, message: RegistryMsg) -> Nil {
  case remote.send(owner, shard_name(shard), message) {
    Ok(Nil) -> Nil
    Error(_) -> counters.incr(counters.RemoteSendFailed)
  }
}

fn update_digest(
  digests: Dict(Atom, Digest),
  key: Atom,
  change: fn(Digest) -> Digest,
) -> Dict(Atom, Digest) {
  dict.upsert(digests, key, fn(existing) {
    change(option.unwrap(existing, digest.empty()))
  })
}

fn unsubscribe(
  state: Shard,
  subscriber: Subscriber,
  targets: List(Int),
) -> Shard {
  let self = remote.self_atom()
  let #(state, removes) =
    list.fold(targets, #(state, dict.new()), fn(acc, target) {
      let #(state, removes) = acc
      let subs = subs_table(state.index)
      let index = subs_index_table(state.index)
      let key = #(target, subscriber)
      let next = case table.member(index, key) {
        False -> {
          table.delete_row(subs, key)
          acc
        }
        True -> {
          table.delete(index, key)
          table.delete_row(subs, key)
          lanes.left(target, subscriber)
          case table.member(subs, target) {
            False ->
              case owner_of(target) {
                Some(owner) if owner != self -> #(
                  Shard(
                    ..state,
                    to_owner: update_digest(
                      state.to_owner,
                      owner,
                      digest.remove(_, target),
                    ),
                  ),
                  append_to(removes, owner, target),
                )
                _ -> acc
              }
            True -> acc
          }
        }
      }
      gate.drop(target, subscriber)
      next
    })
  dict.each(removes, fn(owner, owned) {
    send_shard(owner, state.index, InterestRemove(self, owned))
  })
  state
}

fn sorted_nodes(nodes: List(Atom)) -> List(Atom) {
  list.sort(nodes, fn(a, b) {
    string.compare(atom.to_string(a), atom.to_string(b))
  })
}

fn interest_add(state: Shard, from: Atom, targets: List(Int)) -> Shard {
  list.fold(targets, state, fn(state, target) {
    let row = interest(target)
    case list.contains(row, from) {
      True -> state
      False -> {
        table.insert(interest_table(state.index), #(
          target,
          sorted_nodes([from, ..row]),
        ))
        case counted(target) {
          True ->
            Shard(
              ..state,
              from_node: update_digest(state.from_node, from, digest.add(
                _,
                target,
              )),
            )
          False -> state
        }
      }
    }
  })
}

fn drop_interest(state: Shard, from: Atom, target: Int) -> Shard {
  let row = interest(target)
  case list.contains(row, from) {
    False -> state
    True -> {
      case list.filter(row, fn(found) { found != from }) {
        [] -> table.delete(interest_table(state.index), target)
        kept -> table.insert(interest_table(state.index), #(target, kept))
      }
      case counted(target) {
        True ->
          Shard(
            ..state,
            from_node: update_digest(state.from_node, from, digest.remove(
              _,
              target,
            )),
          )
        False -> state
      }
    }
  }
}

fn interest_remove(state: Shard, from: Atom, targets: List(Int)) -> Shard {
  list.fold(targets, state, fn(state, target) {
    drop_interest(state, from, target)
  })
}

fn digest_offer(state: Shard, from: Atom, offered: Digest) -> Shard {
  let known = dict.get(state.from_node, from) |> result.unwrap(digest.empty())
  case known == offered {
    True -> Nil
    False -> {
      counters.incr(counters.InterestResync)
      send_shard(from, state.index, ResyncRequest(remote.self_atom()))
    }
  }
  state
}

fn resync(state: Shard, owner: Atom) -> Shard {
  let owned =
    subscribed_targets(state.index)
    |> list.filter(fn(target) { owner_of(target) == Some(owner) })
  let chunks = case list.sized_chunk(owned, replace_chunk) {
    [] -> [[]]
    found -> found
  }
  let total = list.length(chunks)
  let _ =
    list.index_map(chunks, fn(chunk, index) {
      send_shard(
        owner,
        state.index,
        InterestReplace(remote.self_atom(), chunk, index == total - 1),
      )
    })
  Shard(..state, to_owner: dict.insert(state.to_owner, owner, digest.of(owned)))
}

fn interest_replace(
  state: Shard,
  from: Atom,
  targets: List(Int),
  last: Bool,
) -> Shard {
  let gathered =
    list.append(dict.get(state.replacing, from) |> result.unwrap([]), targets)
  case last {
    False ->
      Shard(..state, replacing: dict.insert(state.replacing, from, gathered))
    True -> {
      let wanted = id_set.from_list(gathered)
      let held =
        table.rows(interest_table(state.index))
        |> list.filter_map(fn(row: #(Int, List(Atom))) {
          case list.contains(row.1, from) && counted(row.0) {
            True -> Ok(row.0)
            False -> Error(Nil)
          }
        })
        |> id_set.from_list
      let #(added, removed) = id_set.diff(old: held, new: wanted)
      let state =
        id_set.to_list(removed)
        |> list.fold(state, fn(state, target) {
          drop_interest(state, from, target)
        })
      let state = interest_add(state, from, id_set.to_list(added))
      list.each(id_set.to_list(added), fn(target) {
        case pids.lookup(target) {
          Ok(inbox) -> process.send(inbox, ResendNode(from))
          Error(Nil) -> Nil
        }
      })
      Shard(
        ..state,
        replacing: dict.delete(state.replacing, from),
        from_node: dict.insert(
          state.from_node,
          from,
          digest.of(id_set.to_list(wanted)),
        ),
      )
    }
  }
}

fn presence_peers() -> List(Atom) {
  let self = remote.self_atom()
  router.nodes_with(node.Presence)
  |> list.map(node.to_atom)
  |> list.filter(fn(found) { found != self })
}

fn digest_tick(state: Shard) -> Shard {
  let _ = process.send_after(state.ticks, digest_interval_ms(), DigestTick)
  let self = remote.self_atom()
  list.each(presence_peers(), fn(peer) {
    let offered =
      dict.get(state.to_owner, peer) |> result.unwrap(digest.empty())
    send_shard(peer, state.index, DigestOffer(self, offered))
  })
  case state.index {
    0 -> {
      counters.interest_rows(interest_rows())
      counters.subscription_rows(subscription_rows())
    }
    _ -> Nil
  }
  state
}

fn sweep(state: Shard, keys: List(Int)) -> Shard {
  let #(batch, rest) = list.split(keys, step_size)
  case rest {
    [] -> Nil
    _ -> process.send(state.inbox, SweepStep(rest))
  }
  list.fold(batch, state, fn(state, target) {
    list.fold(local_subscribers(target), state, fn(state, subscriber) {
      case subscriber_alive(subscriber) {
        True -> state
        False -> unsubscribe(state, subscriber, [target])
      }
    })
  })
}

fn subscriber_alive(subscriber: Subscriber) -> Bool {
  let owner = case subscriber {
    PresenceSubscriber(inbox:, ..) -> process.subject_owner(inbox)
    GuildSubscriber(inbox:, ..) -> process.subject_owner(inbox)
  }
  case owner {
    Ok(pid) -> process.is_alive(pid)
    Error(Nil) -> False
  }
}

fn live_nodes() -> List(Atom) {
  list.append(router.nodes_with(node.Presence), router.nodes_with(node.Guilds))
  |> list.map(node.to_atom)
  |> list.unique
}

fn topology_changed(state: Shard) -> Shard {
  let self = remote.self_atom()
  let live = [self, ..live_nodes()]
  table.rows(interest_table(state.index))
  |> list.each(fn(row: #(Int, List(Atom))) {
    let #(target, nodes) = row
    let kept = list.filter(nodes, list.contains(live, _))
    case counted(target) || hosted(target), kept {
      False, _ | True, [] -> table.delete(interest_table(state.index), target)
      True, _ ->
        case kept == nodes {
          True -> Nil
          False -> table.insert(interest_table(state.index), #(target, kept))
        }
    }
  })
  let state = state |> track_lost_owners |> recompute
  dict.each(owned_by_remote(state.index), fn(owner, owned) {
    owned
    |> list.sized_chunk(step_size)
    |> list.each(fn(chunk) {
      send_shard(owner, state.index, InterestAdd(self, chunk, None))
    })
  })
  state
}

fn track_lost_owners(state: Shard) -> Shard {
  let self = remote.self_atom()
  let current = route.presence_nodes()
  let lost =
    list.filter(state.nodes, fn(found) {
      found != self && !list.contains(current, found)
    })
  case lost {
    [] -> Nil
    _ ->
      case
        subscribed_targets(state.index)
        |> list.filter(fn(target) {
          case route.owner_among(target, state.nodes) {
            Some(previous) -> list.contains(lost, previous)
            None -> False
          }
        })
      {
        [] -> Nil
        orphaned -> {
          let _ =
            process.send_after(
              state.inbox,
              lost_owner_grace_ms(),
              LostOwners(orphaned),
            )
          Nil
        }
      }
  }
  Shard(..state, nodes: current)
}

fn announce_offline(targets: List(Int)) -> Nil {
  let _ =
    process.spawn_unlinked(fn() {
      targets
      |> list.filter(fn(target) { local_subscribers(target) != [] })
      |> cache.absent
      |> list.each(fn(target) {
        let offline = offline_event(target)
        list.each(local_subscribers(target), lanes.deliver(_, offline))
      })
    })
  Nil
}

fn offline_event(target: Int) -> PresenceEvent {
  event.status_changed(
    user: target,
    version: version.first(version.new_epoch()),
    display: display.offline(),
    custom_status: <<"null":utf8>>,
    user_wire: <<
      "{\"id\":\"":utf8,
      id.write(target):utf8,
      "\"}":utf8,
    >>,
  )
}

fn release_interest(state: Shard, target: Int) -> Shard {
  case counted(target) || hosted(target) {
    True -> Nil
    False -> table.delete(interest_table(state.index), target)
  }
  state
}

fn owned_by_remote(shard: Int) -> Dict(Atom, List(Int)) {
  let self = remote.self_atom()
  subscribed_targets(shard)
  |> list.fold(dict.new(), fn(acc, target) {
    case owner_of(target) {
      Some(owner) if owner != self -> append_to(acc, owner, target)
      _ -> acc
    }
  })
}

fn recompute(state: Shard) -> Shard {
  let to_owner =
    owned_by_remote(state.index)
    |> dict.map_values(fn(_, owned) { digest.of(owned) })
  let from_node =
    table.rows(interest_table(state.index))
    |> list.fold(dict.new(), fn(acc, row: #(Int, List(Atom))) {
      case counted(row.0) {
        True ->
          list.fold(row.1, acc, fn(acc, from) {
            update_digest(acc, from, digest.add(_, row.0))
          })
        False -> acc
      }
    })
  Shard(..state, to_owner:, from_node:)
}
