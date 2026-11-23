// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/presence
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/native
import fluxer_gateway/member_list/snapshot as ml_snapshot
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/event
import fluxer_gateway/perms/native as perms_native
import fluxer_gateway/perms/snapshot
import gleam/bit_array
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/nif_probe
import support/perms_ml_fixture as f
import support/perms_ml_harness as h
import support/wait

const everyone = 100

const secret_role = 200

const public = 11

const gated = 12

const online = 300

type Fixture {
  Fixture(
    perms: perms.Perms,
    reader: perms.Reader,
    members: member_list.MemberList,
    counts: member_list.CountsHandle,
  )
}

fn grants() -> Int {
  f.view + 2048 + f.view_members
}

fn channel(open: Bool) {
  case open {
    True -> f.text(gated, [])
    False ->
      f.text(gated, [
        f.deny_role(everyone, f.view),
        f.allow_role(secret_role, f.view),
      ])
  }
}

fn roles_of(user: Int) -> List(Int) {
  case user % 2 {
    0 -> [secret_role]
    _ -> []
  }
}

fn users(total: Int) -> List(Int) {
  int.range(total, 0, [], fn(acc, user) { [user, ..acc] })
}

fn lookup(found: List(id.UserId)) -> List(#(id.UserId, BitArray, BitArray)) {
  list.map(found, fn(user) {
    let value = id.user_id_to_int(user)
    let #(list_bytes, page_bytes) =
      item.encode(f.member(value, roles_of(value)), None, False)
    #(user, list_bytes, page_bytes)
  })
}

fn build(total: Int, tuning: config.Config) -> Fixture {
  let assert Ok(Nil) = nif.load()
  let source =
    list.fold(
      users(total),
      f.base([f.role(everyone, grants(), 0), f.role(secret_role, 0, 1)], [
        f.text(public, []),
        channel(False),
      ]),
      fn(acc, user) { snapshot.add_member(acc, f.member(user, roles_of(user))) },
    )
  let #(handle, reader) = perms.new(f.guild(100), process.self())
  let assert Ok(_) = perms.load(handle, source)
  let #(members, counts) =
    member_list.new(f.guild(100), tuning, process.self(), handle)
  let loaded =
    list.fold(users(total), ml_snapshot.new(), fn(acc, user) {
      let acc = ml_snapshot.add_member(acc, f.member(user, roles_of(user)))
      case user <= online {
        True ->
          ml_snapshot.add_presence(acc, h.status(user, presence.Online))
          |> ml_snapshot.add_connected(f.user_id(user))
        False -> acc
      }
    })
  let assert Ok(Nil) = member_list.load(members, loaded)
  Fixture(perms: handle, reader:, members:, counts:)
}

fn ref(user: Int) -> SessionRef {
  SessionRef(
    id: session.new_session_id(),
    user: f.user_id(user),
    inbox: process.new_subject(),
  )
}

fn subscribe(fixture: Fixture, user: Int, channel: Int) -> SessionRef {
  let found = ref(user)
  let assert Ok(_) =
    member_list.subscribe(
      fixture.members,
      found,
      f.channel_id(channel),
      [#(0, 99)],
      lookup,
    )
  found
}

fn masks(fixture: Fixture, users: List(Int)) -> Nil {
  let updates =
    list.map(users, fn(user) {
      native.MaskUpdate(
        user:,
        all_messages: user % 3 == 0,
        uncertain: user % 3 == 1,
        muted: False,
        active: False,
      )
    })
  let assert Ok(Nil) =
    native.push_masks_set(member_list.native_handle(fixture.members), updates)
  Nil
}

fn step(fixture: Fixture, round: Int) -> Nil {
  let assert Ok(effects) =
    perms.apply(fixture.perms, [
      event.channel_upsert(channel(round % 2 == 0), event.Updated),
    ])
  let user = 1 + round % online
  let status = case round % 3 {
    0 -> presence.Idle
    1 -> presence.Dnd
    _ -> presence.Online
  }
  let assert Ok(_) =
    member_list.apply_timed(
      fixture.members,
      [
        list_event.presence(
          f.user_id(user),
          status,
          list_event.Changed,
          list_event.Immediate,
        ),
      ],
      Some(perms.delta(effects)),
      lookup,
    )
  masks(fixture, [user, user + 1])
}

fn classify(found: Result(native.MaskReply, perms_native.NifError)) -> String {
  case found {
    Ok(native.Masks(..)) | Ok(native.Stale) | Error(perms_native.NeedsDirty) ->
      ""
    Error(perms_native.Gone) -> "gone"
    Error(perms_native.Broken) -> "broken"
    Error(perms_native.BadInput(reason)) -> "bad_input " <> reason
  }
}

fn push_worker(
  fixture: Fixture,
  rounds: Int,
  results: Subject(List(String)),
) -> Nil {
  let handle = member_list.native_handle(fixture.members)
  let problems =
    int.range(0, rounds, [], fn(acc, _) {
      let epoch = perms.epoch(fixture.reader)
      let masked =
        classify(native.push_masks_query(handle, native.Supersets(epoch)))
      let #(total, _) = member_list.counts(fixture.counts)
      let counted = case total >= 0 {
        True -> ""
        False -> "counts " <> int.to_string(total)
      }
      list.filter([masked, counted, ..acc], fn(problem) { problem != "" })
    })
  process.send(results, problems)
}

fn spawn_workers(
  fixture: Fixture,
  workers: Int,
  rounds: Int,
) -> Subject(List(String)) {
  let results = process.new_subject()
  int.range(0, workers, Nil, fn(_, _) {
    process.spawn(fn() { push_worker(fixture, rounds, results) })
    Nil
  })
  results
}

fn page_worker(
  fixture: Fixture,
  rounds: Int,
  results: Subject(List(String)),
) -> Nil {
  let handle = member_list.native_handle(fixture.members)
  let problems =
    int.range(0, rounds, [], fn(acc, _) {
      case native.ml_query_dirty(handle, native.PageIds(0, 3000)) {
        Ok(native.IdsReply(_)) | Error(perms_native.Gone) -> acc
        other -> [string.inspect(other), ..acc]
      }
    })
  process.send(results, problems)
}

fn collect(results: Subject(List(String)), workers: Int) -> List(String) {
  int.range(0, workers, [], fn(acc, _) {
    let assert Ok(found) = process.receive(results, 120_000)
    list.append(found, acc)
  })
}

fn without_gone(problems: List(String)) -> List(String) {
  list.filter(problems, fn(problem) { problem != "gone" })
}

pub fn push_workers_query_masks_while_the_owner_applies_test() {
  let fixture = build(3000, config.default())
  masks(fixture, users(3000))
  let _ = subscribe(fixture, 2, public)
  let _ = subscribe(fixture, 4, gated)
  let results = spawn_workers(fixture, 24, 200)
  int.range(0, 150, Nil, fn(_, round) { step(fixture, round) })
  assert collect(results, 24) == []
  let epoch = perms.epoch(fixture.reader)
  let assert Ok(native.Masks(immediate:, held:)) =
    native.push_masks_query(
      member_list.native_handle(fixture.members),
      native.Supersets(epoch),
    )
  assert packed_ids.to_list(immediate) != [] || packed_ids.to_list(held) != []
  let assert Ok(ids) = member_list.page_ids(fixture.members, 0, 10)
  assert packed_ids.to_list(ids) != []
  member_list.release(fixture.members)
  perms.release(fixture.perms)
}

pub fn owner_killed_while_push_workers_query_test() {
  let handed = process.new_subject()
  let owner =
    process.spawn_unlinked(fn() {
      process.send(handed, build(3000, config.default()))
      process.sleep_forever()
    })
  let assert Ok(fixture) = process.receive(handed, 60_000)
  let live = nif_probe.metric("member_list_core_live")
  let results = spawn_workers(fixture, 16, 300)
  process.sleep(5)
  process.kill(owner)
  assert without_gone(collect(results, 16)) == []
  let handle = member_list.native_handle(fixture.members)
  assert wait.until(2000, fn() {
    native.push_masks_query(handle, native.Supersets(0))
    == Error(perms_native.Gone)
  })
  assert member_list.page_ids(fixture.members, 0, 10)
    == Error(member_list.ListBroken)
  assert nif_probe.metric("member_list_core_live") <= live
  let #(total, _) = member_list.counts(fixture.counts)
  assert total == 3000
}

pub fn release_while_push_workers_query_test() {
  let fixture = build(3000, config.default())
  masks(fixture, users(3000))
  let results = spawn_workers(fixture, 16, 300)
  process.sleep(5)
  member_list.release(fixture.members)
  assert without_gone(collect(results, 16)) == []
  assert native.push_masks_query(
      member_list.native_handle(fixture.members),
      native.Supersets(perms.epoch(fixture.reader)),
    )
    == Error(perms_native.Gone)
  perms.release(fixture.perms)
}

pub fn rebuild_leaves_push_workers_on_the_old_handle_gone_test() {
  let fixture = build(2000, config.default())
  let watcher = subscribe(fixture, 2, public)
  let results = spawn_workers(fixture, 8, 300)
  let #(fresh_perms, _) = perms.new(f.guild(100), process.self())
  let source =
    list.fold(
      users(2000),
      f.base([f.role(everyone, grants(), 0), f.role(secret_role, 0, 1)], [
        f.text(public, []),
        channel(False),
      ]),
      fn(acc, user) { snapshot.add_member(acc, f.member(user, roles_of(user))) },
    )
  let assert Ok(_) = perms.load(fresh_perms, source)
  let loaded =
    list.fold(users(2000), ml_snapshot.new(), fn(acc, user) {
      ml_snapshot.add_member(acc, f.member(user, roles_of(user)))
    })
  let assert Ok(#(rebuilt, _, dispatches)) =
    member_list.rebuild(fixture.members, fresh_perms, loaded, lookup)
  assert without_gone(collect(results, 8)) == []
  assert list.any(dispatches, fn(found) { list.contains(found.to, watcher) })
  assert member_list.subscribed_channel(rebuilt, watcher.id)
    == Some(f.channel_id(public))
  member_list.release(rebuilt)
  perms.release(fresh_perms)
  perms.release(fixture.perms)
}

pub fn over_budget_list_work_moves_to_dirty_schedulers_test() {
  let tuning = config.default() |> config.set_work_budget_units(500)
  let before = nif_probe.dirty_calls()
  let fixture = build(3000, tuning)
  assert nif_probe.dirty_calls() > before
  let handle = member_list.native_handle(fixture.members)
  assert native.ml_query(handle, native.PageIds(0, 20_000))
    == native.ml_query_dirty(handle, native.PageIds(0, 20_000))
  let assert Ok(native.IdsReply(ids)) =
    native.ml_query_dirty(handle, native.PageIds(0, 20_000))
  assert list.length(packed_ids.to_list(ids)) == 3000
  let watcher = subscribe(fixture, 2, public)
  let before = nif_probe.dirty_calls()
  step(fixture, 0)
  step(fixture, 1)
  assert nif_probe.dirty_calls() > before
  assert member_list.subscribed_channel(fixture.members, watcher.id)
    == Some(f.channel_id(public))
  member_list.release(fixture.members)
  perms.release(fixture.perms)
}

pub fn rebuild_keeps_subscriptions_while_a_push_worker_holds_the_list_test() {
  let fixture = build(3000, config.default())
  masks(fixture, users(3000))
  let watchers =
    int.range(1, 41, [], fn(acc, user) {
      [subscribe(fixture, 2 * user, public), ..acc]
    })
  let results = spawn_workers(fixture, 16, 400)
  let #(fresh_perms, _) = perms.new(f.guild(100), process.self())
  let source =
    list.fold(
      users(3000),
      f.base([f.role(everyone, grants(), 0), f.role(secret_role, 0, 1)], [
        f.text(public, []),
        channel(False),
      ]),
      fn(acc, user) { snapshot.add_member(acc, f.member(user, roles_of(user))) },
    )
  let assert Ok(_) = perms.load(fresh_perms, source)
  let loaded =
    list.fold(users(3000), ml_snapshot.new(), fn(acc, user) {
      ml_snapshot.add_member(acc, f.member(user, roles_of(user)))
    })
  let assert Ok(#(rebuilt, _, _)) =
    member_list.rebuild(fixture.members, fresh_perms, loaded, lookup)
  let _ = collect(results, 16)
  let kept =
    list.filter(watchers, fn(watcher) {
      member_list.subscribed_channel(rebuilt, watcher.id)
      == Some(f.channel_id(public))
    })
  assert list.length(kept) == list.length(watchers)
  member_list.release(rebuilt)
  perms.release(fresh_perms)
  perms.release(fixture.perms)
}

pub fn subscriptions_read_never_blocks_behind_a_dirty_holder_test() {
  let fixture = build(3000, config.default())
  let watchers =
    int.range(1, 41, [], fn(acc, user) {
      [subscribe(fixture, 2 * user, public), ..acc]
    })
  let results = process.new_subject()
  int.range(0, 8, Nil, fn(_, _) {
    process.spawn(fn() { page_worker(fixture, 300, results) })
    Nil
  })
  let handle = member_list.native_handle(fixture.members)
  let answers =
    int.range(0, 300, [], fn(acc, _) {
      case native.ml_query(handle, native.Subscriptions) {
        Ok(native.SubscriptionsReply(found)) ->
          case list.length(found) == list.length(watchers) {
            True -> acc
            False -> ["short " <> int.to_string(list.length(found)), ..acc]
          }
        Error(perms_native.NeedsDirty) -> acc
        other -> [string.inspect(other), ..acc]
      }
    })
  assert answers == []
  assert collect(results, 8) == []
  let assert Ok(native.SubscriptionsReply(found)) =
    native.ml_query_dirty(handle, native.Subscriptions)
  assert list.length(found) == list.length(watchers)
  member_list.release(fixture.members)
  perms.release(fixture.perms)
}

pub fn a_normal_page_of_ids_charges_its_timeslice_test() {
  let fixture = build(8000, config.default())
  let handle = member_list.native_handle(fixture.members)
  let #(found, spent) =
    nif_probe.charged(fn() { native.ml_query(handle, native.PageIds(0, 8000)) })
  let assert Ok(native.IdsReply(ids)) = found
  assert bit_array.byte_size(packed_ids.to_bits(ids)) == 64_000
  assert spent >= 1000
  member_list.release(fixture.members)
  perms.release(fixture.perms)
}

fn hold_until_gone(handle: native.MlHandle, total: Int, rounds: Int) -> String {
  case rounds, native.ml_query_dirty(handle, native.PageIds(0, total)) {
    _, Error(perms_native.Gone) -> ""
    0, _ -> "never gone"
    _, Ok(native.IdsReply(_)) -> hold_until_gone(handle, total, rounds - 1)
    _, other -> string.inspect(other)
  }
}

pub fn owner_killed_while_workers_hold_the_list_frees_its_core_test() {
  let bytes = nif_probe.metric("member_list_core_bytes")
  let handed = process.new_subject()
  let owner =
    process.spawn_unlinked(fn() {
      process.send(handed, build(20_000, config.default()))
      process.sleep_forever()
    })
  let assert Ok(fixture) = process.receive(handed, 60_000)
  let added = nif_probe.metric("member_list_core_bytes") - bytes
  assert added > 0
  let handle = member_list.native_handle(fixture.members)
  let results = process.new_subject()
  int.range(0, 8, Nil, fn(_, _) {
    process.spawn(fn() {
      process.send(results, [hold_until_gone(handle, 20_000, 100_000)])
    })
    Nil
  })
  process.sleep(20)
  let before_kill = nif_probe.metric("member_list_core_bytes")
  process.kill(owner)
  assert list.filter(collect(results, 8), fn(found) { found != "" }) == []
  assert wait.until(2000, fn() {
    nif_probe.metric("member_list_core_bytes") <= before_kill - added
  })
  let #(total, _) = member_list.counts(fixture.counts)
  assert total == 20_000
}
