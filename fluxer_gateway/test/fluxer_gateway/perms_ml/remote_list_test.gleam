// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config
import fluxer_gateway/member_list/effects
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/snapshot as ml_snapshot
import fluxer_gateway/perms
import fluxer_gateway/perms/event
import fluxer_gateway/perms/snapshot
import gleam/bit_array
import gleam/dynamic
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import support/perms_ml_fixture as f

const public = 11

const private = 12

const secret = 900

@external(erlang, "erlang", "binary_to_term")
fn binary_to_pid(bytes: BitArray) -> Pid

fn remote_ref(user: Int, serial: Int) -> SessionRef {
  let pid =
    binary_to_pid(<<
      131, 88, 119, 10, "other@host":utf8, serial:size(32), 0:size(32),
      1:size(32),
    >>)
  SessionRef(
    id: session.new_session_id(),
    user: f.user_id(user),
    inbox: process.unsafely_create_subject(pid, dynamic.string("member_list")),
  )
}

fn grants() -> Int {
  f.view + 2048 + f.view_members
}

fn setup() -> #(perms.Perms, member_list.MemberList, member_list.Lookup) {
  let source =
    f.base([f.role(100, grants(), 0), f.role(secret, 0, 1)], [
      f.text(public, []),
      f.text(private, [f.deny_role(100, f.view), f.allow_role(secret, f.view)]),
    ])
    |> snapshot.add_member(f.member(1, [secret]))
    |> snapshot.add_member(f.member(2, [secret]))
    |> snapshot.add_member(f.member(3, []))
  let #(handle, _) = f.loaded(source)
  let #(members, _) =
    member_list.new(f.guild(100), config.default(), process.self(), handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      [1, 2, 3]
        |> list.fold(ml_snapshot.new(), fn(acc, user) {
          ml_snapshot.add_member(acc, f.member(user, []))
        }),
    )
  let lookup = fn(users) {
    list.map(users, fn(user) {
      let #(list_bytes, page_bytes) =
        item.encode(f.member(id.user_id_to_int(user), []), None, False)
      #(user, list_bytes, page_bytes)
    })
  }
  #(handle, members, lookup)
}

fn rows(found: effects.Dispatch) -> String {
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(found.payload))
  text
}

fn to(dispatches: List(effects.Dispatch), ref: SessionRef) -> List(String) {
  dispatches
  |> list.filter(fn(found) { list.contains(found.to, ref) })
  |> list.map(rows)
}

pub fn permission_change_resyncs_remote_subscribers_test() {
  let #(handle, members, lookup) = setup()
  let one = remote_ref(1, 1)
  let two = remote_ref(2, 2)
  let three = remote_ref(3, 3)
  let assert Ok(_) =
    member_list.subscribe(
      members,
      one,
      f.channel_id(private),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(_) =
    member_list.subscribe(
      members,
      two,
      f.channel_id(private),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(_) =
    member_list.subscribe(
      members,
      three,
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(effects) =
    perms.apply(handle, [
      event.channel_upsert(
        f.text(private, [
          f.deny_role(100, f.view),
          f.allow_role(secret, f.view),
          f.overwrite_member_deny(2, f.view),
        ]),
        event.Updated,
      ),
    ])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  let assert [synced] = to(dispatches, one)
  assert !string.contains(synced, "\"id\":\"2\"")
  assert string.contains(synced, "\"id\":\"1\"")
  assert to(dispatches, two) == []
  assert to(dispatches, three) == []
  let assert Ok(#(again, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  assert again == []
  member_list.release(members)
  perms.release(handle)
}

pub fn role_change_resyncs_every_list_once_test() {
  let #(handle, members, lookup) = setup()
  let one = remote_ref(1, 4)
  let three = remote_ref(3, 5)
  let assert Ok(_) =
    member_list.subscribe(
      members,
      one,
      f.channel_id(private),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(_) =
    member_list.subscribe(
      members,
      three,
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(effects) =
    perms.apply(handle, [
      event.roles([f.role(100, grants(), 0), f.role(secret, 0, 1)]),
    ])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  assert list.length(to(dispatches, one)) == 1
  assert list.length(to(dispatches, three)) == 1
  member_list.release(members)
  perms.release(handle)
}

pub fn member_change_does_not_double_sync_test() {
  let #(handle, members, lookup) = setup()
  let one = remote_ref(1, 6)
  let assert Ok(_) =
    member_list.subscribe(
      members,
      one,
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
  let changed = f.member(3, [secret])
  let assert Ok(effects) = perms.apply(handle, [event.member_upsert(changed)])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(
      members,
      [list_event.member_upsert(changed, list_event.Changed)],
      Some(perms.delta(effects)),
      lookup,
    )
  assert list.length(to(dispatches, one)) == 1
  member_list.release(members)
  perms.release(handle)
}

pub fn rebuild_then_permission_change_still_resyncs_test() {
  let #(handle, members, lookup) = setup()
  let one = remote_ref(1, 7)
  let assert Ok(_) =
    member_list.subscribe(
      members,
      one,
      f.channel_id(private),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(effects) =
    perms.apply(handle, [
      event.roles([f.role(100, grants(), 0), f.role(secret, 0, 1)]),
    ])
  let assert Ok(_) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  let #(fresh, _) =
    f.loaded(
      f.base([f.role(100, grants(), 0)], [f.text(private, [])])
      |> snapshot.add_member(f.member(1, [])),
    )
  let assert Ok(#(rebuilt, _, _)) =
    member_list.rebuild(
      members,
      fresh,
      ml_snapshot.new() |> ml_snapshot.add_member(f.member(1, [])),
      lookup,
    )
  let assert Ok(effects) =
    perms.apply(fresh, [event.roles([f.role(100, grants(), 0)])])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(rebuilt, [], Some(perms.delta(effects)), lookup)
  assert list.length(to(dispatches, one)) == 1
  member_list.release(rebuilt)
  perms.release(fresh)
  perms.release(handle)
}

pub fn rebound_session_gets_later_dispatches_test() {
  let #(handle, members, lookup) = setup()
  let old = remote_ref(1, 8)
  let assert Ok(_) =
    member_list.subscribe(
      members,
      old,
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
  let moved = SessionRef(..remote_ref(1, 9), id: old.id)
  member_list.rebind(members, moved)
  member_list.rebind(members, remote_ref(2, 10))
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(
      members,
      [list_event.member_remove(f.user_id(3))],
      None,
      lookup,
    )
  assert list.length(to(dispatches, moved)) == 1
  assert to(dispatches, old) == []
  member_list.release(members)
  perms.release(handle)
}

pub fn large_list_loads_pages_and_counts_test() {
  let users = int.range(12_000, 0, [], fn(acc, user) { [user, ..acc] })
  let source =
    list.fold(
      users,
      f.base([f.role(100, grants(), 0)], [f.text(public, [])]),
      fn(acc, user) { snapshot.add_member(acc, f.member(user, [])) },
    )
  let #(handle, _) = f.loaded(source)
  let #(members, counts) =
    member_list.new(f.guild(100), config.default(), process.self(), handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      list.fold(users, ml_snapshot.new(), fn(acc, user) {
        ml_snapshot.add_member(acc, f.member(user, []))
      }),
    )
  assert member_list.counts(counts) == #(12_000, 0)
  let assert Ok(page) = member_list.page_ids(members, 0, 12_000)
  assert packed_ids.to_list(page) == users
  let assert Ok(tail) = member_list.page_ids(members, 11_990, 100)
  assert packed_ids.to_list(tail) == list.drop(users, 11_990)
  let assert Ok(found) =
    member_list.channel_counts(members, [f.channel_id(public)])
  assert found == [#(f.channel_id(public), 12_000, 0)]
  member_list.release(members)
  perms.release(handle)
}
