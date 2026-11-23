// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/presence
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
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import support/perms_ml_fixture as f
import support/perms_ml_harness as h

const public = 11

const secret = 900

const total = 50_000

const online = 3000

fn grants() -> Int {
  f.view + 2048 + f.view_members
}

fn ref(user: Int) -> SessionRef {
  SessionRef(
    id: session.new_session_id(),
    user: f.user_id(user),
    inbox: process.new_subject(),
  )
}

fn lookup(users: List(id.UserId)) -> List(#(id.UserId, BitArray, BitArray)) {
  list.map(users, fn(user) {
    let #(list_bytes, page_bytes) =
      item.encode(f.member(id.user_id_to_int(user), []), None, False)
    #(user, list_bytes, page_bytes)
  })
}

fn roles_of(user: Int) -> List(Int) {
  case user % 10 {
    0 -> [secret]
    _ -> []
  }
}

fn users() -> List(Int) {
  int.range(total, 0, [], fn(acc, user) { [user, ..acc] })
}

fn start() -> #(perms.Perms, member_list.MemberList) {
  let source =
    list.fold(
      users(),
      f.base([f.role(100, grants(), 0), f.role(secret, 0, 1)], [
        f.text(public, []),
      ]),
      fn(acc, user) { snapshot.add_member(acc, f.member(user, roles_of(user))) },
    )
  let #(handle, _) = f.loaded(source)
  let #(members, _) =
    member_list.new(f.guild(100), config.default(), process.self(), handle)
  let loaded =
    list.fold(users(), ml_snapshot.new(), fn(acc, user) {
      let acc = ml_snapshot.add_member(acc, f.member(user, roles_of(user)))
      case user <= online {
        True ->
          ml_snapshot.add_presence(acc, h.status(user, presence.Online))
          |> ml_snapshot.add_connected(f.user_id(user))
        False -> acc
      }
    })
  let assert Ok(Nil) = member_list.load(members, loaded)
  #(handle, members)
}

fn text(found: effects.Dispatch) -> String {
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(found.payload))
  text
}

fn subscribe(
  members: member_list.MemberList,
  subscriber: SessionRef,
  ranges: List(#(Int, Int)),
) -> String {
  let assert Ok([first]) =
    member_list.subscribe(
      members,
      subscriber,
      f.channel_id(public),
      ranges,
      lookup,
    )
  text(first)
}

fn range_set(index: Int) -> List(#(Int, Int)) {
  case index % 3 {
    0 -> [#(0, 99)]
    1 -> [#(0, 99), #(100, 199)]
    _ -> [#(0, 99), #(2900, 2999)]
  }
}

pub fn large_list_overwrite_change_sends_one_sync_per_range_set_test() {
  let #(handle, members) = start()
  let viewers =
    int.range(1, 301, [], fn(acc, user) {
      case user % 10 {
        0 -> acc
        _ -> [#(ref(user), range_set(user)), ..acc]
      }
    })
  list.each(viewers, fn(viewer) {
    let _ = subscribe(members, viewer.0, viewer.1)
    Nil
  })
  let assert Ok(effects) =
    perms.apply(handle, [
      event.channel_upsert(
        f.text(public, [f.deny_role(secret, f.view)]),
        event.Updated,
      ),
    ])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  assert list.length(dispatches) == 3
  let reached =
    list.flat_map(dispatches, fn(found) { found.to })
    |> list.map(fn(found) { found.id })
  assert list.length(reached) == list.length(viewers)
  assert set.from_list(reached)
    == set.from_list(list.map(viewers, fn(viewer) { { viewer.0 }.id }))
  list.each([0, 1, 2], fn(index) {
    let ranges = range_set(index)
    let fresh = subscribe(members, ref(301 + index), ranges)
    let assert [sent] =
      list.filter(dispatches, fn(found) {
        list.any(viewers, fn(viewer) {
          viewer.1 == ranges && list.contains(found.to, viewer.0)
        })
      })
    assert text(sent) == fresh
    assert !string.contains(fresh, "\"username\":\"u10\"")
    assert string.contains(fresh, "\"username\":\"u1001\"")
  })
  member_list.release(members)
  perms.release(handle)
}

pub fn large_list_presence_burst_then_role_resync_matches_fresh_view_test() {
  let #(handle, members) = start()
  let watcher = ref(1)
  let _ = subscribe(members, watcher, [#(0, 99), #(100, 199)])
  let burst =
    int.range(online + 1, online + 201, [], fn(acc, user) {
      [
        list_event.connected(f.user_id(user), list_event.Changed),
        list_event.presence(
          f.user_id(user),
          presence.Online,
          list_event.Changed,
          list_event.Immediate,
        ),
        ..acc
      ]
    })
  let assert Ok(_) = member_list.apply_timed(members, burst, None, lookup)
  let assert Ok(effects) =
    perms.apply(handle, [
      event.roles([f.role(100, grants(), 0), f.role(secret, 0, 2)]),
    ])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  let assert [sent] =
    list.filter(dispatches, fn(found) { list.contains(found.to, watcher) })
  let fresh = subscribe(members, ref(2), [#(0, 99), #(100, 199)])
  assert text(sent) == fresh
  assert string.contains(fresh, "\"online_count\":3200")
  member_list.release(members)
  perms.release(handle)
}
