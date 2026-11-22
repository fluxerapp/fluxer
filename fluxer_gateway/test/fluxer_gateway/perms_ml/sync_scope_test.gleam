// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
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
import fluxer_gateway/perms/snapshot.{type Snapshot}
import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import support/perms_ml_fixture as f

const public = 11

const private = 12

const secret = 900

const first_hidden = 5000

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

fn start(
  source: Snapshot,
  users: List(Int),
) -> #(perms.Perms, member_list.MemberList) {
  let #(handle, _) = f.loaded(source)
  let #(members, _) =
    member_list.new(f.guild(100), config.default(), process.self(), handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      list.fold(users, ml_snapshot.new(), fn(acc, user) {
        ml_snapshot.add_member(acc, f.member(user, []))
      }),
    )
  #(handle, members)
}

fn small() -> #(perms.Perms, member_list.MemberList) {
  let source =
    f.base([f.role(100, grants(), 0), f.role(secret, 0, 1)], [
      f.text(public, []),
      f.text(private, [f.deny_role(100, f.view), f.allow_role(secret, f.view)]),
    ])
    |> snapshot.add_member(f.member(1, [secret]))
    |> snapshot.add_member(f.member(2, [secret]))
    |> snapshot.add_member(f.member(3, []))
    |> snapshot.add_member(f.member(4, []))
  start(source, [1, 2, 3, 4])
}

fn subscribe(
  members: member_list.MemberList,
  subscriber: SessionRef,
  channel: Int,
  ranges: List(#(Int, Int)),
) -> String {
  let assert Ok([first]) =
    member_list.subscribe(
      members,
      subscriber,
      f.channel_id(channel),
      ranges,
      lookup,
    )
  text(first)
}

fn text(found: effects.Dispatch) -> String {
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(found.payload))
  text
}

fn to(dispatches: List(effects.Dispatch), target: SessionRef) -> List(String) {
  dispatches
  |> list.filter(fn(found) { list.contains(found.to, target) })
  |> list.map(text)
}

fn release(handle: perms.Perms, members: member_list.MemberList) -> Nil {
  member_list.release(members)
  perms.release(handle)
}

fn role_change(handle: perms.Perms, bits: Int) -> perms.Delta {
  let assert Ok(effects) =
    perms.apply(handle, [
      event.roles([f.role(100, bits, 0), f.role(secret, 0, 1)]),
    ])
  perms.delta(effects)
}

pub fn role_change_sends_one_sync_per_range_set_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let two = ref(2)
  let three = ref(3)
  let narrow = ref(4)
  let wide = subscribe(members, one, public, [#(0, 99)])
  let _ = subscribe(members, two, public, [#(0, 99)])
  let _ = subscribe(members, three, public, [#(0, 99)])
  let _ = subscribe(members, narrow, public, [#(0, 1)])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(
      members,
      [],
      Some(role_change(handle, grants() - 2048)),
      lookup,
    )
  assert list.length(dispatches) == 2
  let assert [grouped] =
    list.filter(dispatches, fn(found) { list.length(found.to) == 3 })
  assert set.from_list(list.map(grouped.to, fn(found) { found.id }))
    == set.from_list([one.id, two.id, three.id])
  assert text(grouped) == wide
  assert list.length(to(dispatches, narrow)) == 1
  release(handle, members)
}

pub fn view_gained_inside_the_resync_scope_is_not_sent_twice_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let two = ref(2)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let _ = subscribe(members, two, private, [#(0, 99)])
  let assert Ok(effects) =
    perms.apply(handle, [
      event.channel_upsert(
        f.text(private, [
          f.deny_role(100, f.view),
          f.allow_role(secret, f.view),
          f.overwrite_member_deny(4, f.view),
        ]),
        event.Updated,
      ),
    ])
  let delta = perms.delta(effects)
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(
      members,
      [list_event.view_gained(one.id, f.channel_id(private))],
      Some(delta),
      lookup,
    )
  assert list.length(to(dispatches, one)) == 1
  assert list.length(to(dispatches, two)) == 1
  let assert Ok(#(again, _)) =
    member_list.apply_timed(
      members,
      [list_event.view_gained(two.id, f.channel_id(private))],
      Some(delta),
      lookup,
    )
  assert again == []
  release(handle, members)
}

pub fn view_gained_with_a_superseded_delta_still_syncs_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let older = role_change(handle, grants() - 2048)
  let newer = role_change(handle, grants())
  let assert Ok(#(first, _)) =
    member_list.apply_timed(members, [], Some(newer), lookup)
  assert list.length(to(first, one)) == 1
  let assert Ok(#(late, _)) =
    member_list.apply_timed(
      members,
      [list_event.view_gained(one.id, f.channel_id(private))],
      Some(older),
      lookup,
    )
  assert list.length(to(late, one)) == 1
  release(handle, members)
}

pub fn channel_scope_resync_spans_more_than_one_event_chunk_test() {
  let hidden =
    int.range(first_hidden, first_hidden + 70, [], fn(acc, channel) {
      [channel, ..acc]
    })
    |> list.reverse
  let watchers = list.map(hidden, fn(channel) { channel - first_hidden + 10 })
  let source =
    list.fold(
      hidden,
      f.base([f.role(100, grants(), 0), f.role(secret, 0, 1)], [
        f.text(public, []),
      ]),
      fn(acc, channel) {
        snapshot.add_channel(
          acc,
          f.text(channel, [
            f.deny_role(100, f.view),
            f.allow_role(secret, f.view),
          ]),
        )
      },
    )
    |> snapshot.add_member(f.member(1, []))
    |> fn(acc) {
      list.fold(watchers, acc, fn(acc, user) {
        snapshot.add_member(acc, f.member(user, [secret]))
      })
    }
  let #(handle, members) = start(source, [1, ..watchers])
  let refs =
    list.map2(hidden, watchers, fn(channel, user) {
      let subscriber = ref(user)
      let _ = subscribe(members, subscriber, channel, [#(0, 99)])
      subscriber
    })
  let bystander = ref(1)
  let _ = subscribe(members, bystander, public, [#(0, 99)])
  let assert Ok(effects) =
    perms.apply(handle, [event.owner(Some(f.user_id(1)))])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(members, [], Some(perms.delta(effects)), lookup)
  list.each(refs, fn(subscriber) {
    let assert [synced] = to(dispatches, subscriber)
    assert string.contains(synced, "\"id\":\"1\"")
  })
  assert to(dispatches, bystander) == []
  release(handle, members)
}

pub fn view_gained_after_a_resubscribe_is_still_sent_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let delta = role_change(handle, grants() - 2048)
  let assert Ok(#(first, _)) =
    member_list.apply_timed(members, [], Some(delta), lookup)
  assert list.length(to(first, one)) == 1
  let assert Ok(_) =
    member_list.subscribe(
      members,
      one,
      f.channel_id(private),
      [#(0, 99), #(100, 199)],
      lookup,
    )
  let assert Ok(#(again, _)) =
    member_list.apply_timed(
      members,
      [list_event.view_gained(one.id, f.channel_id(private))],
      Some(delta),
      lookup,
    )
  assert list.length(to(again, one)) == 1
  release(handle, members)
}

pub fn list_update_between_resync_and_view_gained_keeps_the_sync_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let delta = role_change(handle, grants() - 2048)
  let assert Ok(_) = member_list.apply_timed(members, [], Some(delta), lookup)
  let assert Ok(_) =
    member_list.apply_timed(
      members,
      [list_event.member_remove(f.user_id(2))],
      None,
      lookup,
    )
  let assert Ok(#(again, _)) =
    member_list.apply_timed(
      members,
      [list_event.view_gained(one.id, f.channel_id(private))],
      Some(delta),
      lookup,
    )
  let assert [synced] = to(again, one)
  assert !string.contains(synced, "\"id\":\"2\"")
  release(handle, members)
}

pub fn released_permissions_break_the_list_without_crashing_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, public, [#(0, 99)])
  let delta = role_change(handle, grants() - 2048)
  perms.release(handle)
  assert member_list.apply_timed(members, [], Some(delta), lookup)
    == Error(member_list.ListBroken)
  assert member_list.subscribe(
      members,
      ref(2),
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
    == Error(member_list.ListBroken)
  member_list.release(members)
}

pub fn dead_owner_shuts_the_list_test() {
  let source =
    f.base([f.role(100, grants(), 0)], [f.text(public, [])])
    |> snapshot.add_member(f.member(1, []))
  let #(handle, _) = f.loaded(source)
  let owner = process.spawn_unlinked(fn() { process.sleep(5000) })
  let #(members, _) =
    member_list.new(f.guild(100), config.default(), owner, handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      ml_snapshot.add_member(ml_snapshot.new(), f.member(1, [])),
    )
  process.kill(owner)
  process.sleep(20)
  assert member_list.subscribe(
      members,
      ref(1),
      f.channel_id(public),
      [#(0, 99)],
      lookup,
    )
    == Error(member_list.ListBroken)
  member_list.release(members)
  perms.release(handle)
}

fn hidden_rebuild(
  members: member_list.MemberList,
) -> #(perms.Perms, member_list.MemberList) {
  let #(fresh, _) =
    f.loaded(
      f.base([f.role(100, grants(), 0), f.role(secret, 0, 1)], [
        f.text(public, []),
        f.text(private, [f.deny_role(100, f.view)]),
      ])
      |> snapshot.add_member(f.member(1, [secret]))
      |> snapshot.add_member(f.member(2, [])),
    )
  let assert Ok(#(rebuilt, _, dispatches)) =
    member_list.rebuild(
      members,
      fresh,
      ml_snapshot.new()
        |> ml_snapshot.add_member(f.member(1, []))
        |> ml_snapshot.add_member(f.member(2, [])),
      lookup,
    )
  assert dispatches == []
  #(fresh, rebuilt)
}

fn reveal(handle: perms.Perms) -> perms.Delta {
  let assert Ok(effects) =
    perms.apply(handle, [
      event.channel_upsert(
        f.text(private, [
          f.deny_role(100, f.view),
          f.allow_role(secret, f.view),
        ]),
        event.Updated,
      ),
    ])
  perms.delta(effects)
}

pub fn subscription_hidden_by_a_rebuild_comes_back_with_view_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let #(fresh, rebuilt) = hidden_rebuild(members)
  assert member_list.subscribed_channel(rebuilt, one.id) == None
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(rebuilt, [], Some(reveal(fresh)), lookup)
  let assert [synced] = to(dispatches, one)
  assert string.contains(synced, "\"channel_id\":\"12\"")
  assert member_list.subscribed_channel(rebuilt, one.id)
    == Some(f.channel_id(private))
  member_list.release(rebuilt)
  perms.release(fresh)
  perms.release(handle)
}

pub fn resubscribing_drops_a_subscription_hidden_by_a_rebuild_test() {
  let #(handle, members) = small()
  let one = ref(1)
  let _ = subscribe(members, one, private, [#(0, 99)])
  let #(fresh, rebuilt) = hidden_rebuild(members)
  let _ = subscribe(rebuilt, one, public, [#(0, 99)])
  let assert Ok(#(dispatches, _)) =
    member_list.apply_timed(rebuilt, [], Some(reveal(fresh)), lookup)
  assert to(dispatches, one) == []
  assert member_list.subscribed_channel(rebuilt, one.id)
    == Some(f.channel_id(public))
  member_list.release(rebuilt)
  perms.release(fresh)
  perms.release(handle)
}
