// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/perms
import fluxer_gateway/perms/audience
import fluxer_gateway/perms/event
import fluxer_gateway/perms/native
import fluxer_gateway/perms/snapshot
import gleam/int
import gleam/list
import gleam/option.{Some}
import support/perms_ml_fixture as f

const everyone = 100

const public = 11

const private = 12

const secret_role = 900

fn grants() -> Int {
  f.view + 2048 + f.view_members
}

fn roles() {
  [
    f.role(everyone, grants(), 0),
    f.role(secret_role, 0, 1),
    ..int.range(0, 13, [], fn(acc, i) { [f.role(200 + i, 0, 2 + i), ..acc] })
  ]
}

fn channels() {
  [
    f.text(public, []),
    f.text(private, [
      f.deny_role(everyone, f.view),
      f.allow_role(secret_role, f.view),
    ]),
  ]
}

fn ids(from: Int, to: Int) -> List(Int) {
  int.range(from, to, [], fn(acc, i) { [i, ..acc] }) |> list.reverse
}

pub fn large_snapshot_loads_and_answers_batched_queries_test() {
  let members = ids(1000, 31_000)
  let source =
    list.fold(members, f.base(roles(), channels()), fn(acc, user) {
      let extra = case user % 3 == 0 {
        True -> [secret_role]
        False -> []
      }
      snapshot.add_member(acc, f.member(user, extra))
    })
  let #(handle, reader) = f.loaded(source)
  let assert Ok(viewers) = perms.viewers(reader, f.channel_id(private))
  let expected = list.filter(members, fn(user) { user % 3 == 0 })
  assert packed_ids.to_list(viewers) == expected
  let probe = ids(990, 6000)
  let assert Ok(filtered) =
    perms.filter_viewers(
      reader,
      f.channel_id(private),
      packed_ids.from_list(probe),
    )
  assert packed_ids.to_list(filtered)
    == list.filter(probe, fn(user) { user >= 1000 && user % 3 == 0 })
  let assert Ok(shared) =
    perms.shares_view_many(
      reader,
      f.user_id(1002),
      packed_ids.from_list(ids(1000, 10_000)),
    )
  assert packed_ids.to_list(shared)
    == list.filter(ids(1000, 10_000), fn(user) { user != 1002 })
  let assert Ok(lonely) =
    perms.shares_view_many(
      reader,
      f.user_id(1001),
      packed_ids.from_list(ids(1000, 10_000)),
    )
  assert packed_ids.to_list(lonely)
    == list.filter(ids(1000, 10_000), fn(user) { user != 1001 })
  perms.release(handle)
}

pub fn more_than_one_chunk_of_events_tracks_every_user_test() {
  let members = ids(1, 301)
  let source =
    list.fold(members, f.base(roles(), channels()), fn(acc, user) {
      snapshot.add_member(acc, f.member(user, []))
    })
  let #(handle, reader) = f.loaded(source)
  let promoted = ids(1, 201)
  let events =
    list.map(promoted, fn(user) {
      event.member_upsert(f.member(user, [secret_role]))
    })
  let assert Ok(effects) = perms.apply(handle, events)
  list.each([1, 64, 65, 128, 129, 200], fn(user) {
    let assert Ok(group) = perms.group_of(reader, f.user_id(user))
    assert event.for_user(effects, f.user_id(user), group)
      == event.UserChange(lost: [], gained: [f.channel_id(private)])
  })
  let assert Ok(group) = perms.group_of(reader, f.user_id(250))
  assert event.for_user(effects, f.user_id(250), group)
    == event.UserChange(lost: [], gained: [])
  let assert Ok(viewers) = perms.viewers(reader, f.channel_id(private))
  assert packed_ids.to_list(viewers) == promoted
  perms.release(handle)
}

fn role_set(value: Int) -> audience.Group {
  audience.group_from_native(native.RoleSetGroup(value))
}

fn churn(user: Int, from: Int, to: Int) -> List(event.Event) {
  ids(from, to)
  |> list.map(fn(mask) {
    event.member_upsert(f.member(user, f.subset_roles(mask, 200, 13)))
  })
}

pub fn compaction_keeps_groups_and_visibility_changes_consistent_test() {
  let source =
    f.base(roles(), channels())
    |> snapshot.add_member(f.member(1, [secret_role]))
    |> snapshot.add_member(f.member(2, []))
    |> snapshot.add_member(f.member(3, [201, 205]))
  let #(handle, reader) = f.loaded(source)
  let assert Ok(_) = perms.apply(handle, churn(4, 1, 4000))
  let assert Ok(Some(churned)) = perms.group_of(reader, f.user_id(4))
  assert churned == role_set(4000)
  let revision = perms.revision(reader)
  let events =
    list.append(churn(4, 4000, 4300), [
      event.channel_upsert(
        f.text(private, [f.deny_role(everyone, f.view)]),
        event.Updated,
      ),
      event.member_upsert(f.member(2, [secret_role])),
    ])
  let assert Ok(effects) = perms.apply(handle, events)
  assert perms.revision(reader) > revision
  let assert Ok(Some(compacted)) = perms.group_of(reader, f.user_id(4))
  assert compacted == role_set(3)
  let assert Ok(after) = perms.group_of(reader, f.user_id(1))
  assert event.for_user(effects, f.user_id(1), after)
    == event.UserChange(lost: [f.channel_id(private)], gained: [])
  let assert Ok(two) = perms.group_of(reader, f.user_id(2))
  assert event.for_user(effects, f.user_id(2), two)
    == event.UserChange(lost: [], gained: [])
  let assert Ok(three) = perms.group_of(reader, f.user_id(3))
  assert event.for_user(effects, f.user_id(3), three)
    == event.UserChange(lost: [], gained: [])
  let assert Ok(found) =
    perms.audience(reader, f.channel_id(public), audience.View)
  let assert Some(one) = after
  assert audience.contains(found, one)
  let assert Ok(hidden) =
    perms.audience(reader, f.channel_id(private), audience.View)
  assert !audience.contains(hidden, one)
  assert perms.can_view(reader, f.user_id(3), f.channel_id(public)) == Ok(True)
  assert perms.can_view(reader, f.user_id(1), f.channel_id(private))
    == Ok(False)
  perms.release(handle)
}

pub fn malformed_member_sees_nothing_until_a_valid_upsert_test() {
  let source =
    f.base(roles(), channels())
    |> snapshot.add_member(f.member(1, []))
    |> snapshot.add_malformed_member(f.user_id(2))
  let #(handle, reader) = f.loaded(source)
  assert perms.can_view(reader, f.user_id(2), f.channel_id(public)) == Ok(False)
  let assert Ok(effects) =
    perms.apply(handle, [event.member_upsert(f.member(2, []))])
  let assert Ok(group) = perms.group_of(reader, f.user_id(2))
  assert event.for_user(effects, f.user_id(2), group)
    == event.UserChange(lost: [], gained: [f.channel_id(public)])
  assert perms.can_view(reader, f.user_id(2), f.channel_id(public)) == Ok(True)
  perms.release(handle)
}

pub fn second_load_on_a_live_handle_is_refused_and_keeps_the_model_test() {
  let source =
    f.base(roles(), channels())
    |> snapshot.add_member(f.member(1, [secret_role]))
    |> snapshot.add_member(f.member(2, [secret_role]))
  let #(handle, reader) = f.loaded(source)
  let before = perms.epoch(reader)
  let assert Error(perms.BadInput(_)) =
    perms.load(
      handle,
      f.base(roles(), channels())
        |> snapshot.add_member(f.member(3, [secret_role])),
    )
  assert perms.epoch(reader) == before
  let assert Ok(viewers) = perms.viewers(reader, f.channel_id(private))
  assert packed_ids.to_list(viewers) == [1, 2]
  perms.release(handle)
}

pub fn released_handle_reports_gone_test() {
  let #(handle, reader) =
    f.loaded(
      f.base(roles(), channels()) |> snapshot.add_member(f.member(1, [])),
    )
  perms.release(handle)
  assert perms.can_view(reader, f.user_id(1), f.channel_id(public))
    == Error(perms.Gone)
  assert perms.apply(handle, [event.member_upsert(f.member(1, []))])
    == Error(perms.Gone)
}
