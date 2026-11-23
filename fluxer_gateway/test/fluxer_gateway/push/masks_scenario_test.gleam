// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/packed_ids
import fluxer_gateway/guild/push/masks
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config
import fluxer_gateway/member_list/event
import fluxer_gateway/member_list/snapshot as ml_snapshot
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/event as perm_event
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/snapshot
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}

const guild = 300

const text = 31

fn member(user: Int) {
  fixture.make_member(
    user,
    None,
    "{\"id\":\"" <> int.to_string(user) <> "\",\"username\":\"u\"}",
  )
}

fn setup(
  users: List(Int),
) -> #(perms.Perms, perms.Reader, member_list.MemberList) {
  let assert Ok(Nil) = nif.load()
  let guild_id = fixture.guild(guild)
  let bits = int.bitwise_shift_left(1, 54) + 1024
  let source =
    list.fold(
      users,
      snapshot.new(guild_id)
        |> snapshot.set_roles([fixture.make_role(guild, bits)])
        |> snapshot.add_channel(
          fixture.make_channel(text, channel.Text, None, []),
        ),
      fn(acc, user) { snapshot.add_member(acc, member(user)) },
    )
  let #(handle, reader) = perms.new(guild_id, process.self())
  let assert Ok(_) = perms.load(handle, source)
  let #(members, _) =
    member_list.new(guild_id, config.default(), process.self(), handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      list.fold(users, ml_snapshot.new(), fn(acc, user) {
        ml_snapshot.add_member(acc, member(user))
      }),
    )
  #(handle, reader, members)
}

fn lookup(users) {
  list.map(users, fn(user) { #(user, <<>>, <<>>) })
}

fn query(
  members: member_list.MemberList,
  reader: perms.Reader,
) -> Result(#(List(Int), List(Int)), masks.Fallback) {
  case masks.supersets(members, perms.epoch(reader)) {
    Ok(#(immediate, held)) ->
      Ok(#(
        list.sort(packed_ids.to_list(immediate), int.compare),
        list.sort(packed_ids.to_list(held), int.compare),
      ))
    Error(why) -> Error(why)
  }
}

fn classes() {
  [
    masks.class_update(1, masks.ClassAllMessages),
    masks.class_update(2, masks.ClassUncertain),
    masks.class_update(3, masks.ClassMuted),
    masks.class_update(4, masks.ClassMentionsOnly),
  ]
}

pub fn masks_fall_back_until_the_first_update_test() {
  let #(handle, reader, members) = setup([1, 2, 3, 4])
  assert query(members, reader) == Error(masks.FallbackStale)
  assert masks.update(members, classes())
  assert query(members, reader) == Ok(#([1, 2], []))
  member_list.release(members)
  perms.release(handle)
}

pub fn a_query_for_another_epoch_falls_back_test() {
  let #(handle, reader, members) = setup([1, 2, 3, 4])
  assert masks.update(members, classes())
  let assert Error(masks.FallbackStale) =
    masks.supersets(members, perms.epoch(reader) + 1)
  member_list.release(members)
  perms.release(handle)
}

fn change(
  handle: perms.Perms,
  members: member_list.MemberList,
  perm_events: List(perm_event.Event),
  list_events: List(event.Event),
) -> Nil {
  let assert Ok(effects) = perms.apply(handle, perm_events)
  let assert Ok(_) =
    member_list.apply_timed(
      members,
      list_events,
      Some(perms.delta(effects)),
      lookup,
    )
  Nil
}

pub fn leavers_drop_out_and_joiners_start_uncertain_test() {
  let #(handle, reader, members) = setup([1, 2, 3, 4])
  assert masks.update(members, classes())
  change(
    handle,
    members,
    [
      perm_event.member_remove(fixture.user_id(1)),
      perm_event.member_upsert(member(5)),
    ],
    [
      event.member_remove(fixture.user_id(1)),
      event.member_upsert(member(5), event.Changed),
    ],
  )
  assert query(members, reader) == Ok(#([2, 5], []))
  change(handle, members, [perm_event.member_upsert(member(1))], [
    event.member_upsert(member(1), event.Changed),
  ])
  assert query(members, reader) == Ok(#([1, 2, 5], []))
  member_list.release(members)
  perms.release(handle)
}

pub fn a_muted_member_stays_out_after_a_reclass_test() {
  let #(handle, reader, members) = setup([1, 2, 3, 4])
  assert masks.update(members, classes())
  assert masks.update(members, [
    masks.class_update(1, masks.ClassMuted),
    masks.class_update(3, masks.ClassAllMessages),
    masks.class_update(77, masks.ClassAllMessages),
  ])
  assert query(members, reader) == Ok(#([2, 3], []))
  member_list.release(members)
  perms.release(handle)
}

pub fn more_than_one_slice_of_updates_applies_test() {
  let users = int.range(1, 4200, [], fn(acc, user) { [user, ..acc] })
  let #(handle, reader, members) = setup(users)
  let updates =
    list.map(users, fn(user) {
      case user % 2 {
        0 -> masks.class_update(user, masks.ClassMuted)
        _ -> masks.class_update(user, masks.ClassAllMessages)
      }
    })
  assert masks.update(members, updates)
  let assert Ok(#(immediate, [])) = query(members, reader)
  assert list.length(immediate) == 2100
  assert list.all(immediate, fn(user) { user % 2 == 1 })
  member_list.release(members)
  perms.release(handle)
}

pub fn a_rebuilt_member_list_gets_its_masks_back_on_refresh_test() {
  let #(handle, reader, members) = setup([1, 2, 3, 4])
  let before = masks.key(members, default_level: 0, large: False)
  let assert Some(_) = masks.refresh(members, None, before, classes)
  assert query(members, reader) == Ok(#([1, 2], []))
  let asked = process.new_subject()
  let assert Some(same) =
    masks.refresh(members, Some(before), before, fn() {
      process.send(asked, Nil)
      classes()
    })
  assert process.receive(asked, 0) == Error(Nil)
  assert same == before
  let assert Ok(#(rebuilt, _, _)) =
    member_list.rebuild(
      members,
      handle,
      list.fold([1, 2, 3, 4], ml_snapshot.new(), fn(acc, user) {
        ml_snapshot.add_member(acc, member(user))
      }),
      lookup,
    )
  assert query(rebuilt, reader) == Error(masks.FallbackStale)
  let after = masks.key(rebuilt, default_level: 0, large: False)
  assert after != before
  let assert Some(current) =
    masks.refresh(rebuilt, Some(before), after, classes)
  assert current == after
  assert query(rebuilt, reader) == Ok(#([1, 2], []))
  member_list.release(rebuilt)
  perms.release(handle)
}
