// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/member_list
import fluxer_gateway/member_list/config
import fluxer_gateway/member_list/effects
import fluxer_gateway/member_list/item
import fluxer_gateway/member_list/snapshot as ml_snapshot
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/snapshot
import gleam/bit_array
import gleam/dynamic
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/string

const guild = 100

const text = 11

const viewer = 9

const user_json = "{\"id\":\"9\",\"username\":\"nine\"}"

@external(erlang, "erlang", "binary_to_term")
fn binary_to_pid(bytes: BitArray) -> Pid

@external(erlang, "erlang", "node")
fn node_of(pid: Pid) -> Atom

@external(erlang, "erlang", "node")
fn this_node() -> Atom

fn remote_pid() -> Pid {
  binary_to_pid(<<
    131, 88, 119, 10, "other@host":utf8, 1:size(32), 0:size(32), 1:size(32),
  >>)
}

fn remote_ref() -> SessionRef {
  SessionRef(
    id: session.new_session_id(),
    user: fixture.user_id(viewer),
    inbox: process.unsafely_create_subject(
      remote_pid(),
      dynamic.string("member_list"),
    ),
  )
}

fn setup() -> #(perms.Perms, member_list.MemberList, member_list.Lookup) {
  let assert Ok(Nil) = nif.load()
  let guild_id = fixture.guild(guild)
  let viewer_member = fixture.make_member(viewer, None, user_json)
  let bits = int.bitwise_shift_left(1, 54) + 1024
  let source =
    snapshot.new(guild_id)
    |> snapshot.set_roles([fixture.make_role(guild, bits)])
    |> snapshot.add_channel(fixture.make_channel(text, channel.Text, None, []))
    |> snapshot.add_member(viewer_member)
  let #(handle, _) = perms.new(guild_id, process.self())
  let assert Ok(_) = perms.load(handle, source)
  let #(members, _) =
    member_list.new(guild_id, config.default(), process.self(), handle)
  let assert Ok(Nil) =
    member_list.load(
      members,
      ml_snapshot.new() |> ml_snapshot.add_member(viewer_member),
    )
  let lookup = fn(users) {
    list.map(users, fn(user) {
      let #(list_bytes, page_bytes) =
        item.encode(fixture.make_member(viewer, None, user_json), None, False)
      #(user, list_bytes, page_bytes)
    })
  }
  #(handle, members, lookup)
}

fn is_sync_to(found: effects.Dispatch, ref: SessionRef) -> Bool {
  let payload =
    bit_array.to_string(raw_json.to_bits(found.payload))
    |> option.from_result
    |> option.unwrap("")
  found.to == [ref]
  && found.event == effects.event_name
  && string.contains(payload, "\"op\":\"SYNC\"")
}

pub fn remote_subscriber_receives_sync_test() {
  let #(handle, members, lookup) = setup()
  let ref = remote_ref()
  assert node_of(remote_pid()) != this_node()
  let assert Ok(dispatches) =
    member_list.subscribe(
      members,
      ref,
      fixture.channel_id(text),
      [#(0, 99)],
      lookup,
    )
  assert list.any(dispatches, is_sync_to(_, ref))
  member_list.release(members)
  perms.release(handle)
}

pub fn rebuild_keeps_remote_subscriber_test() {
  let #(handle, members, lookup) = setup()
  let ref = remote_ref()
  let assert Ok(_) =
    member_list.subscribe(
      members,
      ref,
      fixture.channel_id(text),
      [#(0, 99)],
      lookup,
    )
  let viewer_member = fixture.make_member(viewer, None, user_json)
  let assert Ok(#(rebuilt, _, dispatches)) =
    member_list.rebuild(
      members,
      handle,
      ml_snapshot.new() |> ml_snapshot.add_member(viewer_member),
      lookup,
    )
  assert list.any(dispatches, is_sync_to(_, ref))
  member_list.release(rebuilt)
  perms.release(handle)
}

pub fn unsubscribe_stops_dispatches_to_remote_subscriber_test() {
  let #(handle, members, lookup) = setup()
  let ref = remote_ref()
  let assert Ok(_) =
    member_list.subscribe(
      members,
      ref,
      fixture.channel_id(text),
      [#(0, 99)],
      lookup,
    )
  let assert Ok(_) = member_list.unsubscribe(members, ref.id)
  assert member_list.subscribed_channel(members, ref.id) == None
  member_list.release(members)
  perms.release(handle)
}
