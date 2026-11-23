// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/voice/call/state
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import support/voice_ids

@external(erlang, "erlang", "tuple_size")
fn size(member: state.Member) -> Int

@external(erlang, "erlang", "element")
fn disconnect_of(
  index: Int,
  member: state.Member,
) -> fn(ChannelId, Option(ConnectionId)) -> Dynamic

@external(erlang, "erlang", "element")
fn converted_of(index: Int, member: state.Member) -> fn(ConnectionId) -> Dynamic

@external(erlang, "erlang", "append_element")
fn append(member: state.Member, value: a) -> state.Member

fn member() -> state.Member {
  let assert Ok(user) = id.user_id(7)
  state.Member(
    user:,
    inbox: process.new_subject(),
    monitor: None,
    owner: Some(process.self()),
    owner_monitor: None,
  )
}

pub fn an_epoch_four_peer_gets_the_epoch_four_member_shape_test() {
  let current = member()
  assert size(current) == 6
  assert state.for_peer(current, True) == current
  let old = state.for_peer(current, False)
  assert size(old) == 8
  assert old.user == current.user
  assert old.inbox == current.inbox
  assert old.owner == current.owner
  let assert Ok(channel) = id.channel_id(40)
  let _ = disconnect_of(7, old)(channel, Some(voice_ids.conn("c")))
  let _ = converted_of(8, old)(voice_ids.conn("c"))
  assert state.from_wire(old) == current
}

pub fn a_member_from_an_epoch_four_session_is_read_as_plain_data_test() {
  let current = member()
  let old =
    current
    |> append(fn(_channel: ChannelId, _connection: Option(ConnectionId)) { Nil })
    |> append(fn(_connection: ConnectionId) { Nil })
  assert size(old) == 8
  let read = state.from_wire(old)
  assert size(read) == 6
  assert read == current
  assert state.from_wire(current) == current
}
