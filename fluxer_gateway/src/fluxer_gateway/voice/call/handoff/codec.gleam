// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/key
import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/voice_key.{type ConnectionId}
import fluxer_gateway/handoff/codec.{type Codec, type CodecError, Codec}
import fluxer_gateway/handoff/nodes
import fluxer_gateway/presence/handoff/terms
import fluxer_gateway/voice/call/handoff/wire_v1.{
  type CallTailV1, type CallerV1, type InboxV1, type LocatedV1, type MemberV1,
  type PendingJoinV1, type TargetV1, type VoiceStateV1,
}
import fluxer_gateway/voice/call/state.{
  type Call, type Caller, type Member, type PendingJoin, type Slot, Call, Caller,
  PendingJoin, Slot,
}
import fluxer_gateway/voice/geo.{type Geo}
import fluxer_gateway/voice/store
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

pub type Tail {
  Tail(call: Call, located: Dict(ConnectionId, Geo))
}

pub fn range() -> CodecRange {
  CodecRange(
    ns: key.call,
    oldest_read: wire_v1.version,
    max_read: wire_v1.version,
    max_write: wire_v1.version,
  )
}

pub fn tail() -> Codec(Tail) {
  Codec(
    ns: key.call,
    part: blob.Tail,
    range: range(),
    encode_body: encode_tail,
    decode_body: decode_tail,
  )
}

pub fn inbox() -> Codec(message) {
  Codec(
    ns: key.call,
    part: blob.Inbox,
    range: range(),
    encode_body: encode_inbox,
    decode_body: decode_inbox,
  )
}

pub fn encode_target(target: Subject(message)) -> BitArray {
  terms.encode(wire_v1.TargetV1(
    nodes: nodes.known(),
    subject: terms.encode(target),
  ))
}

pub fn decode_target(bytes: BitArray) -> Result(Subject(message), Nil) {
  use wire <- result.try(
    terms.decode(bytes, target_decoder()) |> result.replace_error(Nil),
  )
  nodes.ensure(wire.nodes)
  codec.term_of(wire.subject) |> result.replace_error(Nil)
}

fn writable(version: Int) -> Result(Nil, CodecError) {
  case version == wire_v1.version {
    True -> Ok(Nil)
    False -> Error(codec.UnwritableVersion(version))
  }
}

fn readable(version: Int) -> Result(Nil, CodecError) {
  case version == wire_v1.version {
    True -> Ok(Nil)
    False -> Error(codec.UnreadableVersion(version))
  }
}

fn encode_tail(value: Tail, version: Int) -> Result(BitArray, CodecError) {
  use Nil <- result.map(writable(version))
  terms.encode(tail_v1(value))
}

fn decode_tail(version: Int, body: BitArray) -> Result(Tail, CodecError) {
  use Nil <- result.try(readable(version))
  use wire <- result.try(
    terms.decode(body, tail_decoder()) |> result.map_error(codec.Malformed),
  )
  nodes.ensure(wire.nodes)
  tail_of(wire)
}

fn encode_inbox(value: message, version: Int) -> Result(BitArray, CodecError) {
  use Nil <- result.map(writable(version))
  terms.encode(wire_v1.InboxV1(
    nodes: nodes.known(),
    message: terms.encode(value),
  ))
}

fn decode_inbox(version: Int, body: BitArray) -> Result(message, CodecError) {
  use Nil <- result.try(readable(version))
  use wire <- result.try(
    terms.decode(body, inbox_decoder()) |> result.map_error(codec.Malformed),
  )
  nodes.ensure(wire.nodes)
  codec.term_of(wire.message)
}

fn tail_v1(tail: Tail) -> CallTailV1 {
  let call = tail.call
  wire_v1.CallTailV1(
    channel_id: id.channel_id_to_int(call.channel),
    message_id: id.message_id_to_int(call.message),
    region: call.region,
    ringing: users_v1(call.ringing),
    pending_ringing: users_v1(call.pending_ringing),
    recipients: users_v1(call.recipients),
    caller: option.map(call.caller, caller_v1),
    voice_states: call.voice_states
      |> store.to_list
      |> list.map(voice_state_v1),
    next_seq: call.next_seq,
    members: call.sessions
      |> dict.to_list
      |> list.map(fn(pair) { member_v1(pair.0, pair.1, call.detached) }),
    pending: call.pending
      |> dict.to_list
      |> list.map(pending_v1),
    initiator_ready: call.initiator_ready,
    created_at: call.created_at,
    history: call.history |> set.to_list |> users_v1,
    last_event: option.map(call.last_event, terms.encode),
    room_key: call.room_key,
    located: tail.located
      |> dict.to_list
      |> list.map(fn(pair) {
        wire_v1.LocatedV1(
          connection_id: voice_key.connection_text(pair.0),
          geo: terms.encode(pair.1),
        )
      }),
    nodes: nodes.known(),
  )
}

fn users_v1(users: List(UserId)) -> List(Int) {
  list.map(users, id.user_id_to_int)
}

fn caller_v1(caller: Caller) -> CallerV1 {
  wire_v1.CallerV1(
    user_id: id.user_id_to_int(caller.id),
    name: caller.name,
    avatar: caller.avatar,
  )
}

fn voice_state_v1(pair: #(voice_key.VoiceKey, Slot)) -> VoiceStateV1 {
  let #(found, slot) = pair
  wire_v1.VoiceStateV1(
    channel_id: id.channel_id_to_int(voice_key.channel(found)),
    user_id: id.user_id_to_int(voice_key.user(found)),
    connection_id: voice_key.connection_text(voice_key.connection(found)),
    seq: slot.seq,
    entry: terms.encode(slot.entry),
  )
}

fn member_v1(
  session_id: String,
  member: Member,
  detached: Dict(String, Int),
) -> MemberV1 {
  wire_v1.MemberV1(
    session_id:,
    user_id: id.user_id_to_int(member.user),
    inbox: terms.encode(member.inbox),
    owner: option.map(member.owner, terms.encode),
    detached_at: dict.get(detached, session_id) |> option.from_result,
  )
}

fn pending_v1(pair: #(ConnectionId, PendingJoin)) -> PendingJoinV1 {
  let #(conn, pending) = pair
  wire_v1.PendingJoinV1(
    connection_id: voice_key.connection_text(conn),
    user_id: id.user_id_to_int(pending.user),
    session_id: pending.session_id,
    joined_at: pending.joined_at,
  )
}

fn malformed(reason: String) -> CodecError {
  codec.Malformed(reason)
}

fn user_of(value: Int) -> Result(UserId, CodecError) {
  id.user_id(value) |> result.replace_error(malformed("user id"))
}

fn users_of(values: List(Int)) -> Result(List(UserId), CodecError) {
  list.try_map(values, user_of)
}

fn connection_of(text: String) -> Result(ConnectionId, CodecError) {
  voice_key.connection_id(text) |> result.replace_error(malformed("connection"))
}

fn optional(
  value: Option(a),
  decode: fn(a) -> Result(b, CodecError),
) -> Result(Option(b), CodecError) {
  case value {
    None -> Ok(None)
    Some(found) -> decode(found) |> result.map(Some)
  }
}

fn tail_of(wire: CallTailV1) -> Result(Tail, CodecError) {
  use channel <- result.try(
    id.channel_id(wire.channel_id) |> result.replace_error(malformed("channel")),
  )
  use message <- result.try(
    id.message_id(wire.message_id) |> result.replace_error(malformed("message")),
  )
  use ringing <- result.try(users_of(wire.ringing))
  use pending_ringing <- result.try(users_of(wire.pending_ringing))
  use recipients <- result.try(users_of(wire.recipients))
  use caller <- result.try(optional(wire.caller, caller_of))
  use voice_states <- result.try(list.try_map(wire.voice_states, slot_of))
  use members <- result.try(list.try_map(wire.members, member_of))
  use pending <- result.try(list.try_map(wire.pending, pending_of))
  use history <- result.try(users_of(wire.history))
  use last_event <- result.try(optional(wire.last_event, codec.term_of))
  use located <- result.try(list.try_map(wire.located, located_of))
  let call =
    Call(
      channel:,
      message:,
      region: wire.region,
      ringing:,
      pending_ringing:,
      recipients:,
      caller:,
      voice_states: store.from_list(voice_states),
      next_seq: wire.next_seq,
      sessions: members
        |> list.map(fn(found) { #(found.0, found.1) })
        |> dict.from_list,
      detached: members
        |> list.filter_map(fn(found) {
          case found.2 {
            Some(at) -> Ok(#(found.0, at))
            None -> Error(Nil)
          }
        })
        |> dict.from_list,
      pending: dict.from_list(pending),
      initiator_ready: wire.initiator_ready,
      ring_timers: dict.new(),
      idle_timer: None,
      created_at: wire.created_at,
      history: set.from_list(history),
      last_event:,
      room_key: wire.room_key,
    )
  Ok(Tail(call:, located: dict.from_list(located)))
}

fn caller_of(wire: CallerV1) -> Result(Caller, CodecError) {
  use user <- result.map(user_of(wire.user_id))
  Caller(id: user, name: wire.name, avatar: wire.avatar)
}

fn slot_of(
  wire: VoiceStateV1,
) -> Result(#(voice_key.VoiceKey, Slot), CodecError) {
  use channel <- result.try(
    id.channel_id(wire.channel_id) |> result.replace_error(malformed("channel")),
  )
  use user <- result.try(user_of(wire.user_id))
  use connection <- result.try(connection_of(wire.connection_id))
  use value <- result.map(codec.term_of(wire.entry))
  #(
    voice_key.new(channel:, user:, connection:),
    Slot(entry: value, seq: wire.seq),
  )
}

fn member_of(
  wire: MemberV1,
) -> Result(#(String, Member, Option(Int)), CodecError) {
  use user <- result.try(user_of(wire.user_id))
  use inbox <- result.try(codec.term_of(wire.inbox))
  use owner <- result.map(optional(wire.owner, codec.term_of))
  #(
    wire.session_id,
    state.Member(user:, inbox:, monitor: None, owner:, owner_monitor: None),
    wire.detached_at,
  )
}

fn pending_of(
  wire: PendingJoinV1,
) -> Result(#(ConnectionId, PendingJoin), CodecError) {
  use connection <- result.try(connection_of(wire.connection_id))
  use user <- result.map(user_of(wire.user_id))
  #(
    connection,
    PendingJoin(user:, session_id: wire.session_id, joined_at: wire.joined_at),
  )
}

fn located_of(wire: LocatedV1) -> Result(#(ConnectionId, Geo), CodecError) {
  use connection <- result.try(connection_of(wire.connection_id))
  use found <- result.map(codec.term_of(wire.geo))
  #(connection, found)
}

fn strings() -> Decoder(List(String)) {
  decode.list(decode.string)
}

fn ints() -> Decoder(List(Int)) {
  decode.list(decode.int)
}

fn target_decoder() -> Decoder(TargetV1) {
  use <- terms.record("target_v1")
  use nodes <- decode.field(1, strings())
  use subject <- decode.field(2, decode.bit_array)
  decode.success(wire_v1.TargetV1(nodes:, subject:))
}

fn inbox_decoder() -> Decoder(InboxV1) {
  use <- terms.record("inbox_v1")
  use nodes <- decode.field(1, strings())
  use message <- decode.field(2, decode.bit_array)
  decode.success(wire_v1.InboxV1(nodes:, message:))
}

fn caller_decoder() -> Decoder(CallerV1) {
  use <- terms.record("caller_v1")
  use user_id <- decode.field(1, decode.int)
  use name <- decode.field(2, decode.string)
  use avatar <- decode.field(3, terms.option(decode.string))
  decode.success(wire_v1.CallerV1(user_id:, name:, avatar:))
}

fn voice_state_decoder() -> Decoder(VoiceStateV1) {
  use <- terms.record("voice_state_v1")
  use channel_id <- decode.field(1, decode.int)
  use user_id <- decode.field(2, decode.int)
  use connection_id <- decode.field(3, decode.string)
  use seq <- decode.field(4, decode.int)
  use entry <- decode.field(5, decode.bit_array)
  decode.success(wire_v1.VoiceStateV1(
    channel_id:,
    user_id:,
    connection_id:,
    seq:,
    entry:,
  ))
}

fn member_decoder() -> Decoder(MemberV1) {
  use <- terms.record("member_v1")
  use session_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.int)
  use inbox <- decode.field(3, decode.bit_array)
  use owner <- decode.field(4, terms.option(decode.bit_array))
  use detached_at <- decode.field(5, terms.option(decode.int))
  decode.success(wire_v1.MemberV1(
    session_id:,
    user_id:,
    inbox:,
    owner:,
    detached_at:,
  ))
}

fn pending_decoder() -> Decoder(PendingJoinV1) {
  use <- terms.record("pending_join_v1")
  use connection_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.int)
  use session_id <- decode.field(3, decode.string)
  use joined_at <- decode.field(4, decode.int)
  decode.success(wire_v1.PendingJoinV1(
    connection_id:,
    user_id:,
    session_id:,
    joined_at:,
  ))
}

fn located_decoder() -> Decoder(LocatedV1) {
  use <- terms.record("located_v1")
  use connection_id <- decode.field(1, decode.string)
  use geo <- decode.field(2, decode.bit_array)
  decode.success(wire_v1.LocatedV1(connection_id:, geo:))
}

fn tail_decoder() -> Decoder(CallTailV1) {
  use <- terms.record("call_tail_v1")
  use channel_id <- decode.field(1, decode.int)
  use message_id <- decode.field(2, decode.int)
  use region <- decode.field(3, terms.option(decode.string))
  use ringing <- decode.field(4, ints())
  use pending_ringing <- decode.field(5, ints())
  use recipients <- decode.field(6, ints())
  use caller <- decode.field(7, terms.option(caller_decoder()))
  use voice_states <- decode.field(8, decode.list(voice_state_decoder()))
  use next_seq <- decode.field(9, decode.int)
  use members <- decode.field(10, decode.list(member_decoder()))
  use pending <- decode.field(11, decode.list(pending_decoder()))
  use initiator_ready <- decode.field(12, decode.bool)
  use created_at <- decode.field(13, decode.int)
  use history <- decode.field(14, ints())
  use last_event <- decode.field(15, terms.option(decode.bit_array))
  use room_key <- decode.field(16, terms.option(decode.string))
  use located <- decode.field(17, decode.list(located_decoder()))
  use nodes <- decode.field(18, strings())
  decode.success(wire_v1.CallTailV1(
    channel_id:,
    message_id:,
    region:,
    ringing:,
    pending_ringing:,
    recipients:,
    caller:,
    voice_states:,
    next_seq:,
    members:,
    pending:,
    initiator_ready:,
    created_at:,
    history:,
    last_event:,
    room_key:,
    located:,
    nodes:,
  ))
}
