// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/key
import fluxer_gateway/core/id
import fluxer_gateway/core/presence.{type Status} as core_presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/handoff/codec.{type Codec, type CodecError, Codec}
import fluxer_gateway/presence/custom_status.{type CustomStatus}
import fluxer_gateway/presence/dispatch_id.{type DispatchId}
import fluxer_gateway/presence/display
import fluxer_gateway/presence/event.{type PresenceEvent, PresenceEvent}
import fluxer_gateway/presence/handoff/terms
import fluxer_gateway/presence/handoff/wire_v1.{
  type CarriedSessionV1, type CarriedV1, type CatchupV1, type CustomStatusV1,
  type DispatchIdV1, type DmMessageV1, type DoomV1, type EpochV1, type EventV1,
  type GuildMembershipV1, type InboxV1, type OpValueV1, type PendingPushV1,
  type PresenceBlobV1, type ProfileV1, type PublishedV1, type PushBufferV1,
  type SessionRefV1, type SettingsValueV1, type SubscriberV1, type UserEventV1,
  type VersionV1,
}
import fluxer_gateway/presence/id_set
import fluxer_gateway/presence/profile.{type Profile}
import fluxer_gateway/presence/protocol.{
  type Carried, type Catchup, type Input, type StartRequest, type Subscriber,
  type UserEvent, Carried, CarriedSession, StartRequest,
}
import fluxer_gateway/presence/push_buffer.{type PushBuffer}
import fluxer_gateway/presence/version.{type Version}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/list
import gleam/option.{None, Some}
import gleam/result

pub fn range() -> CodecRange {
  CodecRange(
    ns: key.presence,
    oldest_read: wire_v1.version,
    max_read: wire_v1.version,
    max_write: wire_v1.version,
  )
}

pub fn tail() -> Codec(StartRequest) {
  Codec(
    ns: key.presence,
    part: blob.Tail,
    range: range(),
    encode_body: encode_tail,
    decode_body: decode_tail,
  )
}

pub fn inbox() -> Codec(Input(Nil)) {
  Codec(
    ns: key.presence,
    part: blob.Inbox,
    range: range(),
    encode_body: encode_inbox,
    decode_body: decode_inbox,
  )
}

pub fn encode_target(target: Subject(Input(Monitor))) -> BitArray {
  terms.encode(terms.subject(target))
}

pub fn decode_target(bytes: BitArray) -> Result(Subject(Input(Monitor)), Nil) {
  terms.decode(bytes, terms.subject_wire())
  |> result.try(terms.to_subject)
  |> result.replace_error(Nil)
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

fn malformed(reason: String) -> CodecError {
  codec.Malformed(reason)
}

fn encode_tail(
  value: StartRequest,
  version: Int,
) -> Result(BitArray, CodecError) {
  use Nil <- result.map(writable(version))
  terms.encode(blob_v1(value))
}

fn decode_tail(
  version: Int,
  body: BitArray,
) -> Result(StartRequest, CodecError) {
  use Nil <- result.try(readable(version))
  use wire <- result.try(
    terms.decode(body, blob_decoder()) |> result.map_error(malformed),
  )
  start_request(wire) |> result.map_error(malformed)
}

fn encode_inbox(
  value: Input(Nil),
  version: Int,
) -> Result(BitArray, CodecError) {
  use Nil <- result.try(writable(version))
  use wire <- result.map(
    inbox_v1(value)
    |> result.replace_error(malformed("the input does not cross nodes")),
  )
  terms.encode(wire)
}

fn decode_inbox(
  version: Int,
  body: BitArray,
) -> Result(Input(Nil), CodecError) {
  use Nil <- result.try(readable(version))
  use wire <- result.try(
    terms.decode(body, inbox_decoder()) |> result.map_error(malformed),
  )
  input(wire) |> result.map_error(malformed)
}

fn blob_v1(request: StartRequest) -> PresenceBlobV1 {
  wire_v1.PresenceBlobV1(
    user_id: request.user,
    profile: profile_v1(request.profile),
    custom_status: option.map(request.custom_status, custom_status_v1),
    settings_invisible: request.settings_invisible,
    guilds: id_set.to_list(request.guilds),
    targets: id_set.to_list(request.targets),
    push_max_entries: request.push_limits.max_entries,
    push_max_bytes: request.push_limits.max_bytes,
    carried: option.map(request.carried, carried_v1),
  )
}

fn start_request(wire: PresenceBlobV1) -> Result(StartRequest, String) {
  use profile <- result.try(profile_of(wire.profile))
  use carried <- result.map(case wire.carried {
    Some(found) -> carried_of(found) |> result.map(Some)
    None -> Ok(None)
  })
  StartRequest(
    user: wire.user_id,
    profile:,
    custom_status: option.map(wire.custom_status, custom_status_of),
    settings_invisible: wire.settings_invisible,
    guilds: id_set.from_list(wire.guilds),
    targets: id_set.from_list(wire.targets),
    push_limits: push_buffer.Limits(
      max_entries: wire.push_max_entries,
      max_bytes: wire.push_max_bytes,
    ),
    carried:,
  )
}

fn carried_v1(carried: Carried) -> CarriedV1 {
  wire_v1.CarriedV1(
    version: version_v1(carried.version),
    last: option.map(carried.last, fn(published) {
      wire_v1.PublishedV1(
        display: display.to_int(published.display),
        custom_status_key: published.custom_status_key,
        custom_status_wire: published.custom_status_wire,
      )
    }),
    current: option.map(carried.current, event_v1),
    temporary_guilds: id_set.to_list(carried.temporary_guilds),
    push: push_v1(carried.push),
    sessions: list.map(carried.sessions, fn(found) {
      wire_v1.CarriedSessionV1(
        session: session_ref_v1(found.session),
        status: core_presence.status_to_wire(found.status),
        afk: found.afk,
        mobile: found.mobile,
      )
    }),
    validating: option.map(carried.validating, custom_status_v1),
    delivered: list.map(
      dispatch_id.oldest_first(carried.delivered),
      dispatch_id_v1,
    ),
    doomed: list.map(carried.doomed, fn(row) {
      let #(session, doom, at_ms) = row
      wire_v1.DoomV1(
        session_id: session.session_id_to_string(session),
        matching: case doom {
          protocol.TerminateEvery -> None
          protocol.TerminateMatching(hashes) -> Some(hashes)
        },
        at_ms:,
      )
    }),
  )
}

fn carried_of(wire: CarriedV1) -> Result(Carried, String) {
  use last <- result.try(case wire.last {
    Some(published) -> published_of(published) |> result.map(Some)
    None -> Ok(None)
  })
  use current <- result.try(case wire.current {
    Some(found) -> event_of(found) |> result.map(Some)
    None -> Ok(None)
  })
  use sessions <- result.try(list.try_map(wire.sessions, carried_session_of))
  use doomed <- result.map(list.try_map(wire.doomed, doom_of))
  Carried(
    version: version_of(wire.version),
    last:,
    current:,
    temporary_guilds: id_set.from_list(wire.temporary_guilds),
    push: push_of(wire.push),
    sessions:,
    validating: option.map(wire.validating, custom_status_of),
    delivered: dispatch_id.from_oldest_first(list.map(
      wire.delivered,
      dispatch_id_of,
    )),
    doomed:,
  )
}

fn published_of(wire: PublishedV1) -> Result(protocol.Published, String) {
  use shown <- result.map(
    display.from_int(wire.display) |> result.replace_error("display"),
  )
  protocol.Published(
    display: shown,
    custom_status_key: wire.custom_status_key,
    custom_status_wire: wire.custom_status_wire,
  )
}

fn carried_session_of(
  wire: CarriedSessionV1,
) -> Result(protocol.CarriedSession, String) {
  use session <- result.try(session_ref_of(wire.session))
  use status <- result.map(status_of(wire.status))
  CarriedSession(session:, status:, afk: wire.afk, mobile: wire.mobile)
}

fn doom_of(
  wire: DoomV1,
) -> Result(#(session.SessionId, protocol.Doom, Int), String) {
  use found <- result.map(session_id_of(wire.session_id))
  let doom = case wire.matching {
    None -> protocol.TerminateEvery
    Some(hashes) -> protocol.TerminateMatching(hashes)
  }
  #(found, doom, wire.at_ms)
}

fn status_of(text: String) -> Result(Status, String) {
  core_presence.status_from_wire(text) |> result.replace_error("status")
}

fn session_id_of(text: String) -> Result(session.SessionId, String) {
  session.session_id_from_string(text) |> result.replace_error("session id")
}

fn session_ref_v1(found: SessionRef) -> SessionRefV1 {
  wire_v1.SessionRefV1(
    session_id: session.session_id_to_string(found.id),
    user_id: id.user_id_to_int(found.user),
    inbox: terms.subject(found.inbox),
  )
}

fn session_ref_of(wire: SessionRefV1) -> Result(SessionRef, String) {
  use found <- result.try(session_id_of(wire.session_id))
  use user <- result.try(
    id.user_id(wire.user_id) |> result.replace_error("user id"),
  )
  use inbox <- result.map(terms.to_subject(wire.inbox))
  SessionRef(id: found, user:, inbox:)
}

fn version_v1(found: Version) -> VersionV1 {
  let #(node, minted_ns, unique) = version.epoch_parts(version.epoch(found))
  wire_v1.VersionV1(
    epoch: wire_v1.EpochV1(node: atom.to_string(node), minted_ns:, unique:),
    seq: version.seq(found),
  )
}

fn version_of(wire: VersionV1) -> Version {
  let epoch: EpochV1 = wire.epoch
  version.restore(
    version.restore_epoch(
      atom.create(epoch.node),
      epoch.minted_ns,
      epoch.unique,
    ),
    wire.seq,
  )
}

fn dispatch_id_v1(found: DispatchId) -> DispatchIdV1 {
  let #(node, serial) = dispatch_id.parts(found)
  wire_v1.DispatchIdV1(node: atom.to_string(node), serial:)
}

fn dispatch_id_of(wire: DispatchIdV1) -> DispatchId {
  dispatch_id.restore(atom.create(wire.node), wire.serial)
}

fn custom_status_v1(status: CustomStatus) -> CustomStatusV1 {
  wire_v1.CustomStatusV1(
    wire: custom_status.wire(status),
    expires_at_ms: custom_status.expires_at_ms(status),
    payload: custom_status.payload(status),
  )
}

fn custom_status_of(wire: CustomStatusV1) -> CustomStatus {
  custom_status.restore(
    wire: wire.wire,
    expires_at_ms: wire.expires_at_ms,
    payload: wire.payload,
  )
}

fn profile_v1(found: Profile) -> ProfileV1 {
  wire_v1.ProfileV1(
    fields: list.map(profile.fields(found), fn(pair) {
      #(profile.name_of(pair.0), pair.1)
    }),
    restricted: profile.restricted(found),
    bot: profile.is_bot(found),
  )
}

fn profile_of(wire: ProfileV1) -> Result(Profile, String) {
  use fields <- result.map(
    list.try_map(wire.fields, fn(pair) {
      profile.field_named(pair.0)
      |> result.map(fn(field) { #(field, pair.1) })
      |> result.replace_error("profile field " <> pair.0)
    }),
  )
  profile.new(fields:, restricted: wire.restricted, bot: wire.bot)
}

fn event_v1(found: PresenceEvent) -> EventV1 {
  wire_v1.EventV1(
    user_updated: found.kind == event.UserUpdated,
    user: found.user,
    version: version_v1(found.version),
    display: display.to_int(found.display),
    custom_status: found.custom_status,
    user_wire: found.user_wire,
    wire: found.wire,
  )
}

fn event_of(wire: EventV1) -> Result(PresenceEvent, String) {
  use shown <- result.map(
    display.from_int(wire.display) |> result.replace_error("display"),
  )
  PresenceEvent(
    kind: case wire.user_updated {
      True -> event.UserUpdated
      False -> event.StatusChanged
    },
    user: wire.user,
    version: version_of(wire.version),
    display: shown,
    custom_status: wire.custom_status,
    user_wire: wire.user_wire,
    wire: wire.wire,
    facts: Some(event.scan(wire.wire)),
  )
}

fn push_v1(buffer: PushBuffer) -> PushBufferV1 {
  wire_v1.PushBufferV1(
    entries: list.map(push_buffer.kept(buffer), fn(kept) {
      wire_v1.PendingPushV1(
        channel_id: kept.channel,
        message_id: kept.message,
        data: raw_json.to_bits(kept.data),
        author_id: kept.author,
        buffered_at_ms: kept.buffered_at,
      )
    }),
    read_marks: push_buffer.read_marks(buffer),
    eligible: push_buffer.is_eligible(buffer),
  )
}

fn push_of(wire: PushBufferV1) -> PushBuffer {
  push_buffer.restore(
    kept: list.map(wire.entries, fn(entry: PendingPushV1) {
      push_buffer.Kept(
        channel: entry.channel_id,
        message: entry.message_id,
        data: raw_json.wrap(entry.data),
        author: entry.author_id,
        buffered_at: entry.buffered_at_ms,
      )
    }),
    read_marks: wire.read_marks,
    eligible: wire.eligible,
  )
}

fn subscriber_v1(to: Subscriber) -> SubscriberV1 {
  case to {
    protocol.PresenceSubscriber(user:, inbox:) ->
      wire_v1.PresenceSubscriberV1(user:, inbox: terms.subject(inbox))
    protocol.GuildSubscriber(guild:, inbox:) ->
      wire_v1.GuildSubscriberV1(guild:, inbox: terms.subject(inbox))
  }
}

fn subscriber_of(wire: SubscriberV1) -> Result(Subscriber, String) {
  case wire {
    wire_v1.PresenceSubscriberV1(user:, inbox:) ->
      terms.to_subject(inbox)
      |> result.map(fn(inbox) { protocol.PresenceSubscriber(user:, inbox:) })
    wire_v1.GuildSubscriberV1(guild:, inbox:) ->
      terms.to_subject(inbox)
      |> result.map(fn(inbox) { protocol.GuildSubscriber(guild:, inbox:) })
  }
}

fn catchup_v1(catchup: Catchup) -> CatchupV1 {
  case catchup {
    protocol.Everything -> wire_v1.EverythingV1
    protocol.ChangedSince(ms:, stamp:) -> wire_v1.ChangedSinceV1(ms:, stamp:)
  }
}

fn op_value_v1(value: custom_status.OpValue) -> OpValueV1 {
  case value {
    custom_status.OpAbsent -> wire_v1.OpAbsentV1
    custom_status.OpNull -> wire_v1.OpNullV1
    custom_status.OpMap(status) -> wire_v1.OpMapV1(custom_status_v1(status))
    custom_status.OpOther -> wire_v1.OpOtherV1
  }
}

fn op_value_of(wire: OpValueV1) -> custom_status.OpValue {
  case wire {
    wire_v1.OpAbsentV1 -> custom_status.OpAbsent
    wire_v1.OpNullV1 -> custom_status.OpNull
    wire_v1.OpMapV1(status) -> custom_status.OpMap(custom_status_of(status))
    wire_v1.OpOtherV1 -> custom_status.OpOther
  }
}

fn user_event_v1(parsed: UserEvent) -> UserEventV1 {
  case parsed {
    protocol.UserUpdate(found) -> wire_v1.UserUpdateV1(profile_v1(found))
    protocol.UserSettingsUpdate(custom_status: value, invisible:) ->
      wire_v1.UserSettingsUpdateV1(
        custom_status: case value {
          custom_status.SettingsAbsent -> wire_v1.SettingsAbsentV1
          custom_status.SettingsMap(status) ->
            wire_v1.SettingsMapV1(custom_status_v1(status))
          custom_status.SettingsOther -> wire_v1.SettingsOtherV1
        },
        invisible:,
      )
    protocol.MessageCreate(message) ->
      wire_v1.MessageCreateV1(
        option.map(message, fn(found) {
          wire_v1.DmMessageV1(
            author: found.author,
            channel: found.channel,
            message: found.message,
            suppressed: found.suppressed,
            data: raw_json.to_bits(found.data),
          )
        }),
      )
    protocol.MessageAck(channel:, message:) ->
      wire_v1.MessageAckV1(channel:, message:)
    protocol.OtherEvent -> wire_v1.OtherEventV1
  }
}

fn user_event_of(wire: UserEventV1) -> Result(UserEvent, String) {
  case wire {
    wire_v1.UserUpdateV1(found) ->
      profile_of(found) |> result.map(protocol.UserUpdate)
    wire_v1.UserSettingsUpdateV1(custom_status: value, invisible:) ->
      Ok(protocol.UserSettingsUpdate(
        custom_status: case value {
          wire_v1.SettingsAbsentV1 -> custom_status.SettingsAbsent
          wire_v1.SettingsMapV1(status) ->
            custom_status.SettingsMap(custom_status_of(status))
          wire_v1.SettingsOtherV1 -> custom_status.SettingsOther
        },
        invisible:,
      ))
    wire_v1.MessageCreateV1(message) ->
      Ok(
        protocol.MessageCreate(
          option.map(message, fn(found: DmMessageV1) {
            push_buffer.DmMessage(
              author: found.author,
              channel: found.channel,
              message: found.message,
              suppressed: found.suppressed,
              data: raw_json.wrap(found.data),
            )
          }),
        ),
      )
    wire_v1.MessageAckV1(channel:, message:) ->
      Ok(protocol.MessageAck(channel:, message:))
    wire_v1.OtherEventV1 -> Ok(protocol.OtherEvent)
  }
}

fn membership_v1(operation: protocol.GuildMembership) -> GuildMembershipV1 {
  case operation {
    protocol.JoinGuild(guild) -> wire_v1.JoinGuildV1(guild)
    protocol.LeaveGuild(guild) -> wire_v1.LeaveGuildV1(guild)
    protocol.AddTemporaryGuild(guild) -> wire_v1.AddTemporaryGuildV1(guild)
    protocol.RemoveTemporaryGuild(guild) ->
      wire_v1.RemoveTemporaryGuildV1(guild)
  }
}

fn membership_of(wire: GuildMembershipV1) -> protocol.GuildMembership {
  case wire {
    wire_v1.JoinGuildV1(guild) -> protocol.JoinGuild(guild)
    wire_v1.LeaveGuildV1(guild) -> protocol.LeaveGuild(guild)
    wire_v1.AddTemporaryGuildV1(guild) -> protocol.AddTemporaryGuild(guild)
    wire_v1.RemoveTemporaryGuildV1(guild) ->
      protocol.RemoveTemporaryGuild(guild)
  }
}

pub fn crosses(value: Input(a)) -> Bool {
  result.is_ok(inbox_v1(value))
}

fn inbox_v1(value: Input(a)) -> Result(InboxV1, Nil) {
  case value {
    protocol.ConnectSession(
      session:,
      status:,
      afk:,
      mobile:,
      guilds:,
      targets:,
      reply:,
    ) ->
      Ok(wire_v1.ConnectSessionV1(
        session: session_ref_v1(session),
        status: core_presence.status_to_wire(status),
        afk:,
        mobile:,
        guilds: id_set.to_list(guilds),
        targets: id_set.to_list(targets),
        reply: terms.subject(reply),
      ))
    protocol.Disconnect(session: found, owner:) ->
      Ok(wire_v1.DisconnectV1(
        session_id: session.session_id_to_string(found),
        owner: terms.subject(owner),
      ))
    protocol.Update(session: found, status:, afk:, mobile:, custom_status:) ->
      Ok(wire_v1.UpdateV1(
        session_id: session.session_id_to_string(found),
        status: core_presence.status_to_wire(status),
        afk:,
        mobile:,
        custom_status: op_value_v1(custom_status),
      ))
    protocol.SubscribeTargets(users) ->
      Ok(wire_v1.SubscribeTargetsV1(id_set.to_list(users)))
    protocol.UnsubscribeTargets(users) ->
      Ok(wire_v1.UnsubscribeTargetsV1(id_set.to_list(users)))
    protocol.Foreign(found) -> Ok(wire_v1.ForeignV1(event_v1(found)))
    protocol.Resend(to) -> Ok(wire_v1.ResendV1(subscriber_v1(to)))
    protocol.ResendNode(node) -> Ok(wire_v1.ResendNodeV1(atom.to_string(node)))
    protocol.ResendSince(to:, since_ms:) ->
      Ok(wire_v1.ResendSinceV1(subscriber_v1(to), since_ms))
    protocol.Joined(to:, catchup:) ->
      Ok(wire_v1.JoinedV1(subscriber_v1(to), catchup_v1(catchup)))
    protocol.UserDispatch(id: found, event: name, data:, parsed:) ->
      Ok(wire_v1.UserDispatchV1(
        id: dispatch_id_v1(found),
        event: name,
        data: raw_json.to_bits(data),
        parsed: user_event_v1(parsed),
      ))
    protocol.Guild(operation) -> Ok(wire_v1.GuildV1(membership_v1(operation)))
    protocol.VisitStart(guild:, operator:) ->
      Ok(wire_v1.VisitStartV1(guild:, operator:))
    protocol.VisitEnd(guild) -> Ok(wire_v1.VisitEndV1(guild))
    protocol.TerminateSessions(hashes:, reply:) ->
      Ok(wire_v1.TerminateSessionsV1(hashes:, reply: terms.subject(reply)))
    protocol.TerminateAll -> Ok(wire_v1.TerminateAllV1)
    protocol.Rejoin -> Ok(wire_v1.RejoinV1)
    protocol.Connect(..)
    | protocol.RepointSession(..)
    | protocol.Repoint(..)
    | protocol.SessionDown(..)
    | protocol.SnapshotRead(..)
    | protocol.Validated(..)
    | protocol.TerminateReply(..)
    | protocol.ExpiryTick
    | protocol.GraceExpired(..)
    | protocol.IdleCheck
    | protocol.MoveTo(..)
    | protocol.MoveRetry(..)
    | protocol.MoveFinished(..)
    | protocol.AdoptHandoff(..)
    | protocol.Adopt(..)
    | protocol.ShedTick
    | protocol.SubscribeAfterStart(..) -> Error(Nil)
  }
}

fn input(wire: InboxV1) -> Result(Input(Nil), String) {
  case wire {
    wire_v1.ConnectSessionV1(
      session:,
      status:,
      afk:,
      mobile:,
      guilds:,
      targets:,
      reply:,
    ) -> {
      use session <- result.try(session_ref_of(session))
      use status <- result.try(status_of(status))
      use reply <- result.map(terms.to_subject(reply))
      protocol.ConnectSession(
        session:,
        status:,
        afk:,
        mobile:,
        guilds: id_set.from_list(guilds),
        targets: id_set.from_list(targets),
        reply:,
      )
    }
    wire_v1.DisconnectV1(session_id:, owner:) -> {
      use found <- result.try(session_id_of(session_id))
      use owner <- result.map(terms.to_subject(owner))
      protocol.Disconnect(session: found, owner:)
    }
    wire_v1.UpdateV1(session_id:, status:, afk:, mobile:, custom_status:) -> {
      use found <- result.try(session_id_of(session_id))
      use status <- result.map(status_of(status))
      protocol.Update(
        session: found,
        status:,
        afk:,
        mobile:,
        custom_status: op_value_of(custom_status),
      )
    }
    wire_v1.SubscribeTargetsV1(users) ->
      Ok(protocol.SubscribeTargets(id_set.from_list(users)))
    wire_v1.UnsubscribeTargetsV1(users) ->
      Ok(protocol.UnsubscribeTargets(id_set.from_list(users)))
    wire_v1.ForeignV1(found) -> event_of(found) |> result.map(protocol.Foreign)
    wire_v1.ResendV1(to) -> subscriber_of(to) |> result.map(protocol.Resend)
    wire_v1.ResendNodeV1(node) -> Ok(protocol.ResendNode(atom.create(node)))
    wire_v1.ResendSinceV1(to:, since_ms:) ->
      subscriber_of(to)
      |> result.map(fn(to) { protocol.ResendSince(to:, since_ms:) })
    wire_v1.JoinedV1(to:, catchup: _) ->
      subscriber_of(to)
      |> result.map(fn(to) {
        protocol.Joined(to:, catchup: protocol.Everything)
      })
    wire_v1.UserDispatchV1(id: found, event: name, data:, parsed:) ->
      user_event_of(parsed)
      |> result.map(fn(parsed) {
        protocol.UserDispatch(
          id: dispatch_id_of(found),
          event: name,
          data: raw_json.wrap(data),
          parsed:,
        )
      })
    wire_v1.GuildV1(operation) -> Ok(protocol.Guild(membership_of(operation)))
    wire_v1.VisitStartV1(guild:, operator:) ->
      Ok(protocol.VisitStart(guild:, operator:))
    wire_v1.VisitEndV1(guild) -> Ok(protocol.VisitEnd(guild))
    wire_v1.TerminateSessionsV1(hashes:, reply:) ->
      terms.to_subject(reply)
      |> result.map(fn(reply) { protocol.TerminateSessions(hashes:, reply:) })
    wire_v1.TerminateAllV1 -> Ok(protocol.TerminateAll)
    wire_v1.RejoinV1 -> Ok(protocol.Rejoin)
  }
}

fn int_list() -> Decoder(List(Int)) {
  decode.list(decode.int)
}

fn epoch_decoder() -> Decoder(EpochV1) {
  use <- terms.record("epoch_v1")
  use node <- decode.field(1, decode.string)
  use minted_ns <- decode.field(2, decode.int)
  use unique <- decode.field(3, decode.int)
  decode.success(wire_v1.EpochV1(node:, minted_ns:, unique:))
}

fn version_decoder() -> Decoder(VersionV1) {
  use <- terms.record("version_v1")
  use epoch <- decode.field(1, epoch_decoder())
  use seq <- decode.field(2, decode.int)
  decode.success(wire_v1.VersionV1(epoch:, seq:))
}

fn dispatch_id_decoder() -> Decoder(DispatchIdV1) {
  use <- terms.record("dispatch_id_v1")
  use node <- decode.field(1, decode.string)
  use serial <- decode.field(2, decode.int)
  decode.success(wire_v1.DispatchIdV1(node:, serial:))
}

fn custom_status_decoder() -> Decoder(CustomStatusV1) {
  use <- terms.record("custom_status_v1")
  use wire <- decode.field(1, decode.bit_array)
  use expires_at_ms <- decode.field(2, terms.option(decode.int))
  use payload <- decode.field(
    3,
    decode.list(terms.pair(decode.string, decode.bit_array)),
  )
  decode.success(wire_v1.CustomStatusV1(wire:, expires_at_ms:, payload:))
}

fn profile_decoder() -> Decoder(ProfileV1) {
  use <- terms.record("profile_v1")
  use fields <- decode.field(
    1,
    decode.list(terms.pair(decode.string, decode.bit_array)),
  )
  use restricted <- decode.field(2, decode.bool)
  use bot <- decode.field(3, decode.bool)
  decode.success(wire_v1.ProfileV1(fields:, restricted:, bot:))
}

fn event_decoder() -> Decoder(EventV1) {
  use <- terms.record("event_v1")
  use user_updated <- decode.field(1, decode.bool)
  use user <- decode.field(2, decode.int)
  use version <- decode.field(3, version_decoder())
  use display <- decode.field(4, decode.int)
  use custom_status <- decode.field(5, decode.bit_array)
  use user_wire <- decode.field(6, decode.bit_array)
  use wire <- decode.field(7, decode.bit_array)
  decode.success(wire_v1.EventV1(
    user_updated:,
    user:,
    version:,
    display:,
    custom_status:,
    user_wire:,
    wire:,
  ))
}

fn published_decoder() -> Decoder(PublishedV1) {
  use <- terms.record("published_v1")
  use display <- decode.field(1, decode.int)
  use custom_status_key <- decode.field(2, decode.bit_array)
  use custom_status_wire <- decode.field(3, decode.bit_array)
  decode.success(wire_v1.PublishedV1(
    display:,
    custom_status_key:,
    custom_status_wire:,
  ))
}

fn session_ref_decoder() -> Decoder(SessionRefV1) {
  use <- terms.record("session_ref_v1")
  use session_id <- decode.field(1, decode.string)
  use user_id <- decode.field(2, decode.int)
  use inbox <- decode.field(3, terms.subject_wire())
  decode.success(wire_v1.SessionRefV1(session_id:, user_id:, inbox:))
}

fn carried_session_decoder() -> Decoder(CarriedSessionV1) {
  use <- terms.record("carried_session_v1")
  use session <- decode.field(1, session_ref_decoder())
  use status <- decode.field(2, decode.string)
  use afk <- decode.field(3, decode.bool)
  use mobile <- decode.field(4, decode.bool)
  decode.success(wire_v1.CarriedSessionV1(session:, status:, afk:, mobile:))
}

fn pending_push_decoder() -> Decoder(PendingPushV1) {
  use <- terms.record("pending_push_v1")
  use channel_id <- decode.field(1, decode.int)
  use message_id <- decode.field(2, decode.int)
  use data <- decode.field(3, decode.bit_array)
  use author_id <- decode.field(4, decode.int)
  use buffered_at_ms <- decode.field(5, decode.int)
  decode.success(wire_v1.PendingPushV1(
    channel_id:,
    message_id:,
    data:,
    author_id:,
    buffered_at_ms:,
  ))
}

fn push_decoder() -> Decoder(PushBufferV1) {
  use <- terms.record("push_buffer_v1")
  use entries <- decode.field(1, decode.list(pending_push_decoder()))
  use read_marks <- decode.field(
    2,
    decode.list(terms.pair(decode.int, decode.int)),
  )
  use eligible <- decode.field(3, decode.bool)
  decode.success(wire_v1.PushBufferV1(entries:, read_marks:, eligible:))
}

fn doom_decoder() -> Decoder(DoomV1) {
  use <- terms.record("doom_v1")
  use session_id <- decode.field(1, decode.string)
  use matching <- decode.field(2, terms.option(decode.list(decode.string)))
  use at_ms <- decode.field(3, decode.int)
  decode.success(wire_v1.DoomV1(session_id:, matching:, at_ms:))
}

fn carried_decoder() -> Decoder(CarriedV1) {
  use <- terms.record("carried_v1")
  use version <- decode.field(1, version_decoder())
  use last <- decode.field(2, terms.option(published_decoder()))
  use current <- decode.field(3, terms.option(event_decoder()))
  use temporary_guilds <- decode.field(4, int_list())
  use push <- decode.field(5, push_decoder())
  use sessions <- decode.field(6, decode.list(carried_session_decoder()))
  use validating <- decode.field(7, terms.option(custom_status_decoder()))
  use delivered <- decode.field(8, decode.list(dispatch_id_decoder()))
  use doomed <- decode.field(9, decode.list(doom_decoder()))
  decode.success(wire_v1.CarriedV1(
    version:,
    last:,
    current:,
    temporary_guilds:,
    push:,
    sessions:,
    validating:,
    delivered:,
    doomed:,
  ))
}

fn blob_decoder() -> Decoder(PresenceBlobV1) {
  use <- terms.record("presence_blob_v1")
  use user_id <- decode.field(1, decode.int)
  use profile <- decode.field(2, profile_decoder())
  use custom_status <- decode.field(3, terms.option(custom_status_decoder()))
  use settings_invisible <- decode.field(4, decode.bool)
  use guilds <- decode.field(5, int_list())
  use targets <- decode.field(6, int_list())
  use push_max_entries <- decode.field(7, decode.int)
  use push_max_bytes <- decode.field(8, decode.int)
  use carried <- decode.field(9, terms.option(carried_decoder()))
  decode.success(wire_v1.PresenceBlobV1(
    user_id:,
    profile:,
    custom_status:,
    settings_invisible:,
    guilds:,
    targets:,
    push_max_entries:,
    push_max_bytes:,
    carried:,
  ))
}

fn subscriber_decoder() -> Decoder(SubscriberV1) {
  decode.one_of(
    {
      use <- terms.record("presence_subscriber_v1")
      use user <- decode.field(1, decode.int)
      use inbox <- decode.field(2, terms.subject_wire())
      decode.success(wire_v1.PresenceSubscriberV1(user:, inbox:))
    },
    [
      {
        use <- terms.record("guild_subscriber_v1")
        use guild <- decode.field(1, decode.int)
        use inbox <- decode.field(2, terms.subject_wire())
        decode.success(wire_v1.GuildSubscriberV1(guild:, inbox:))
      },
    ],
  )
}

fn catchup_decoder() -> Decoder(CatchupV1) {
  decode.one_of(
    decode.map(terms.tag("everything_v1"), fn(_) { wire_v1.EverythingV1 }),
    [
      {
        use <- terms.record("changed_since_v1")
        use ms <- decode.field(1, decode.int)
        use stamp <- decode.field(2, decode.int)
        decode.success(wire_v1.ChangedSinceV1(ms:, stamp:))
      },
    ],
  )
}

fn op_value_decoder() -> Decoder(OpValueV1) {
  decode.one_of(
    decode.map(terms.tag("op_absent_v1"), fn(_) { wire_v1.OpAbsentV1 }),
    [
      decode.map(terms.tag("op_null_v1"), fn(_) { wire_v1.OpNullV1 }),
      decode.map(terms.tag("op_other_v1"), fn(_) { wire_v1.OpOtherV1 }),
      {
        use <- terms.record("op_map_v1")
        use status <- decode.field(1, custom_status_decoder())
        decode.success(wire_v1.OpMapV1(status))
      },
    ],
  )
}

fn settings_value_decoder() -> Decoder(SettingsValueV1) {
  decode.one_of(
    decode.map(terms.tag("settings_absent_v1"), fn(_) {
      wire_v1.SettingsAbsentV1
    }),
    [
      decode.map(terms.tag("settings_other_v1"), fn(_) {
        wire_v1.SettingsOtherV1
      }),
      {
        use <- terms.record("settings_map_v1")
        use status <- decode.field(1, custom_status_decoder())
        decode.success(wire_v1.SettingsMapV1(status))
      },
    ],
  )
}

fn dm_message_decoder() -> Decoder(DmMessageV1) {
  use <- terms.record("dm_message_v1")
  use author <- decode.field(1, decode.int)
  use channel <- decode.field(2, terms.option(decode.int))
  use message <- decode.field(3, terms.option(decode.int))
  use suppressed <- decode.field(4, decode.bool)
  use data <- decode.field(5, decode.bit_array)
  decode.success(wire_v1.DmMessageV1(
    author:,
    channel:,
    message:,
    suppressed:,
    data:,
  ))
}

fn user_event_decoder() -> Decoder(UserEventV1) {
  decode.one_of(
    decode.map(terms.tag("other_event_v1"), fn(_) { wire_v1.OtherEventV1 }),
    [
      {
        use <- terms.record("user_update_v1")
        use found <- decode.field(1, profile_decoder())
        decode.success(wire_v1.UserUpdateV1(found))
      },
      {
        use <- terms.record("user_settings_update_v1")
        use value <- decode.field(1, settings_value_decoder())
        use invisible <- decode.field(2, terms.option(decode.bool))
        decode.success(wire_v1.UserSettingsUpdateV1(
          custom_status: value,
          invisible:,
        ))
      },
      {
        use <- terms.record("message_create_v1")
        use message <- decode.field(1, terms.option(dm_message_decoder()))
        decode.success(wire_v1.MessageCreateV1(message))
      },
      {
        use <- terms.record("message_ack_v1")
        use channel <- decode.field(1, terms.option(decode.int))
        use message <- decode.field(2, terms.option(decode.int))
        decode.success(wire_v1.MessageAckV1(channel:, message:))
      },
    ],
  )
}

fn membership_decoder() -> Decoder(GuildMembershipV1) {
  use tag <- decode.field(0, atom.decoder())
  use guild <- decode.field(1, decode.int)
  case atom.to_string(tag) {
    "join_guild_v1" -> decode.success(wire_v1.JoinGuildV1(guild))
    "leave_guild_v1" -> decode.success(wire_v1.LeaveGuildV1(guild))
    "add_temporary_guild_v1" ->
      decode.success(wire_v1.AddTemporaryGuildV1(guild))
    "remove_temporary_guild_v1" ->
      decode.success(wire_v1.RemoveTemporaryGuildV1(guild))
    _ -> decode.failure(wire_v1.JoinGuildV1(0), "GuildMembershipV1")
  }
}

fn inbox_decoder() -> Decoder(InboxV1) {
  decode.one_of(
    decode.map(terms.tag("terminate_all_v1"), fn(_) { wire_v1.TerminateAllV1 }),
    [
      decode.map(terms.tag("rejoin_v1"), fn(_) { wire_v1.RejoinV1 }),
      tagged_inbox_decoder(),
    ],
  )
}

fn tagged_inbox_decoder() -> Decoder(InboxV1) {
  use tag <- decode.field(0, atom.decoder())
  case atom.to_string(tag) {
    "connect_session_v1" -> {
      use session <- decode.field(1, session_ref_decoder())
      use status <- decode.field(2, decode.string)
      use afk <- decode.field(3, decode.bool)
      use mobile <- decode.field(4, decode.bool)
      use guilds <- decode.field(5, int_list())
      use targets <- decode.field(6, int_list())
      use reply <- decode.field(7, terms.subject_wire())
      decode.success(wire_v1.ConnectSessionV1(
        session:,
        status:,
        afk:,
        mobile:,
        guilds:,
        targets:,
        reply:,
      ))
    }
    "disconnect_v1" -> {
      use session_id <- decode.field(1, decode.string)
      use owner <- decode.field(2, terms.subject_wire())
      decode.success(wire_v1.DisconnectV1(session_id:, owner:))
    }
    "update_v1" -> {
      use session_id <- decode.field(1, decode.string)
      use status <- decode.field(2, decode.string)
      use afk <- decode.field(3, decode.bool)
      use mobile <- decode.field(4, decode.bool)
      use custom_status <- decode.field(5, op_value_decoder())
      decode.success(wire_v1.UpdateV1(
        session_id:,
        status:,
        afk:,
        mobile:,
        custom_status:,
      ))
    }
    "subscribe_targets_v1" -> {
      use users <- decode.field(1, int_list())
      decode.success(wire_v1.SubscribeTargetsV1(users))
    }
    "unsubscribe_targets_v1" -> {
      use users <- decode.field(1, int_list())
      decode.success(wire_v1.UnsubscribeTargetsV1(users))
    }
    "foreign_v1" -> {
      use found <- decode.field(1, event_decoder())
      decode.success(wire_v1.ForeignV1(found))
    }
    "resend_v1" -> {
      use to <- decode.field(1, subscriber_decoder())
      decode.success(wire_v1.ResendV1(to))
    }
    "resend_node_v1" -> {
      use node <- decode.field(1, decode.string)
      decode.success(wire_v1.ResendNodeV1(node))
    }
    "resend_since_v1" -> {
      use to <- decode.field(1, subscriber_decoder())
      use since_ms <- decode.field(2, decode.int)
      decode.success(wire_v1.ResendSinceV1(to:, since_ms:))
    }
    "joined_v1" -> {
      use to <- decode.field(1, subscriber_decoder())
      use catchup <- decode.field(2, catchup_decoder())
      decode.success(wire_v1.JoinedV1(to:, catchup:))
    }
    "user_dispatch_v1" -> {
      use found <- decode.field(1, dispatch_id_decoder())
      use name <- decode.field(2, decode.string)
      use data <- decode.field(3, decode.bit_array)
      use parsed <- decode.field(4, user_event_decoder())
      decode.success(wire_v1.UserDispatchV1(
        id: found,
        event: name,
        data:,
        parsed:,
      ))
    }
    "guild_v1" -> {
      use operation <- decode.field(1, membership_decoder())
      decode.success(wire_v1.GuildV1(operation))
    }
    "visit_start_v1" -> {
      use guild <- decode.field(1, decode.int)
      use operator <- decode.field(2, decode.bool)
      decode.success(wire_v1.VisitStartV1(guild:, operator:))
    }
    "visit_end_v1" -> {
      use guild <- decode.field(1, decode.int)
      decode.success(wire_v1.VisitEndV1(guild))
    }
    "terminate_sessions_v1" -> {
      use hashes <- decode.field(1, decode.list(decode.string))
      use reply <- decode.field(2, terms.subject_wire())
      decode.success(wire_v1.TerminateSessionsV1(hashes:, reply:))
    }
    other -> decode.failure(wire_v1.TerminateAllV1, "InboxV1 " <> other)
  }
}
