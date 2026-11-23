// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type SessionId, type SessionInbox, type SessionRef,
}
import fluxer_gateway/presence/custom_status.{
  type CustomStatus, type OpValue, type SettingsValue, type ValidationReply,
}
import fluxer_gateway/presence/dispatch_id.{type DispatchId, type Recent}
import fluxer_gateway/presence/display.{type Display}
import fluxer_gateway/presence/event.{type PresenceEvent}
import fluxer_gateway/presence/id_set.{type IdSet}
import fluxer_gateway/presence/profile.{type Profile}
import fluxer_gateway/presence/push_buffer.{
  type DmMessage, type Limits, type PushBuffer,
}
import fluxer_gateway/presence/version.{type Version}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Subject}
import gleam/option.{type Option}

pub type Subscriber {
  PresenceSubscriber(user: Int, inbox: Subject(Input(Monitor)))
  GuildSubscriber(guild: Int, inbox: Subject(GuildRequest))
}

pub type Catchup {
  Everything
  ChangedSince(ms: Int, stamp: Int)
}

pub type Published {
  Published(
    display: Display,
    custom_status_key: BitArray,
    custom_status_wire: BitArray,
  )
}

pub type CarriedSession {
  CarriedSession(session: SessionRef, status: Status, afk: Bool, mobile: Bool)
}

pub type Doom {
  TerminateEvery
  TerminateMatching(hashes: List(String))
}

pub type Carried {
  Carried(
    version: Version,
    last: Option(Published),
    current: Option(PresenceEvent),
    temporary_guilds: IdSet,
    push: PushBuffer,
    sessions: List(CarriedSession),
    validating: Option(CustomStatus),
    delivered: Recent,
    doomed: List(#(SessionId, Doom, Int)),
  )
}

pub type StartRequest {
  StartRequest(
    user: Int,
    profile: Profile,
    custom_status: Option(CustomStatus),
    settings_invisible: Bool,
    guilds: IdSet,
    targets: IdSet,
    push_limits: Limits,
    carried: Option(Carried),
  )
}

pub type ConnectRequest(monitor) {
  ConnectRequest(
    session: SessionRef,
    status: Status,
    afk: Bool,
    mobile: Bool,
    guilds: IdSet,
    targets: IdSet,
    monitor: monitor,
  )
}

pub type SessionEnd {
  Closed
  Unreachable
  HandedOff
}

pub type ConnectReply {
  ConnectReply(sessions_replace: BitArray)
  ConnectMoved(to: Subject(Input(Monitor)))
}

pub type UserEvent {
  UserUpdate(Profile)
  UserSettingsUpdate(custom_status: SettingsValue, invisible: Option(Bool))
  MessageCreate(Option(DmMessage))
  MessageAck(channel: Option(Int), message: Option(Int))
  OtherEvent
}

pub type GuildMembership {
  JoinGuild(Int)
  LeaveGuild(Int)
  AddTemporaryGuild(Int)
  RemoveTemporaryGuild(Int)
}

pub type TerminateResult {
  Terminated
  Gone
  KeptBySession
  TerminateFailed(reason: String)
}

pub type TerminateOutcome {
  TerminateOk
  TerminateRefused(reason: String)
  TerminateMoved(to: Subject(Input(Monitor)))
}

pub type AdoptReply {
  Adopted
  AdoptMoved(to: Subject(Input(Monitor)))
}

pub type Handoff {
  HandoffSpawned(inbox: Subject(Input(Monitor)))
  HandoffRunning(inbox: Subject(Input(Monitor)))
}

pub type Input(monitor) {
  ConnectSession(
    session: SessionRef,
    status: Status,
    afk: Bool,
    mobile: Bool,
    guilds: IdSet,
    targets: IdSet,
    reply: Subject(ConnectReply),
  )
  Connect(request: ConnectRequest(monitor), reply: Subject(ConnectReply))
  RepointSession(session: SessionRef, from: SessionRef)
  Repoint(session: SessionRef, from: SessionRef, monitor: monitor)
  SessionDown(monitor: monitor, ending: SessionEnd)
  Disconnect(session: SessionId, owner: Subject(SessionInbox))
  Update(
    session: SessionId,
    status: Status,
    afk: Bool,
    mobile: Bool,
    custom_status: OpValue,
  )
  SubscribeTargets(users: IdSet)
  UnsubscribeTargets(users: IdSet)
  Foreign(event: PresenceEvent)
  Resend(to: Subscriber)
  ResendNode(node: Atom)
  ResendSince(to: Subscriber, since_ms: Int)
  Joined(to: Subscriber, catchup: Catchup)
  SnapshotRead(token: Int, events: List(PresenceEvent))
  Validated(generation: Int, reply: ValidationReply)
  UserDispatch(id: DispatchId, event: String, data: RawJson, parsed: UserEvent)
  Guild(operation: GuildMembership)
  VisitStart(guild: Int, operator: Bool)
  VisitEnd(guild: Int)
  TerminateSessions(hashes: List(String), reply: Subject(TerminateOutcome))
  TerminateReply(token: Int, session: SessionId, result: TerminateResult)
  TerminateAll
  Rejoin
  ExpiryTick
  GraceExpired(token: Int)
  IdleCheck
  MoveTo(node: Atom)
  MoveRetry(target: Option(Atom))
  MoveFinished(token: Int, moved: Result(Subject(Input(Monitor)), Nil))
  AdoptHandoff(request: StartRequest, reply: Subject(AdoptReply))
  Adopt(
    request: StartRequest,
    monitors: List(monitor),
    reply: Subject(AdoptReply),
  )
  ShedTick
  SubscribeAfterStart(users: IdSet)
}

pub type ManagerMsg {
  StartOrLookup(
    request: StartRequest,
    first: Input(Monitor),
    reply: Subject(Result(Subject(Input(Monitor)), Nil)),
  )
  Lookup(user: Int, reply: Subject(Result(Subject(Input(Monitor)), Nil)))
  StartHandoff(request: StartRequest, reply: Subject(Result(Handoff, Nil)))
  InstallArriving(
    request: StartRequest,
    leaving: Option(Subject(Input(Monitor))),
    reply: Subject(Result(Subject(Input(Monitor)), Nil)),
  )
  PresenceDown(down: process.Down)
  LinkExit(exit: process.ExitMessage)
  ManagerTopology
  ManagerTick(tick: Int)
}

pub fn handoff_exit() -> Dynamic {
  dynamic.array([
    atom.to_dynamic(atom.create("shutdown")),
    atom.to_dynamic(atom.create("handoff")),
  ])
}

pub fn retag(input: Input(a)) -> Result(Input(b), Nil) {
  case input {
    ConnectSession(session:, status:, afk:, mobile:, guilds:, targets:, reply:) ->
      Ok(ConnectSession(
        session:,
        status:,
        afk:,
        mobile:,
        guilds:,
        targets:,
        reply:,
      ))
    Disconnect(session:, owner:) -> Ok(Disconnect(session:, owner:))
    RepointSession(session:, from:) -> Ok(RepointSession(session:, from:))
    Update(session:, status:, afk:, mobile:, custom_status:) ->
      Ok(Update(session:, status:, afk:, mobile:, custom_status:))
    SubscribeTargets(users) -> Ok(SubscribeTargets(users))
    UnsubscribeTargets(users) -> Ok(UnsubscribeTargets(users))
    Foreign(event) -> Ok(Foreign(event))
    Resend(to) -> Ok(Resend(to))
    ResendNode(node) -> Ok(ResendNode(node))
    ResendSince(to:, since_ms:) -> Ok(ResendSince(to:, since_ms:))
    Joined(to:, catchup:) -> Ok(Joined(to:, catchup:))
    UserDispatch(id:, event:, data:, parsed:) ->
      Ok(UserDispatch(id:, event:, data:, parsed:))
    Guild(operation) -> Ok(Guild(operation))
    VisitStart(guild:, operator:) -> Ok(VisitStart(guild:, operator:))
    VisitEnd(guild) -> Ok(VisitEnd(guild))
    TerminateSessions(hashes:, reply:) -> Ok(TerminateSessions(hashes:, reply:))
    TerminateAll -> Ok(TerminateAll)
    Rejoin -> Ok(Rejoin)
    Connect(..)
    | Repoint(..)
    | SessionDown(..)
    | SnapshotRead(..)
    | Validated(..)
    | TerminateReply(..)
    | ExpiryTick
    | GraceExpired(..)
    | IdleCheck
    | MoveTo(..)
    | MoveRetry(..)
    | MoveFinished(..)
    | AdoptHandoff(..)
    | Adopt(..)
    | ShedTick
    | SubscribeAfterStart(..) -> Error(Nil)
  }
}
