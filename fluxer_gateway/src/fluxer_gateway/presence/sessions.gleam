// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{type Status, Offline}
import fluxer_gateway/core/session.{
  type SessionId, type SessionInbox, type SessionRef,
}
import fluxer_gateway/presence/display.{type Display, SessionState}
import fluxer_gateway/presence/status as status_rules
import gleam/bytes_tree.{type BytesTree}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/order
import gleam/result

type Liveness {
  Live
  Lost
  HandedOff
}

pub opaque type SessionEntry(monitor) {
  SessionEntry(
    session: SessionRef,
    status: Status,
    afk: Bool,
    mobile: Bool,
    monitor: monitor,
    liveness: Liveness,
    id_wire: BitArray,
  )
}

pub opaque type Sessions(monitor) {
  Sessions(entries: List(SessionEntry(monitor)))
}

pub type Upsert(monitor) {
  Added
  Refreshed(old_monitor: monitor)
  Unchanged
}

pub fn entry(
  session session: SessionRef,
  status status: Status,
  afk afk: Bool,
  mobile mobile: Bool,
  monitor monitor: monitor,
) -> SessionEntry(monitor) {
  SessionEntry(
    session:,
    status:,
    afk:,
    mobile:,
    monitor:,
    liveness: Live,
    id_wire: <<
      "\"session_id\":\"":utf8,
      session.session_id_bits(session.id):bits,
      "\",\"status\":\"":utf8,
    >>,
  )
}

pub fn entry_session(entry: SessionEntry(monitor)) -> SessionRef {
  entry.session
}

pub fn entry_id(entry: SessionEntry(monitor)) -> SessionId {
  entry.session.id
}

pub fn entry_monitor(entry: SessionEntry(monitor)) -> monitor {
  entry.monitor
}

pub fn entry_status(entry: SessionEntry(monitor)) -> Status {
  entry.status
}

pub fn entry_afk(entry: SessionEntry(monitor)) -> Bool {
  entry.afk
}

pub fn entry_mobile(entry: SessionEntry(monitor)) -> Bool {
  entry.mobile
}

pub fn empty() -> Sessions(monitor) {
  Sessions([])
}

fn find(
  sessions: Sessions(monitor),
  id: SessionId,
) -> Result(SessionEntry(monitor), Nil) {
  list.find(sessions.entries, fn(e) { e.session.id == id })
}

pub fn upsert(
  sessions: Sessions(monitor),
  new: SessionEntry(monitor),
) -> #(Sessions(monitor), Upsert(monitor)) {
  case find(sessions, new.session.id) {
    Error(Nil) -> #(Sessions(list.append(sessions.entries, [new])), Added)
    Ok(old) ->
      case old.liveness, old.session.inbox == new.session.inbox {
        Live, True -> #(
          replace(sessions, SessionEntry(..new, monitor: old.monitor)),
          Unchanged,
        )
        Lost, _ | HandedOff, _ | Live, False -> #(
          replace(sessions, new),
          Refreshed(old_monitor: old.monitor),
        )
      }
  }
}

pub fn upsert_carried(
  sessions: Sessions(monitor),
  carried: SessionEntry(monitor),
) -> #(Sessions(monitor), Upsert(monitor)) {
  case find(sessions, carried.session.id) {
    Ok(SessionEntry(liveness: Live, ..)) -> #(sessions, Unchanged)
    Ok(SessionEntry(liveness: Lost, ..))
    | Ok(SessionEntry(liveness: HandedOff, ..))
    | Error(Nil) -> upsert(sessions, carried)
  }
}

fn replace(
  sessions: Sessions(monitor),
  new: SessionEntry(monitor),
) -> Sessions(monitor) {
  Sessions(
    list.map(sessions.entries, fn(e) {
      case e.session.id == new.session.id {
        True -> new
        False -> e
      }
    }),
  )
}

pub fn set_state(
  sessions: Sessions(monitor),
  id: SessionId,
  status: Status,
  afk: Bool,
  mobile: Bool,
) -> Result(#(Sessions(monitor), Bool), Nil) {
  use old <- result.map(find(sessions, id))
  case old.status == status && old.afk == afk && old.mobile == mobile {
    True -> #(sessions, False)
    False -> #(
      replace(sessions, SessionEntry(..old, status:, afk:, mobile:)),
      True,
    )
  }
}

pub fn repoint(
  sessions: Sessions(monitor),
  to: SessionRef,
  from: Subject(SessionInbox),
  monitor: monitor,
) -> Result(#(Sessions(monitor), monitor), Nil) {
  use old <- result.map(
    list.find(sessions.entries, fn(e) {
      e.session.id == to.id && e.session.inbox == from && e.liveness == Live
    }),
  )
  #(replace(sessions, SessionEntry(..old, session: to, monitor:)), old.monitor)
}

pub fn find_by_monitor(
  sessions: Sessions(monitor),
  monitor: monitor,
) -> Result(SessionEntry(monitor), Nil) {
  list.find(sessions.entries, fn(e) { e.monitor == monitor })
}

pub fn remove_by_monitor(
  sessions: Sessions(monitor),
  monitor: monitor,
) -> Result(#(Sessions(monitor), SessionEntry(monitor)), Nil) {
  use found <- result.map(find_by_monitor(sessions, monitor))
  #(
    Sessions(list.filter(sessions.entries, fn(e) { e.monitor != monitor })),
    found,
  )
}

pub fn mark_lost(
  sessions: Sessions(monitor),
  monitor: monitor,
  handed_off handed_off: Bool,
) -> Sessions(monitor) {
  let liveness = case handed_off {
    True -> HandedOff
    False -> Lost
  }
  Sessions(
    list.map(sessions.entries, fn(e) {
      case e.monitor == monitor {
        True -> SessionEntry(..e, liveness:)
        False -> e
      }
    }),
  )
}

pub fn drop_lost(
  sessions: Sessions(monitor),
) -> #(Sessions(monitor), List(SessionEntry(monitor))) {
  let #(lost, kept) =
    list.partition(sessions.entries, fn(e) { e.liveness != Live })
  #(Sessions(kept), lost)
}

pub fn drop_unreachable(
  sessions: Sessions(monitor),
) -> #(Sessions(monitor), List(SessionEntry(monitor))) {
  let #(lost, kept) =
    list.partition(sessions.entries, fn(e) { e.liveness == Lost })
  #(Sessions(kept), lost)
}

pub fn has_lost(sessions: Sessions(monitor)) -> Bool {
  list.any(sessions.entries, fn(e) { e.liveness != Live })
}

pub fn has_live(sessions: Sessions(monitor)) -> Bool {
  list.any(sessions.entries, fn(e) { e.liveness == Live })
}

pub fn remove_owned(
  sessions: Sessions(monitor),
  id: SessionId,
  owner: Subject(SessionInbox),
) -> #(Sessions(monitor), List(SessionEntry(monitor))) {
  let #(gone, kept) =
    list.partition(sessions.entries, fn(e) {
      e.session.id == id && e.session.inbox == owner
    })
  #(Sessions(kept), gone)
}

pub fn remove_by_ids(
  sessions: Sessions(monitor),
  ids: List(SessionId),
) -> #(Sessions(monitor), List(SessionEntry(monitor))) {
  let #(gone, kept) =
    list.partition(sessions.entries, fn(e) { list.contains(ids, e.session.id) })
  #(Sessions(kept), gone)
}

pub fn is_empty(sessions: Sessions(monitor)) -> Bool {
  sessions.entries == []
}

pub fn entries(sessions: Sessions(monitor)) -> List(SessionEntry(monitor)) {
  sessions.entries
}

pub fn live_refs(sessions: Sessions(monitor)) -> List(SessionRef) {
  live_sessions(sessions.entries)
}

fn live_sessions(entries: List(SessionEntry(monitor))) -> List(SessionRef) {
  case entries {
    [] -> []
    [entry, ..rest] if entry.liveness == Live -> [
      entry.session,
      ..live_sessions(rest)
    ]
    [_, ..rest] -> live_sessions(rest)
  }
}

fn state_of(entry: SessionEntry(monitor)) -> display.SessionState {
  SessionState(status: entry.status, afk: entry.afk, mobile: entry.mobile)
}

pub fn flatten(
  sessions: Sessions(monitor),
  invisible invisible: Bool,
) -> Display {
  list.fold(sessions.entries, display.flat(), fn(acc, e) {
    display.add(acc, state_of(e), invisible)
  })
  |> display.finish
}

const sorted_inline_limit = 8

@external(erlang, "gleam_stdlib", "identity")
fn parts_tree(parts: List(BitArray)) -> BytesTree

fn sort_ascending(
  entries: List(SessionEntry(monitor)),
) -> List(SessionEntry(monitor)) {
  case list.length(entries) > sorted_inline_limit {
    True -> list.sort(entries, ascending)
    False -> list.fold(entries, [], insert_ascending)
  }
}

fn insert_ascending(
  sorted: List(SessionEntry(monitor)),
  entry: SessionEntry(monitor),
) -> List(SessionEntry(monitor)) {
  case sorted {
    [] -> [entry]
    [first, ..rest] ->
      case session.session_id_before(entry.session.id, first.session.id) {
        True -> [entry, ..sorted]
        False -> [first, ..insert_ascending(rest, entry)]
      }
  }
}

fn ascending(
  a: SessionEntry(monitor),
  b: SessionEntry(monitor),
) -> order.Order {
  session.compare_session_id(a.session.id, b.session.id)
}

pub fn replace_wire(
  sessions: Sessions(monitor),
  invisible invisible: Bool,
) -> BytesTree {
  let Rows(best:, online_mobile:, all_afk:, parts:) =
    replace_rows(
      sort_ascending(sessions.entries),
      invisible,
      Rows(best: Offline, online_mobile: False, all_afk: True, parts: [
        <<"]":utf8>>,
      ]),
    )
  let mobile = best == presence.Online && online_mobile
  let afk = !mobile && all_afk && sessions.entries != []
  parts_tree([
    <<"[":utf8>>,
    flags_wire(afk, mobile),
    <<"\"session_id\":\"all\",\"status\":\"":utf8>>,
    status_tail(best),
    ..parts
  ])
}

type Rows {
  Rows(best: Status, online_mobile: Bool, all_afk: Bool, parts: List(BitArray))
}

fn replace_rows(
  entries: List(SessionEntry(monitor)),
  invisible: Bool,
  rows: Rows,
) -> Rows {
  case entries {
    [] -> rows
    [e, ..rest] -> {
      let status = case invisible, e.status {
        True, Offline -> Offline
        True, _ -> presence.Invisible
        False, found -> found
      }
      replace_rows(
        rest,
        invisible,
        Rows(
          best: status_rules.stronger(status, rows.best),
          online_mobile: rows.online_mobile
            || { e.mobile && status == presence.Online },
          all_afk: rows.all_afk && e.afk,
          parts: [
            <<",":utf8>>,
            flags_wire(e.afk, e.mobile),
            e.id_wire,
            status_tail(status),
            ..rows.parts
          ],
        ),
      )
    }
  }
}

fn flags_wire(afk: Bool, mobile: Bool) -> BitArray {
  case afk, mobile {
    True, True -> <<"{\"afk\":true,\"mobile\":true,":utf8>>
    True, False -> <<"{\"afk\":true,\"mobile\":false,":utf8>>
    False, True -> <<"{\"afk\":false,\"mobile\":true,":utf8>>
    False, False -> <<"{\"afk\":false,\"mobile\":false,":utf8>>
  }
}

fn status_tail(value: Status) -> BitArray {
  case value {
    presence.Online -> <<"online\"}":utf8>>
    presence.Idle -> <<"idle\"}":utf8>>
    presence.Dnd -> <<"dnd\"}":utf8>>
    presence.Invisible -> <<"invisible\"}":utf8>>
    Offline -> <<"offline\"}":utf8>>
  }
}

pub fn holds_push(sessions: Sessions(monitor)) -> Bool {
  list.any(sessions.entries, fn(e) { !e.afk && e.status != Offline })
}
