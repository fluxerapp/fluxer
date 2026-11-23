// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type GuildId, type UserId}
import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionId, type SessionRef}
import fluxer_gateway/member_list/config.{type Config} as ml_config
import fluxer_gateway/member_list/effects.{type Dispatch, type Timer}
import fluxer_gateway/member_list/event.{type Event}
import fluxer_gateway/member_list/native
import fluxer_gateway/member_list/ranges as ml_ranges
import fluxer_gateway/member_list/sessions.{type SessionTable}
import fluxer_gateway/member_list/snapshot.{type Snapshot} as ml_snapshot
import fluxer_gateway/perms.{type Delta, type Perms}
import fluxer_gateway/perms/event as perm_event
import fluxer_gateway/perms/native as perms_native
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}

pub opaque type MemberList {
  MemberList(
    handle: native.MlHandle,
    sessions: SessionTable,
    guild: GuildId,
    tuning: Config,
    owner: Pid,
  )
}

pub opaque type CountsHandle {
  CountsHandle(native.MlCounts)
}

pub type MemberListError {
  ListBroken
  ListBadInput(reason: String)
}

pub type Lookup =
  fn(List(UserId)) -> List(#(UserId, BitArray, BitArray))

const max_events = 64

pub fn new(
  guild: GuildId,
  tuning: Config,
  owner: Pid,
  perms: Perms,
) -> #(MemberList, CountsHandle) {
  let #(handle, counts) = create(guild, tuning, owner, perms)
  let created =
    MemberList(handle:, sessions: sessions.new(), guild:, tuning:, owner:)
  #(created, CountsHandle(counts))
}

fn create(
  guild: GuildId,
  tuning: Config,
  owner: Pid,
  model: Perms,
) -> #(native.MlHandle, native.MlCounts) {
  native.ml_new(
    id.guild_id_to_int(guild),
    ml_config.to_native(tuning),
    owner,
    perms.native_handle(model),
  )
}

pub fn load(
  members: MemberList,
  source: Snapshot,
) -> Result(Nil, MemberListError) {
  let loaded = ml_snapshot.to_native(source)
  either(fn() { native.ml_load(members.handle, loaded) }, fn() {
    native.ml_load_dirty(members.handle, loaded)
  })
  |> result.map_error(from_native_error)
}

pub fn apply_timed(
  members: MemberList,
  events: List(Event),
  delta: Option(Delta),
  lookup: Lookup,
) -> Result(#(List(Dispatch), Timer), MemberListError) {
  let found = list.map(events, event.to_native)
  case delta {
    None -> {
      sessions.forget_synced(members.sessions)
      run(members, list.append(found, reenabled(found, [])), [], lookup)
    }
    Some(delta) -> {
      let epoch = perms.delta_epoch(delta)
      let synced = sessions.synced_at(members.sessions, epoch)
      let resynced = resync_events(members, delta)
      let retried = case resynced != [] || perms.delta_regained(delta) {
        True -> sessions.take_parked(members.sessions)
        False -> []
      }
      let wanted =
        found
        |> list.filter(fn(found) {
          !already_synced(found, synced) && !covered(found, resynced)
        })
        |> list.append(resynced)
        |> list.append(reenabled(found, resynced))
        |> list.append(retried)
      let ran = run(members, wanted, perms.native_deltas(delta), lookup)
      park_gated(members, retried)
      use #(dispatches, timer) <- result.map(ran)
      let keys = case list.all(wanted, is_view_gained) {
        True -> synced
        False -> set.new()
      }
      sessions.record_synced(
        members.sessions,
        epoch,
        list.fold(dispatches, keys, fn(keys, dispatch) {
          list.fold(dispatch.to, keys, fn(keys, ref) {
            set.insert(keys, event.session_key(ref.id))
          })
        }),
      )
      #(dispatches, timer)
    }
  }
}

fn park_gated(members: MemberList, events: List(native.MlEvent)) -> Nil {
  list.each(events, fn(found) {
    case found {
      native.Subscribe(session:, channel:, ..) ->
        case query(members, native.SessionRanges(session)) {
          Ok(native.RangesReply(Some(#(current, [_, ..]))))
            if current == channel
          -> Nil
          _ -> sessions.park(members.sessions, session, found)
        }
      _ -> Nil
    }
  })
}

fn already_synced(found: native.MlEvent, synced: Set(BitArray)) -> Bool {
  case found {
    native.ViewGained(session:, ..) -> set.contains(synced, session)
    _ -> False
  }
}

fn reenabled(
  events: List(native.MlEvent),
  resynced: List(native.MlEvent),
) -> List(native.MlEvent) {
  let all = native.Permissions(native.AllLists)
  case
    list.contains(events, native.SetUpdates(native.UpdatesOn))
    && !list.contains(resynced, all)
  {
    True -> [all]
    False -> []
  }
}

fn covered(found: native.MlEvent, resynced: List(native.MlEvent)) -> Bool {
  case found {
    native.ViewGained(channel:, ..) ->
      list.any(resynced, fn(resync) {
        resync == native.Permissions(native.AllLists)
        || resync == native.Permissions(native.ChannelList(channel))
      })
    _ -> False
  }
}

fn is_view_gained(found: native.MlEvent) -> Bool {
  case found {
    native.ViewGained(..) -> True
    _ -> False
  }
}

fn resync_events(members: MemberList, delta: Delta) -> List(native.MlEvent) {
  let wanted = case perms.delta_resync(delta) {
    perm_event.NoLists -> []
    perm_event.AllLists -> [native.Permissions(native.AllLists)]
    perm_event.Lists(channels) ->
      channels
      |> set.to_list
      |> list.sort(int.compare)
      |> list.map(fn(channel) {
        native.Permissions(native.ChannelList(channel))
      })
  }
  case
    wanted != []
    && sessions.claim_resync(members.sessions, perms.delta_epoch(delta))
  {
    True -> wanted
    False -> []
  }
}

pub fn subscribe(
  members: MemberList,
  subscriber: SessionRef,
  channel: ChannelId,
  requested: List(#(Int, Int)),
  lookup: Lookup,
) -> Result(List(Dispatch), MemberListError) {
  let key = event.session_key(subscriber.id)
  let channel_int = id.channel_id_to_int(channel)
  let wanted = ml_ranges.normalize(ml_ranges.parse_request(requested))
  let old = case
    either(
      fn() { native.ml_query(members.handle, native.SessionRanges(key)) },
      fn() { native.ml_query_dirty(members.handle, native.SessionRanges(key)) },
    )
  {
    Ok(native.RangesReply(Some(#(found, items)))) if found == channel_int ->
      ml_ranges.from_list(items)
    _ -> ml_ranges.empty()
  }
  let sync = ml_ranges.to_sync(new: wanted, old:)
  let token = sessions.subscriber(members.sessions, subscriber.id)
  let subscribe =
    native.Subscribe(
      session: key,
      subscriber: token,
      user: id.user_id_to_int(subscriber.user),
      channel: channel_int,
      ranges: ml_ranges.to_list(wanted),
      sync: ml_ranges.to_list(sync),
    )
  sessions.forget_synced(members.sessions)
  sessions.unpark(members.sessions, key)
  use #(found, _) <- result.map(apply(members, [subscribe], [], lookup))
  sessions.put(members.sessions, token, subscriber)
  to_dispatches(members.sessions, found)
}

pub fn rebind(members: MemberList, subscriber: SessionRef) -> Nil {
  sessions.rebind(members.sessions, subscriber)
}

pub fn subscribed_channel(
  members: MemberList,
  session: SessionId,
) -> Option(ChannelId) {
  case query(members, native.SessionRanges(event.session_key(session))) {
    Ok(native.RangesReply(Some(#(found, [_, ..])))) ->
      id.channel_id(found) |> option.from_result
    _ -> None
  }
}

pub fn unsubscribe(
  members: MemberList,
  session: SessionId,
) -> Result(List(Dispatch), MemberListError) {
  let dropped =
    run(members, [native.DropSession(event.session_key(session))], [], fn(_) {
      []
    })
  sessions.remove(members.sessions, session)
  sessions.forget_synced(members.sessions)
  sessions.unpark(members.sessions, event.session_key(session))
  result.map(dropped, pair.first)
}

pub fn channel_counts(
  members: MemberList,
  channels: List(ChannelId),
) -> Result(List(#(ChannelId, Int, Int)), MemberListError) {
  let wanted = native.ChannelCounts(list.map(channels, id.channel_id_to_int))
  use reply <- result.try(query(members, wanted))
  case reply {
    native.CountsReply(counts) ->
      list.try_map(counts, fn(found) {
        let #(channel, total, online) = found
        id.channel_id(channel)
        |> result.map(fn(channel) { #(channel, total, online) })
        |> result.replace_error(ListBroken)
      })
    _ -> Error(ListBroken)
  }
}

pub fn counts(handle: CountsHandle) -> #(Int, Int) {
  let CountsHandle(counts) = handle
  native.ml_counts(counts)
}

pub fn page_ids(
  members: MemberList,
  offset: Int,
  limit: Int,
) -> Result(PackedIds, MemberListError) {
  use reply <- result.try(query(members, native.PageIds(offset, limit)))
  case reply {
    native.IdsReply(ids) -> Ok(ids)
    _ -> Error(ListBroken)
  }
}

pub fn native_handle(members: MemberList) -> native.MlHandle {
  members.handle
}

pub fn release(members: MemberList) -> Nil {
  native.ml_release(members.handle)
  sessions.delete(members.sessions)
}

pub fn rebuild(
  members: MemberList,
  perms: Perms,
  source: Snapshot,
  lookup: Lookup,
) -> Result(#(MemberList, CountsHandle, List(Dispatch)), MemberListError) {
  let kept = case query(members, native.Subscriptions) {
    Ok(native.SubscriptionsReply(found)) -> found
    _ -> []
  }
  let #(handle, counts) =
    create(members.guild, members.tuning, members.owner, perms)
  let fresh = MemberList(..members, handle:)
  sessions.forget_resyncs(members.sessions)
  sessions.forget_synced(members.sessions)
  let replayed = {
    use _ <- result.try(load(fresh, source))
    let events =
      list.map(kept, fn(found) {
        native.Subscribe(
          session: found.session,
          subscriber: found.subscriber,
          user: found.user,
          channel: found.channel,
          ranges: found.ranges,
          sync: found.ranges,
        )
      })
      |> list.append(sessions.take_parked(members.sessions))
    let ran = run(fresh, events, [], lookup)
    park_gated(fresh, events)
    ran
  }
  case replayed {
    Ok(#(dispatches, _)) -> {
      native.ml_release(members.handle)
      Ok(#(fresh, CountsHandle(counts), dispatches))
    }
    Error(error) -> {
      native.ml_release(handle)
      Error(error)
    }
  }
}

fn query(
  members: MemberList,
  wanted: native.MlQuery,
) -> Result(native.MlReply, MemberListError) {
  either(fn() { native.ml_query(members.handle, wanted) }, fn() {
    native.ml_query_dirty(members.handle, wanted)
  })
  |> result.map_error(from_native_error)
}

fn run(
  members: MemberList,
  events: List(native.MlEvent),
  deltas: List(perms_native.PermDelta),
  lookup: Lookup,
) -> Result(#(List(Dispatch), Timer), MemberListError) {
  use #(found, timer) <- result.map(apply(members, events, deltas, lookup))
  #(to_dispatches(members.sessions, found), timer)
}

fn apply(
  members: MemberList,
  events: List(native.MlEvent),
  deltas: List(perms_native.PermDelta),
  lookup: Lookup,
) -> Result(#(List(native.MlDispatch), Timer), MemberListError) {
  calls(list.sized_chunk(events, max_events), deltas)
  |> list.try_fold(#([], effects.KeepTimer), fn(acc, call) {
    let #(chunk, delta) = call
    use step <- result.try(
      either(fn() { native.ml_apply(members.handle, chunk, delta) }, fn() {
        native.ml_apply_dirty(members.handle, chunk, delta)
      }),
    )
    use #(dispatches, timer) <- result.try(
      drive(members.handle, lookup, step, []),
    )
    Ok(#([dispatches, ..acc.0], effects.later(acc.1, timer_of(timer))))
  })
  |> result.map(fn(found) { #(list.flatten(list.reverse(found.0)), found.1) })
  |> result.map_error(from_native_error)
}

fn calls(
  chunks: List(List(native.MlEvent)),
  deltas: List(perms_native.PermDelta),
) -> List(#(List(native.MlEvent), Option(perms_native.PermDelta))) {
  let plain = fn(chunk) { #(chunk, None) }
  case list.reverse(deltas) {
    [] -> list.map(chunks, plain)
    [last, ..earlier] -> {
      let leading =
        earlier
        |> list.reverse
        |> list.map(fn(delta) { #([], Some(delta)) })
      let tail = case chunks {
        [] -> [#([], Some(last))]
        [first, ..rest] -> [#(first, Some(last)), ..list.map(rest, plain)]
      }
      list.append(leading, tail)
    }
  }
}

fn drive(
  handle: native.MlHandle,
  lookup: Lookup,
  step: native.MlStep,
  acc: List(List(native.MlDispatch)),
) -> Result(
  #(List(native.MlDispatch), native.TimerAction),
  perms_native.NifError,
) {
  case step {
    native.Done(found) ->
      Ok(#(list.flatten(list.reverse([found.dispatches, ..acc])), found.timer))
    native.PageDone(_) -> Error(perms_native.Broken)
    native.Partial(found) -> {
      use next <- result.try(native.ml_resume(handle) |> no_dirty)
      drive(handle, lookup, next, [found.dispatches, ..acc])
    }
    native.NeedItems(users) -> {
      use asked <- result.try(
        list.try_map(users, id.user_id)
        |> result.replace_error(perms_native.Broken),
      )
      let items =
        lookup(asked)
        |> list.map(fn(found) {
          let #(user, list_bytes, page_bytes) = found
          native.EncodedItem(id.user_id_to_int(user), list_bytes, page_bytes)
        })
      use next <- result.try(native.ml_fill(handle, items) |> no_dirty)
      drive(handle, lookup, next, acc)
    }
  }
}

fn to_dispatches(
  table: SessionTable,
  found: List(native.MlDispatch),
) -> List(Dispatch) {
  list.filter_map(found, to_dispatch(table, _))
}

fn to_dispatch(
  table: SessionTable,
  found: native.MlDispatch,
) -> Result(Dispatch, Nil) {
  let subscribers = case found.target {
    native.ToSessions(subscribers) -> subscribers
    native.ToSession(subscriber) -> [subscriber]
  }
  case sessions.refs(table, subscribers) {
    [] -> Error(Nil)
    refs ->
      Ok(effects.Dispatch(
        to: refs,
        event: effects.event_name,
        payload: raw_json.wrap(found.payload),
      ))
  }
}

fn timer_of(timer: native.TimerAction) -> Timer {
  case timer {
    native.KeepTimer -> effects.KeepTimer
    native.ArmTimer -> effects.ArmTimer
    native.CancelTimer -> effects.CancelTimer
  }
}

fn either(
  normal_call: fn() -> Result(a, perms_native.NifError),
  dirty_call: fn() -> Result(a, perms_native.NifError),
) -> Result(a, perms_native.NifError) {
  case normal_call() {
    Error(perms_native.NeedsDirty) -> dirty_call()
    other -> other
  }
}

fn no_dirty(
  found: Result(a, perms_native.NifError),
) -> Result(a, perms_native.NifError) {
  case found {
    Error(perms_native.NeedsDirty) -> Error(perms_native.Broken)
    other -> other
  }
}

fn from_native_error(error: perms_native.NifError) -> MemberListError {
  case error {
    perms_native.BadInput(reason) -> ListBadInput(reason)
    perms_native.NeedsDirty | perms_native.Broken | perms_native.Gone ->
      ListBroken
  }
}
