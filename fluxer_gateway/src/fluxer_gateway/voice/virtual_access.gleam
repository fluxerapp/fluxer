// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId, type UserId}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}

pub opaque type VirtualAccess {
  VirtualAccess(
    granted: Dict(UserId, Set(ChannelId)),
    view_only: Dict(UserId, Set(ChannelId)),
    pending_join: Dict(UserId, Set(ChannelId)),
    preserve: Dict(UserId, Set(ChannelId)),
    move_pending: Dict(UserId, Set(ChannelId)),
  )
}

pub fn new() -> VirtualAccess {
  VirtualAccess(dict.new(), dict.new(), dict.new(), dict.new(), dict.new())
}

fn member(
  map: Dict(UserId, Set(ChannelId)),
  user: UserId,
  channel: ChannelId,
) -> Bool {
  case dict.get(map, user) {
    Ok(channels) -> set.contains(channels, channel)
    Error(Nil) -> False
  }
}

fn add(
  map: Dict(UserId, Set(ChannelId)),
  user: UserId,
  channel: ChannelId,
) -> Dict(UserId, Set(ChannelId)) {
  dict.upsert(map, user, fn(existing) {
    existing |> option.unwrap(set.new()) |> set.insert(channel)
  })
}

fn clear(
  map: Dict(UserId, Set(ChannelId)),
  user: UserId,
  channel: ChannelId,
) -> Dict(UserId, Set(ChannelId)) {
  case dict.get(map, user) {
    Ok(channels) -> {
      let remaining = set.delete(channels, channel)
      case set.is_empty(remaining) {
        True -> dict.delete(map, user)
        False -> dict.insert(map, user, remaining)
      }
    }
    Error(Nil) -> map
  }
}

pub fn has_access(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  member(access.granted, user, channel)
}

pub fn has_voice_access(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  has_access(access, user, channel) && !member(access.view_only, user, channel)
}

pub fn is_pending_join(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  member(access.pending_join, user, channel)
}

fn has_preserve(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  member(access.preserve, user, channel)
}

pub fn is_move_pending(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  member(access.move_pending, user, channel)
}

pub fn is_held(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> Bool {
  is_pending_join(access, user, channel)
  || has_preserve(access, user, channel)
  || is_move_pending(access, user, channel)
}

pub fn channels(access: VirtualAccess, user: UserId) -> List(ChannelId) {
  dict.get(access.granted, user)
  |> result.map(set.to_list)
  |> result.unwrap([])
}

pub fn grant(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> VirtualAccess {
  VirtualAccess(
    ..access,
    granted: add(access.granted, user, channel),
    view_only: clear(access.view_only, user, channel),
    pending_join: add(access.pending_join, user, channel),
  )
}

pub fn grant_view_only(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> VirtualAccess {
  VirtualAccess(
    ..access,
    granted: add(access.granted, user, channel),
    view_only: add(access.view_only, user, channel),
    pending_join: clear(access.pending_join, user, channel),
  )
}

pub fn all(access: VirtualAccess) -> List(#(UserId, List(ChannelId))) {
  dict.to_list(access.granted)
  |> list.map(fn(pair) { #(pair.0, set.to_list(pair.1)) })
}

pub fn granted_changes(
  before: VirtualAccess,
  after: VirtualAccess,
) -> List(#(UserId, List(ChannelId))) {
  case before.granted == after.granted {
    True -> []
    False ->
      dict.keys(before.granted)
      |> list.append(dict.keys(after.granted))
      |> list.unique
      |> list.filter(fn(user) {
        dict.get(before.granted, user) != dict.get(after.granted, user)
      })
      |> list.map(fn(user) { #(user, channels(after, user)) })
  }
}

pub fn revoke(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> VirtualAccess {
  case dict.get(access.granted, user) {
    Error(Nil) -> access
    Ok(channels) -> {
      let remaining = set.delete(channels, channel)
      case set.is_empty(remaining) {
        True -> remove_user(access, user)
        False ->
          VirtualAccess(
            granted: dict.insert(access.granted, user, remaining),
            view_only: clear(access.view_only, user, channel),
            pending_join: clear(access.pending_join, user, channel),
            preserve: clear(access.preserve, user, channel),
            move_pending: clear(access.move_pending, user, channel),
          )
      }
    }
  }
}

fn remove_user(access: VirtualAccess, user: UserId) -> VirtualAccess {
  VirtualAccess(
    granted: dict.delete(access.granted, user),
    view_only: dict.delete(access.view_only, user),
    pending_join: dict.delete(access.pending_join, user),
    preserve: dict.delete(access.preserve, user),
    move_pending: dict.delete(access.move_pending, user),
  )
}

pub fn mark_transition(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> VirtualAccess {
  VirtualAccess(
    ..access,
    pending_join: add(access.pending_join, user, channel),
    preserve: add(access.preserve, user, channel),
    move_pending: add(access.move_pending, user, channel),
  )
}

pub fn clear_transition(
  access: VirtualAccess,
  user: UserId,
  channel: ChannelId,
) -> VirtualAccess {
  VirtualAccess(
    ..access,
    pending_join: clear(access.pending_join, user, channel),
    preserve: clear(access.preserve, user, channel),
    move_pending: clear(access.move_pending, user, channel),
  )
}

pub fn retain_transitions(
  access: VirtualAccess,
  keep: fn(UserId, ChannelId) -> Bool,
) -> VirtualAccess {
  let retain = fn(map: Dict(UserId, Set(ChannelId))) {
    map
    |> dict.map_values(fn(user, channels) {
      set.filter(channels, fn(channel) { keep(user, channel) })
    })
    |> dict.filter(fn(_user, channels) { !set.is_empty(channels) })
  }
  VirtualAccess(
    ..access,
    pending_join: retain(access.pending_join),
    preserve: retain(access.preserve),
    move_pending: retain(access.move_pending),
  )
}

pub fn granted_users(access: VirtualAccess) -> List(UserId) {
  dict.keys(access.granted)
}

pub fn clear_transitions(
  access: VirtualAccess,
  pairs: List(#(UserId, ChannelId)),
) -> VirtualAccess {
  list.fold(pairs, access, fn(acc, pair) {
    clear_transition(acc, pair.0, pair.1)
  })
}
