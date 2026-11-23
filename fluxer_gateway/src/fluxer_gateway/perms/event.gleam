// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/role.{type Role}
import fluxer_gateway/perms/audience.{type Group}
import fluxer_gateway/perms/cutoff.{type Cutoff}
import fluxer_gateway/perms/native
import fluxer_gateway/perms/role_list
import fluxer_gateway/perms/strip
import gleam/int
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import gleam/set.{type Set}

pub opaque type Event {
  Event(native.PermEvent)
}

pub type Cause {
  Created
  Updated
}

pub type Target {
  WholeGroup(group: Group)
  OneUser(user: UserId)
}

pub type Regroup {
  Regroup(user: UserId, old: Option(Group), new: Option(Group))
}

pub type VisibilityChange {
  VisibilityChange(
    target: Target,
    lost: List(ChannelId),
    gained: List(ChannelId),
  )
}

pub opaque type Effects {
  Effects(parts: List(Part))
}

pub opaque type Part {
  Part(
    regroups: List(Regroup),
    changes: List(VisibilityChange),
    delta: native.PermDelta,
    compaction_due: Bool,
    renumbered: BitArray,
    epoch: Int,
    resync: Resync,
  )
}

pub type Resync {
  NoLists
  AllLists
  Lists(channels: Set(Int))
}

pub type UserChange {
  UserChange(lost: List(ChannelId), gained: List(ChannelId))
}

type Tally {
  Tally(lost: Set(ChannelId), gained: Set(ChannelId))
}

pub fn roles(roles: List(Role)) -> Event {
  Event(native.Roles(strip.of_roles(roles)))
}

pub fn owner(user: Option(UserId)) -> Event {
  Event(native.Owner(option.map(user, id.user_id_to_int)))
}

pub fn channel_upsert(channel: Channel, cause: Cause) -> Event {
  let cause = case cause {
    Created -> native.Created
    Updated -> native.Updated
  }
  Event(native.ChannelUpsert(strip.of_channel(channel), cause))
}

pub fn channel_delete(channel: ChannelId) -> Event {
  Event(native.ChannelDelete(id.channel_id_to_int(channel)))
}

pub fn channel_order(channels: List(ChannelId)) -> Event {
  Event(native.ChannelOrder(list.map(channels, id.channel_id_to_int)))
}

pub fn member_upsert(source: Member) -> Event {
  Event(native.MemberUpsert(
    id.user_id_to_int(member.user_id(source)),
    role_list.of_member(source),
  ))
}

pub fn malformed_member(user: UserId) -> Event {
  Event(native.MemberUpsert(id.user_id_to_int(user), role_list.Malformed))
}

pub fn member_remove(user: UserId) -> Event {
  Event(native.MemberRemove(id.user_id_to_int(user)))
}

pub fn guild_nsfw(nsfw: Option(Bool), level: Int) -> Event {
  Event(native.GuildNsfw(nsfw, level))
}

pub fn history_cutoff(cutoff: Cutoff) -> Event {
  Event(native.HistoryCutoff(cutoff))
}

pub fn virtual_access(user: UserId, view: List(ChannelId)) -> Event {
  Event(native.VirtualAccess(
    id.user_id_to_int(user),
    list.map(view, id.channel_id_to_int),
  ))
}

pub fn changes(effects: Effects) -> List(VisibilityChange) {
  list.flat_map(effects.parts, fn(part) { part.changes })
}

pub fn individual_users(effects: Effects) -> Set(UserId) {
  list.fold(effects.parts, set.new(), fn(acc, part) {
    let acc =
      list.fold(part.regroups, acc, fn(acc, found) {
        set.insert(acc, found.user)
      })
    list.fold(part.changes, acc, fn(acc, found) {
      case found.target {
        OneUser(user) -> set.insert(acc, user)
        WholeGroup(_) -> acc
      }
    })
  })
}

pub fn compaction_due(effects: Effects) -> Bool {
  list.last(effects.parts)
  |> result.map(fn(part) { part.compaction_due })
  |> result.unwrap(False)
}

pub fn for_user(
  effects: Effects,
  user: UserId,
  group: Option(Group),
) -> UserChange {
  let #(_, reached) =
    effects.parts
    |> list.reverse
    |> list.fold(#(group, []), fn(acc, part) {
      let #(group_after, reached) = acc
      let moved = list.find(part.regroups, fn(found) { found.user == user })
      let group_before = case moved {
        Ok(found) -> found.old
        Error(Nil) ->
          option.map(group_after, audience.renumbered(_, part.renumbered))
      }
      let hits =
        list.filter(part.changes, fn(found) {
          case found.target, moved {
            OneUser(target), _ -> target == user
            WholeGroup(target), Error(Nil) -> Some(target) == group_after
            WholeGroup(_), Ok(_) -> False
          }
        })
      #(group_before, [hits, ..reached])
    })
  let tally =
    reached
    |> list.flatten
    |> list.fold(Tally(lost: set.new(), gained: set.new()), fn(tally, found) {
      let tally = list.fold(found.lost, tally, lose)
      list.fold(found.gained, tally, gain)
    })
  UserChange(lost: sorted(tally.lost), gained: sorted(tally.gained))
}

fn lose(tally: Tally, channel: ChannelId) -> Tally {
  case set.contains(tally.gained, channel) {
    True -> Tally(..tally, gained: set.delete(tally.gained, channel))
    False -> Tally(..tally, lost: set.insert(tally.lost, channel))
  }
}

fn gain(tally: Tally, channel: ChannelId) -> Tally {
  case set.contains(tally.lost, channel) {
    True -> Tally(..tally, lost: set.delete(tally.lost, channel))
    False -> Tally(..tally, gained: set.insert(tally.gained, channel))
  }
}

fn sorted(channels: Set(ChannelId)) -> List(ChannelId) {
  channels |> set.to_list |> list.sort(id.compare_channel_id)
}

@internal
pub fn to_native(event: Event) -> native.PermEvent {
  let Event(found) = event
  found
}

@internal
pub fn deltas(effects: Effects) -> List(native.PermDelta) {
  list.map(effects.parts, fn(part) { part.delta })
}

@internal
pub fn epoch(effects: Effects) -> Int {
  list.fold(effects.parts, 0, fn(found, part) { int.max(found, part.epoch) })
}

@internal
pub fn resync(effects: Effects) -> Resync {
  list.fold(effects.parts, NoLists, fn(found, part) {
    merge_resync(found, part.resync)
  })
}

fn merge_resync(first: Resync, next: Resync) -> Resync {
  case first, next {
    AllLists, _ | _, AllLists -> AllLists
    NoLists, other | other, NoLists -> other
    Lists(a), Lists(b) -> Lists(set.union(a, b))
  }
}

@internal
pub fn from_native(
  effects: native.PermEffects,
  events: List(native.PermEvent),
) -> Result(Effects, Nil) {
  use regroups <- result.try(list.try_map(effects.regroups, regroup))
  use changes <- result.try(list.try_map(effects.changes, change))
  Ok(
    Effects([
      Part(
        regroups:,
        changes:,
        delta: effects.delta,
        compaction_due: effects.compaction_due,
        renumbered: effects.renumbered,
        epoch: effects.epoch,
        resync: list_resync(events, effects.changes),
      ),
    ]),
  )
}

fn list_resync(
  events: List(native.PermEvent),
  changes: List(native.VisibilityChange),
) -> Resync {
  list.fold(events, NoLists, fn(found, event) {
    merge_resync(found, case event {
      native.Roles(_)
      | native.ChannelDelete(_)
      | native.ChannelUpsert(_, native.Created) -> AllLists
      native.ChannelUpsert(channel, native.Updated) ->
        Lists(set.insert(changed_channels(changes), channel.id))
      native.Owner(_)
      | native.VirtualAccess(_, _)
      | native.GuildNsfw(_, _)
      | native.HistoryCutoff(_) -> Lists(changed_channels(changes))
      native.ChannelOrder(_)
      | native.MemberUpsert(_, _)
      | native.MemberRemove(_) -> NoLists
    })
  })
}

fn changed_channels(changes: List(native.VisibilityChange)) -> Set(Int) {
  list.fold(changes, set.new(), fn(found, change) {
    found
    |> set.union(set.from_list(change.lost))
    |> set.union(set.from_list(change.gained))
  })
}

@internal
pub fn concat(first: Effects, next: Effects) -> Effects {
  Effects(list.append(first.parts, next.parts))
}

fn regroup(found: native.Regroup) -> Result(Regroup, Nil) {
  use user <- result.try(user_id(found.user))
  Ok(Regroup(
    user:,
    old: option.map(found.old, audience.group_from_native),
    new: option.map(found.new, audience.group_from_native),
  ))
}

fn change(found: native.VisibilityChange) -> Result(VisibilityChange, Nil) {
  use target <- result.try(target(found.target))
  use lost <- result.try(list.try_map(found.lost, channel_id))
  use gained <- result.try(list.try_map(found.gained, channel_id))
  Ok(VisibilityChange(target:, lost:, gained:))
}

fn target(found: native.Target) -> Result(Target, Nil) {
  case found {
    native.GroupTarget(group) ->
      Ok(WholeGroup(audience.group_from_native(group)))
    native.OneUser(user) -> result.map(user_id(user), OneUser)
  }
}

fn user_id(value: Int) -> Result(UserId, Nil) {
  id.user_id(value) |> result.replace_error(Nil)
}

fn channel_id(value: Int) -> Result(ChannelId, Nil) {
  id.channel_id(value) |> result.replace_error(Nil)
}
