// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids.{type PackedIds}
import fluxer_gateway/member_list
import fluxer_gateway/member_list/native
import fluxer_gateway/perms/native as perms_native
import fluxer_gateway/push/metrics
import fluxer_gateway/push/rule
import fluxer_gateway/push/settings.{type Settings}
import gleam/list
import gleam/option.{type Option, Some}

pub type Class {
  ClassAllMessages
  ClassUncertain
  ClassMuted
  ClassMentionsOnly
}

pub type Fallback {
  FallbackStale
  FallbackBroken
  FallbackBusy
  FallbackError
}

const max_updates_per_call = 4096

pub opaque type Key {
  Key(handle: native.MlHandle, default_level: Int, large: Bool)
}

pub fn key(
  members: member_list.MemberList,
  default_level default_level: Int,
  large large: Bool,
) -> Key {
  Key(handle: member_list.native_handle(members), default_level:, large:)
}

pub fn refresh(
  members: member_list.MemberList,
  current: Option(Key),
  wanted: Key,
  rows: fn() -> List(native.MaskUpdate),
) -> Option(Key) {
  case current {
    Some(found) if found == wanted -> current
    _ ->
      case update(members, rows()) {
        True -> Some(wanted)
        False -> current
      }
  }
}

pub fn update(
  members: member_list.MemberList,
  updates: List(native.MaskUpdate),
) -> Bool {
  let handle = member_list.native_handle(members)
  updates
  |> list.sized_chunk(max_updates_per_call)
  |> list.fold(True, fn(ok, slice) {
    case native.push_masks_set(handle, slice) {
      Ok(Nil) -> ok
      Error(_) -> {
        metrics.count("push_masks_update_failures", 1)
        False
      }
    }
  })
}

pub fn supersets(
  members: member_list.MemberList,
  epoch: Int,
) -> Result(#(PackedIds, PackedIds), Fallback) {
  case
    native.push_masks_query(
      member_list.native_handle(members),
      native.Supersets(epoch),
    )
  {
    Ok(native.Masks(immediate:, held:)) -> Ok(#(immediate, held))
    Ok(native.Stale) -> fall_back(FallbackStale)
    Error(perms_native.Broken) | Error(perms_native.Gone) ->
      fall_back(FallbackBroken)
    Error(perms_native.NeedsDirty) -> fall_back(FallbackBusy)
    Error(perms_native.BadInput(_)) -> fall_back(FallbackError)
  }
}

fn fall_back(why: Fallback) -> Result(a, Fallback) {
  let label = case why {
    FallbackStale -> "stale"
    FallbackBroken -> "broken"
    FallbackBusy -> "busy"
    FallbackError -> "error"
  }
  metrics.count_labelled("push_masks_fallback_total", [#("why", label)], 1)
  Error(why)
}

pub fn class_of(found: Settings, canonical: rule.Shape) -> Class {
  case settings.has_overrides(found) || settings.has_timed_mute(found) {
    True -> ClassUncertain
    False ->
      case rule.profile_verdict(canonical, found) {
        rule.Never(rule.MobilePushOff)
        | rule.Never(rule.GuildMuted)
        | rule.Never(rule.LevelNone) -> ClassMuted
        rule.IfMentioned(otherwise: rule.Eligible(rule.LevelAll), ..) ->
          ClassAllMessages
        rule.IfMentioned(otherwise: rule.Ineligible(rule.NotMentioned), ..) ->
          ClassMentionsOnly
        _ -> ClassUncertain
      }
  }
}

pub fn class_update(user: Int, class: Class) -> native.MaskUpdate {
  let #(all, uncertain, muted) = case class {
    ClassAllMessages -> #(True, False, False)
    ClassUncertain -> #(False, True, False)
    ClassMuted -> #(False, False, True)
    ClassMentionsOnly -> #(False, False, False)
  }
  native.MaskUpdate(user:, all_messages: all, uncertain:, muted:, active: False)
}
