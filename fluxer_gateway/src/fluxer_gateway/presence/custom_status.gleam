// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/presence/display.{type Display}
import fluxer_gateway/util/iso8601
import gleam/bit_array
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type CustomStatus {
  CustomStatus(
    key: BitArray,
    wire: BitArray,
    expires_at_ms: Option(Int),
    payload: List(#(String, BitArray)),
  )
}

pub type OpValue {
  OpAbsent
  OpNull
  OpMap(CustomStatus)
  OpOther
}

pub type Request {
  Keep
  Clear
  Validate(candidate: CustomStatus)
}

pub type ValidationReply {
  Validated(Option(CustomStatus))
  ValidatedWithoutValue
  Rejected
}

pub type SettingsValue {
  SettingsAbsent
  SettingsMap(CustomStatus)
  SettingsOther
}

const max_timer_ms = 86_400_000

const compared_fields = ["text", "expires_at", "emoji_id", "emoji_name"]

pub fn from_tree(value: JsonTree) -> Result(CustomStatus, Nil) {
  use expanded <- result.try(json_tree.expand(value))
  case expanded {
    json_tree.Object(fields) -> {
      use wire <- result.try(json_tree.canonical(expanded))
      use payload <- result.map(
        list.try_fold(list.reverse(compared_fields), [], fn(acc, name) {
          case list.key_find(fields, name) {
            Ok(found) ->
              json_tree.canonical(found)
              |> result.map(fn(bytes) { [#(name, bytes), ..acc] })
            Error(Nil) -> Ok(acc)
          }
        }),
      )
      CustomStatus(
        key: compare_key(payload),
        wire:,
        expires_at_ms: expiry_of(fields),
        payload:,
      )
    }
    _ -> Error(Nil)
  }
}

fn compare_key(payload: List(#(String, BitArray))) -> BitArray {
  compared_fields
  |> list.map(fn(name) {
    list.key_find(payload, name) |> result.unwrap(<<"null":utf8>>)
  })
  |> list.intersperse(<<",":utf8>>)
  |> bit_array.concat
}

fn expiry_of(fields: List(#(String, JsonTree))) -> Option(Int) {
  list.key_find(fields, "expires_at")
  |> result.try(json_tree.string)
  |> result.try(iso8601.parse_ms)
  |> option.from_result
}

pub fn op_value(raw: Option(RawJson)) -> OpValue {
  case raw {
    None -> OpAbsent
    Some(bytes) -> {
      let tree = json_tree.from_raw(bytes)
      case json_tree.expand(tree) {
        Ok(json_tree.Null) -> OpNull
        Ok(_) ->
          case from_tree(tree) {
            Ok(status) -> OpMap(status)
            Error(Nil) -> OpOther
          }
        Error(Nil) -> OpOther
      }
    }
  }
}

pub fn settings_value(value: Result(JsonTree, Nil)) -> SettingsValue {
  case value {
    Error(Nil) -> SettingsAbsent
    Ok(found) ->
      case from_tree(found) {
        Ok(status) -> SettingsMap(status)
        Error(Nil) -> SettingsOther
      }
  }
}

pub fn initial(raw: Option(RawJson)) -> Option(CustomStatus) {
  case op_value(raw) {
    OpMap(status) -> Some(status)
    OpAbsent | OpNull | OpOther -> None
  }
}

pub fn key(status: CustomStatus) -> BitArray {
  status.key
}

pub fn wire(status: CustomStatus) -> BitArray {
  status.wire
}

pub fn expires_at_ms(status: CustomStatus) -> Option(Int) {
  status.expires_at_ms
}

pub fn validation_payload(status: CustomStatus) -> Json {
  status.payload
  |> list.map(fn(pair) { #(pair.0, raw_json.to_json(raw_json.wrap(pair.1))) })
  |> json.object
}

pub fn classify(current: Option(CustomStatus), candidate: OpValue) -> Request {
  case candidate, current {
    OpAbsent, _ | OpOther, _ -> Keep
    OpNull, _ -> Clear
    OpMap(new), Some(old) if old.key == new.key -> Keep
    OpMap(new), _ -> Validate(new)
  }
}

pub fn from_validated(
  current: Option(CustomStatus),
  reply: ValidationReply,
) -> Option(CustomStatus) {
  case reply {
    Validated(value) -> value
    ValidatedWithoutValue -> None
    Rejected -> current
  }
}

pub fn from_settings(
  current: Option(CustomStatus),
  value: SettingsValue,
) -> Option(CustomStatus) {
  case value {
    SettingsAbsent -> current
    SettingsMap(status) -> Some(status)
    SettingsOther -> None
  }
}

fn expired(status: CustomStatus, now_ms: Int) -> Bool {
  case status.expires_at_ms {
    Some(at) -> at - now_ms <= 0
    None -> False
  }
}

pub fn visible(
  current: Option(CustomStatus),
  now_ms now_ms: Int,
  restricted restricted: Bool,
  display shown: Display,
) -> Option(CustomStatus) {
  case display.is_visible(shown) && !restricted {
    True ->
      option.then(current, fn(value) {
        case expired(value, now_ms) {
          True -> None
          False -> Some(value)
        }
      })
    False -> None
  }
}

pub fn visible_wire(
  current: Option(CustomStatus),
  now_ms now_ms: Int,
  restricted restricted: Bool,
  display shown: Display,
) -> BitArray {
  case visible(current, now_ms:, restricted:, display: shown) {
    Some(value) -> value.wire
    None -> <<"null":utf8>>
  }
}

pub fn wakeup_ms(status: Option(CustomStatus), now_ms: Int) -> Option(Int) {
  case status {
    Some(CustomStatus(expires_at_ms: Some(at), ..)) if at - now_ms > 0 ->
      Some(int.min(at - now_ms, max_timer_ms))
    _ -> None
  }
}

pub fn payload(status: CustomStatus) -> List(#(String, BitArray)) {
  status.payload
}

pub fn restore(
  wire wire: BitArray,
  expires_at_ms expires_at_ms: Option(Int),
  payload payload: List(#(String, BitArray)),
) -> CustomStatus {
  CustomStatus(key: compare_key(payload), wire:, expires_at_ms:, payload:)
}
