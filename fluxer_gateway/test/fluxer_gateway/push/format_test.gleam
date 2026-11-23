// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/format
import fluxer_gateway/push/settings
import gleam/dynamic
import gleam/dynamic/decode
import gleam/option.{None, Some}

pub fn truncate_keeps_whole_characters_test() {
  let assert "ab" = format.truncate_bytes("abc", 2)
  let assert "a" = format.truncate_bytes("aé", 2)
  let assert "aé" = format.truncate_bytes("aé", 3)
}

pub fn build_url_test() {
  let assert "/channels/@me/5/9" = format.build_url(0, 5, 9)
  let assert "/channels/1/5/9" = format.build_url(1, 5, 9)
}

pub fn channel_mention_ids_test() {
  let assert [12, 34] =
    format.channel_mention_ids("see <#34> and <#12> and <#x> <#34>")
}

pub fn settings_decoder_is_lenient_test() {
  let raw =
    dynamic.properties([
      #(dynamic.string("muted"), dynamic.bool(True)),
      #(dynamic.string("message_notifications"), dynamic.string("loud")),
      #(dynamic.string("mobile_push"), dynamic.nil()),
    ])
  let assert Ok(decoded) = decode.run(raw, settings.decoder())
  let assert settings.MutedForever = settings.mute(decoded)
  let assert None = settings.level(decoded)
  let assert True = settings.mobile_push(decoded)
}

pub fn level_resolution_test() {
  let s =
    settings.new(
      mobile_push: True,
      level: Some(3),
      mute: settings.NotMuted,
      suppress_everyone: False,
      suppress_roles: False,
      overrides: [
        #(
          10,
          settings.channel_override(
            level: Some(2),
            mute: settings.NotMuted,
            flags: 0,
          ),
        ),
        #(
          11,
          settings.channel_override(
            level: Some(-1),
            mute: settings.NotMuted,
            flags: 0,
          ),
        ),
      ],
    )
  let assert 2 = settings.resolve_level(s, 10, 1)
  let assert 1 = settings.resolve_level(s, 11, 1)
  let assert 0 = settings.resolve_level(s, 12, 3)
}
