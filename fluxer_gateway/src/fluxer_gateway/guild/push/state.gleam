// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/guild/push/admission.{type Admission}
import fluxer_gateway/guild/push/engagement.{type Engagement}
import fluxer_gateway/guild/push/guild_info.{type GuildInfo}
import fluxer_gateway/guild/push/masks.{type Class}
import fluxer_gateway/guild/push/profiles.{type Profiles}
import fluxer_gateway/push/settings.{type Settings}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option, None}

pub type GuildMessage {
  ReleasedHolds(sessions: List(String), reply: Subject(List(String)))
  ProfileSynced(user: Int)
  ProfilesFetched(key: masks.Key, classes: List(FetchedClass))
}

pub type FetchedClass {
  FetchedClass(user: Int, settings: Settings, class: Class)
}

pub type GuildPush {
  GuildPush(
    guild: GuildId,
    inbox: Subject(GuildMessage),
    engagement: Engagement,
    admission: Admission(Pid),
    info: GuildInfo,
    profiles: Profiles,
    mask_key: Option(masks.Key),
    next_profile_sweep_ms: Int,
  )
}

pub fn new(guild: GuildId) -> GuildPush {
  let inbox = process.new_subject()
  GuildPush(
    guild:,
    inbox:,
    engagement: engagement.new(),
    admission: admission.new(),
    info: guild_info.empty(),
    profiles: profiles.new(guild, fn(user) {
      process.send(inbox, ProfileSynced(user))
    }),
    mask_key: None,
    next_profile_sweep_ms: 0,
  )
}
