// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;

use crate::budget::Meter;
use crate::channels::{Channel, ChannelKind};
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelId, ChannelIdx, RoleSetId};
use crate::model::Model;
use crate::shared::View;

pub(crate) fn everyone_views(model: &Model, idx: ChannelIdx) -> bool {
    model
        .signatures
        .planes_of(idx)
        .is_some_and(|p| p.get(RoleSetId::EVERYONE).contains(Visible::VIEW))
}

pub(crate) fn thread_only(kind: ChannelKind) -> bool {
    matches!(kind, ChannelKind::Forum | ChannelKind::Media)
}

fn landing(kind: ChannelKind) -> bool {
    matches!(
        kind,
        ChannelKind::Text | ChannelKind::Voice | ChannelKind::Announcement
    )
}

fn visitor_bits(model: &Model, channel: &Channel) -> Permissions {
    let base = model.roles.everyone_permissions() & !Permissions::ADMINISTRATOR;
    channel.rules().everyone().apply(base)
        & (Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY)
}

impl View<'_> {
    pub fn visitor_permissions(&self, channel: ChannelId) -> Permissions {
        self.model
            .channels
            .by_id(channel)
            .map_or(Permissions::empty(), |c| visitor_bits(&self.model, c))
    }

    pub fn visitor_channels(
        &self,
        adult: bool,
        meter: &mut Meter,
    ) -> Vec<(ChannelId, Permissions)> {
        let model = &self.model;
        let guild_nsfw = model.flags.guild_nsfw();
        let mut out = Vec::new();
        for (_, channel) in model.channels.in_order() {
            meter.charge(8);
            let applied = visitor_bits(model, channel);
            if !applied.contains(Permissions::VIEW_CHANNEL) {
                continue;
            }
            if !adult && effective_nsfw(model, channel, guild_nsfw) {
                continue;
            }
            out.push((channel.id(), applied));
        }
        out
    }

    pub fn everyone_mentions(&self, channels: &[ChannelId], meter: &mut Meter) -> Vec<ChannelId> {
        let model = &self.model;
        let mut seen = BTreeSet::new();
        let mut out = Vec::new();
        for id in channels {
            meter.charge(8);
            if !seen.insert(*id) {
                continue;
            }
            let Some(idx) = model.channels.resolve(*id) else {
                continue;
            };
            let Some(channel) = model.channels.get(idx) else {
                continue;
            };
            if thread_only(channel.kind()) || !channel.is_named() {
                continue;
            }
            if everyone_views(model, idx) {
                out.push(*id);
            }
        }
        out
    }

    pub fn first_everyone_viewable(
        &self,
        candidates: &[ChannelId],
        meter: &mut Meter,
    ) -> Option<ChannelId> {
        let model = &self.model;
        candidates.iter().copied().find(|id| {
            meter.charge(8);
            model.channels.resolve(*id).is_some_and(|idx| {
                model.channels.get(idx).is_some_and(|c| landing(c.kind()))
                    && everyone_views(model, idx)
            })
        })
    }
}

fn effective_nsfw(model: &Model, channel: &Channel, guild_nsfw: bool) -> bool {
    if let Some(value) = channel.nsfw_override() {
        return value;
    }
    if !channel.kind().is_category()
        && let Some(parent) = channel.parent().and_then(|p| model.channels.get(p))
        && let Some(value) = parent.nsfw_override()
    {
        return value;
    }
    guild_nsfw
}
