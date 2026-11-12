// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {openInAppOrExternalUrl} from '@app/features/messaging/components/markdown/renderers/LinkRenderer';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {i18n} from '@lingui/core';

interface OpenLinkChannelOptions {
	skipGate?: boolean;
}

const openingLinkChannelIds = new Set<string>();

export function openLinkChannel(channel: Channel, options: OpenLinkChannelOptions = {}): boolean {
	if (channel.type !== ChannelTypes.GUILD_LINK || !channel.url) return false;
	if (openingLinkChannelIds.has(channel.id)) return true;
	if (
		!options.skipGate &&
		GuildMatureContentAgree.shouldShowGate({
			channelId: channel.id,
			guildId: channel.guildId ?? null,
		})
	) {
		return false;
	}
	openingLinkChannelIds.add(channel.id);
	try {
		openInAppOrExternalUrl(channel.url, {i18n, guildId: channel.guildId ?? undefined});
	} finally {
		openingLinkChannelIds.delete(channel.id);
	}
	return true;
}
