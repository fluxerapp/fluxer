// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';

type MinimalChannel = Pick<Channel, 'id' | 'type' | 'position' | 'guildId'>;

export function compareChannelPosition(a: MinimalChannel, b: MinimalChannel): number {
	if (a.position !== b.position) {
		return (a.position ?? 0) - (b.position ?? 0);
	}
	return a.id.localeCompare(b.id);
}

export function filterViewableChannels<T extends MinimalChannel>(channels: ReadonlyArray<T>): Array<T> {
	return channels.filter((channel) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type));
}

export function pickDefaultGuildChannelId({
	guildId,
	channels,
	selectedChannelId,
}: {
	guildId: string;
	channels: ReadonlyArray<MinimalChannel>;
	selectedChannelId?: string | null;
}): string | null {
	const viewable = filterViewableChannels(channels.filter((channel) => channel.guildId === guildId));
	if (!viewable.length) return null;
	if (selectedChannelId && viewable.some((channel) => channel.id === selectedChannelId)) {
		return selectedChannelId;
	}
	return viewable.sort(compareChannelPosition)[0].id;
}
