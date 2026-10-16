// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

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
	threadsActive = false,
}: {
	guildId: string;
	channels: ReadonlyArray<MinimalChannel>;
	selectedChannelId?: string | null;
	threadsActive?: boolean;
}): string | null {
	const guildChannels = channels.filter((channel) => channel.guildId === guildId);
	const viewable = filterViewableChannels(guildChannels);
	const threadOnly = threadsActive
		? guildChannels.filter((channel) => THREAD_ONLY_CHANNEL_TYPES.has(channel.type))
		: [];
	if (selectedChannelId && [...viewable, ...threadOnly].some((channel) => channel.id === selectedChannelId)) {
		return selectedChannelId;
	}
	const candidates = viewable.length ? viewable : threadOnly;
	if (!candidates.length) return null;
	return candidates.sort(compareChannelPosition)[0].id;
}
