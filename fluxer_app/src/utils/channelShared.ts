/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';

type MinimalChannel = Pick<ChannelRecord, 'id' | 'type' | 'position' | 'guildId'>;

const VIEWABLE_CHANNEL_TYPES = new Set<number>([ChannelTypes.GUILD_TEXT, ChannelTypes.GUILD_VOICE]);

export const compareChannelPosition = (a: MinimalChannel, b: MinimalChannel): number => {
	if (a.position !== b.position) {
		return (a.position ?? 0) - (b.position ?? 0);
	}
	return a.id.localeCompare(b.id);
};

export const filterViewableChannels = <T extends MinimalChannel>(channels: ReadonlyArray<T>): Array<T> =>
	channels.filter((channel) => VIEWABLE_CHANNEL_TYPES.has(channel.type));

export const pickDefaultGuildChannelId = ({
	guildId,
	channels,
	selectedChannelId,
	systemChannelId,
	rulesChannelId,
}: {
	guildId: string;
	channels: ReadonlyArray<MinimalChannel>;
	selectedChannelId?: string | null;
	systemChannelId?: string | null;
	rulesChannelId?: string | null;
}): string | null => {
	if (!channels.length) return null;

	const channelById = new Map(channels.map((channel) => [channel.id, channel]));

	const isChannelInGuild = (channelId?: string | null) =>
		channelId ? channelById.get(channelId)?.guildId === guildId : false;

	if (isChannelInGuild(selectedChannelId)) {
		return selectedChannelId!;
	}

	if (isChannelInGuild(systemChannelId)) {
		return systemChannelId!;
	}

	if (isChannelInGuild(rulesChannelId)) {
		return rulesChannelId!;
	}

	const viewable = [...filterViewableChannels(channels)].sort(compareChannelPosition);
	return viewable[0]?.id ?? null;
};
