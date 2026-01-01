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
import * as ChannelUtils from '~/utils/ChannelUtils';

export const isTextChannel = (ch: ChannelRecord) =>
	ch.type === ChannelTypes.GUILD_TEXT || ch.type === ChannelTypes.GUILD_LINK;

const isVoiceChannel = (ch: ChannelRecord) => ch.type === ChannelTypes.GUILD_VOICE;

export const isCategory = (ch: ChannelRecord) => ch.type === ChannelTypes.GUILD_CATEGORY;

interface ChannelGroup {
	category?: ChannelRecord;
	textChannels: Array<ChannelRecord>;
	voiceChannels: Array<ChannelRecord>;
}

export const organizeChannels = (channels: ReadonlyArray<ChannelRecord>): Array<ChannelGroup> => {
	const categories = channels.filter(isCategory).sort(ChannelUtils.compareChannels);
	const channelsByParent = new Map<string | null, Array<ChannelRecord>>();

	for (const channel of channels.filter((ch) => !isCategory(ch))) {
		const parentId = channel.parentId;
		if (!channelsByParent.has(parentId)) channelsByParent.set(parentId, []);
		channelsByParent.get(parentId)!.push(channel);
	}

	const groups: Array<ChannelGroup> = [];
	const nullChannels = channelsByParent.get(null) || [];
	groups.push({
		textChannels: nullChannels.filter(isTextChannel).sort(ChannelUtils.compareChannels),
		voiceChannels: nullChannels.filter(isVoiceChannel).sort(ChannelUtils.compareChannels),
	});

	for (const category of categories) {
		const categoryChannels = channelsByParent.get(category.id) || [];
		groups.push({
			category,
			textChannels: categoryChannels.filter(isTextChannel).sort(ChannelUtils.compareChannels),
			voiceChannels: categoryChannels.filter(isVoiceChannel).sort(ChannelUtils.compareChannels),
		});
	}

	return groups;
};
