// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {compareChannelOrdering} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';

export const isTextChannel = (ch: Channel) =>
	ch.type === ChannelTypes.GUILD_TEXT ||
	ch.type === ChannelTypes.GUILD_ANNOUNCEMENT ||
	ch.type === ChannelTypes.GUILD_LINK;
const isVoiceChannel = (ch: Channel) => ch.type === ChannelTypes.GUILD_VOICE;
export const isCategory = (ch: Channel) => ch.type === ChannelTypes.GUILD_CATEGORY;

interface ChannelGroup {
	category?: Channel;
	textChannels: Array<Channel>;
	voiceChannels: Array<Channel>;
}

type ParentBucket = {
	textChannels: Array<Channel>;
	voiceChannels: Array<Channel>;
};

const ROOT_BUCKET_KEY = null;
export const organizeChannels = (channels: ReadonlyArray<Channel>): Array<ChannelGroup> => {
	const categories: Array<Channel> = [];
	const buckets = new Map<string | null, ParentBucket>();
	const ensureBucket = (parentId: string | null): ParentBucket => {
		let bucket = buckets.get(parentId);
		if (!bucket) {
			bucket = {textChannels: [], voiceChannels: []};
			buckets.set(parentId, bucket);
		}
		return bucket;
	};
	const orderedChannels = [...channels].sort(compareChannelOrdering);
	for (const channel of orderedChannels) {
		if (isCategory(channel)) {
			categories.push(channel);
			continue;
		}
		if (isTextChannel(channel)) {
			ensureBucket(channel.parentId).textChannels.push(channel);
		} else if (isVoiceChannel(channel)) {
			ensureBucket(channel.parentId).voiceChannels.push(channel);
		}
	}
	const groups: Array<ChannelGroup> = [];
	const rootBucket = buckets.get(ROOT_BUCKET_KEY);
	groups.push({
		textChannels: rootBucket?.textChannels ?? [],
		voiceChannels: rootBucket?.voiceChannels ?? [],
	});
	for (const category of categories) {
		const bucket = buckets.get(category.id);
		groups.push({
			category,
			textChannels: bucket?.textChannels ?? [],
			voiceChannels: bucket?.voiceChannels ?? [],
		});
	}
	return groups;
};
