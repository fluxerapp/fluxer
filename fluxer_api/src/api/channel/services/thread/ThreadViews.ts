// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ThreadView} from '@app/api/channel/services/thread/ThreadMappers';
import type {Channel} from '@app/api/models/Channel';
import {ThreadState} from '@app/api/models/ThreadState';
import {ThreadStats} from '@app/api/models/ThreadStats';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

async function liveTagIds(
	repository: ChannelRepository,
	guildId: GuildID,
	parentId: ChannelID,
): Promise<ReadonlySet<bigint>> {
	const config = await repository.threads.getParentConfig(guildId, parentId);
	return new Set(config?.availableTags.map((tag) => tag.id));
}

function withLiveTags(state: ThreadState, tagIds: ReadonlySet<bigint> | undefined): ThreadState {
	if (!tagIds || state.appliedTags.every((tag) => tagIds.has(tag))) return state;
	return new ThreadState({...state.toRow(), applied_tags: state.appliedTags.filter((tag) => tagIds.has(tag))});
}

export async function loadThreadViews(
	repository: ChannelRepository,
	states: Array<ThreadState>,
	knownParents: ReadonlyArray<Channel> = [],
): Promise<Array<ThreadView>> {
	if (states.length === 0) return [];
	const threadIds = states.map((state) => state.threadId);
	const parentTypes = new Map<ChannelID, number>(knownParents.map((parent) => [parent.id, parent.type]));
	const missingParents = [...new Set(states.map((state) => state.parentId))].filter((id) => !parentTypes.has(id));
	const [channels, stats, parents] = await Promise.all([
		repository.channelData.listChannels(threadIds),
		repository.threads.getStatsMany(threadIds),
		missingParents.length > 0 ? repository.channelData.listChannels(missingParents) : Promise.resolve([]),
	]);
	for (const parent of parents) parentTypes.set(parent.id, parent.type);
	const taggedParents = [
		...new Map(
			states
				.filter(
					(state) =>
						state.appliedTags.length > 0 && THREAD_ONLY_CHANNEL_TYPES.has(parentTypes.get(state.parentId) ?? -1),
				)
				.map((state) => [state.parentId, state.guildId] as const),
		),
	];
	const tagsByParent = new Map(
		await Promise.all(
			taggedParents.map(
				async ([parentId, guildId]) => [parentId, await liveTagIds(repository, guildId, parentId)] as const,
			),
		),
	);
	const channelById = new Map(channels.map((channel) => [channel.id, channel]));
	return states.flatMap((state) => {
		const channel = channelById.get(state.threadId);
		if (!channel) return [];
		return [
			{
				channel,
				state: withLiveTags(state, tagsByParent.get(state.parentId)),
				stats: stats.get(state.threadId) ?? ThreadStats.empty(state.threadId),
				parentType: parentTypes.get(state.parentId) ?? null,
			},
		];
	});
}

export async function loadThreadView(
	repository: ChannelRepository,
	channel: Channel,
	state: ThreadState,
	parent: Channel,
): Promise<ThreadView> {
	const [stats, tagIds] = await Promise.all([
		repository.threads.getStats(channel.id),
		state.appliedTags.length > 0 && parent.isThreadOnly()
			? liveTagIds(repository, state.guildId, parent.id)
			: Promise.resolve(undefined),
	]);
	return {channel, state: withLiveTags(state, tagIds), stats, parentType: parent.type};
}
