// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {type ThreadDispatchEvent, threadUpdateEvent} from '@app/api/channel/services/thread/ThreadDispatch';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import {everEnabled, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadState} from '@app/api/models/ThreadState';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {PUBLIC_THREAD_CHANNEL_TYPES, publicThreadTypeFor} from '@fluxer/constants/src/ThreadConstants';
import {ChannelHasThreadsError} from '@fluxer/errors/src/domains/channel/ChannelHasThreadsError';

const RETYPE_CONCURRENCY = 8;
export const PARENT_CONVERSION_LOCK_TTL_SECONDS = 30;

export type ParentThreads = Array<{threadId: ChannelID; type: number}>;

export async function loadConvertibleParentThreads(
	channelRepository: ChannelRepository,
	guildId: GuildID,
	parent: Channel,
	nextType: number,
): Promise<ParentThreads> {
	if (!everEnabled() || !(await isTainted(guildId, {fresh: true}))) return [];
	const threads = await channelRepository.threads.listParentThreads(parent.id);
	if (
		nextType === ChannelTypes.GUILD_ANNOUNCEMENT &&
		threads.some((thread) => thread.type === ChannelTypes.PRIVATE_THREAD)
	) {
		throw new ChannelHasThreadsError();
	}
	return threads;
}

export async function retypeParentThreads<T>(
	channelRepository: ChannelRepository,
	threads: ParentThreads,
	parentType: number,
	writeParent: () => Promise<T>,
): Promise<{parent: T; active: Array<ThreadState>}> {
	const type = publicThreadTypeFor(parentType);
	const pending = threads.filter((thread) => thread.type !== type && PUBLIC_THREAD_CHANNEL_TYPES.has(thread.type));
	const retyped: ParentThreads = [];
	try {
		const states = await mapWithConcurrency(pending, RETYPE_CONCURRENCY, async (thread) => {
			const state = await channelRepository.threads.setThreadType(thread.threadId, type);
			retyped.push(thread);
			return state;
		});
		const parent = await writeParent();
		return {parent, active: states.filter((state): state is ThreadState => state !== null && !state.archived)};
	} catch (error) {
		await mapWithConcurrency(retyped, RETYPE_CONCURRENCY, (thread) =>
			channelRepository.threads.setThreadType(thread.threadId, thread.type),
		).catch((rollbackError) => {
			Logger.error({error: rollbackError}, 'Failed to roll back thread retype after a failed parent conversion');
		});
		throw error;
	}
}

export async function retypedThreadEvents(
	channelRepository: ChannelRepository,
	parent: Channel,
	active: Array<ThreadState>,
): Promise<Array<ThreadDispatchEvent>> {
	if (active.length === 0) return [];
	const views = await loadThreadViews(channelRepository, active, [parent]);
	return views.map((view) => threadUpdateEvent(view));
}
