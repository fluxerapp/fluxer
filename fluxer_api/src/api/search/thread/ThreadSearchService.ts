// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, type GuildID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ThreadView} from '@app/api/channel/services/thread/ThreadMappers';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import {guildActive} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import {getThreadSearchService} from '@app/api/SearchFactory';
import type {IThreadSearchService} from '@app/api/search/IThreadSearchService';
import type {SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {
	SearchableThread,
	ThreadSearchCursor,
	ThreadSearchFilters,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate, TIMESTAMP_SHIFT} from '@fluxer/snowflake/src/Snowflake';

const BACKFILL_BATCH_SIZE = 100;

function millis(id: bigint | null | undefined, fallback: Date): number {
	return id != null ? snowflakeToDate(id).getTime() : fallback.getTime();
}

export function threadSearchCursor(id: bigint): ThreadSearchCursor {
	return {createdAt: snowflakeToDate(id).getTime(), idSequence: Number(id & ((1n << TIMESTAMP_SHIFT) - 1n))};
}

export function toSearchableThread(view: ThreadView): SearchableThread {
	const {channel, state} = view;
	return {
		id: channel.id.toString(),
		guildId: state.guildId.toString(),
		parentId: state.parentId.toString(),
		type: channel.type,
		name: channel.name ?? '',
		ownerId: channel.ownerId?.toString() ?? null,
		archived: state.archived,
		locked: state.locked,
		appliedTagIds: state.appliedTags.map((tag) => tag.toString()),
		...threadSearchCursor(channel.id),
		lastMessageAt: millis(channel.lastMessageId, state.createdAt),
		archivedAt: (state.archiveTimestamp ?? state.createdAt).getTime(),
	};
}

async function readyService(): Promise<IThreadSearchService | null> {
	const service = getThreadSearchService();
	if (!service) return null;
	await service.ready();
	return service;
}

export async function syncThreadSearchDocument(repository: ChannelRepository, threadId: ChannelID): Promise<void> {
	const service = await readyService();
	if (!service) return;
	const state = await repository.threads.getState(threadId);
	if (state && !guildActive(state.guildId)) return;
	const [view] = state ? await loadThreadViews(repository, [state]) : [];
	if (!view) {
		await service.deleteDocument(threadId.toString());
		return;
	}
	await service.indexDocument(toSearchableThread(view));
}

export async function deleteThreadSearchDocuments(threadIds: ReadonlyArray<ChannelID>): Promise<void> {
	if (threadIds.length === 0) return;
	try {
		const service = await readyService();
		if (!service) return;
		await service.deleteDocuments(threadIds.map((id) => id.toString()));
	} catch (error) {
		Logger.warn({error, count: threadIds.length}, 'Failed to delete thread search documents');
	}
}

export async function backfillThreadSearch(repository: ChannelRepository, guildId: GuildID): Promise<number> {
	const service = await readyService();
	if (!service || !guildActive(guildId)) return 0;
	const threadIds = await repository.threads.listGuildThreadIds(guildId);
	let indexed = 0;
	for (let offset = 0; offset < threadIds.length; offset += BACKFILL_BATCH_SIZE) {
		const states = await repository.threads.getStates(threadIds.slice(offset, offset + BACKFILL_BATCH_SIZE));
		const views = await loadThreadViews(repository, states);
		await service.indexDocuments(views.map(toSearchableThread));
		indexed += views.length;
	}
	await service.refreshIndex();
	await repository.threads.markGuildSearchBackfilled(guildId, new Date());
	return indexed;
}

export async function searchThreadDocuments(params: {
	name: string;
	filters: ThreadSearchFilters;
	limit: number;
	offset: number;
}): Promise<SearchResult<SearchableThread> | null> {
	const service = await readyService();
	if (!service) return null;
	return service.search(params.name, params.filters, {limit: params.limit, offset: params.offset});
}

export function searchHitThreadIds(result: SearchResult<SearchableThread>): Array<ChannelID> {
	return result.hits.map((hit) => createChannelID(BigInt(hit.id)));
}
