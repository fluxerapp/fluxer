// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {getCompiledChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';

const REPAIR_DEBOUNCE_MS = 30_000;
const THREAD_SEARCH_ACTIVITY_WINDOW_MS = 30_000;

async function workerService() {
	const {getWorkerService} = await import('@app/api/middleware/ServiceRegistry');
	return getWorkerService();
}

export async function enqueueDeleteChannelThreads(guildId: GuildID, parentId: ChannelID): Promise<void> {
	await (await workerService()).addJob(
		'deleteChannelThreads',
		{guildId: guildId.toString(), parentId: parentId.toString()},
		{jobKey: `delete-channel-threads-${parentId}`},
	);
}

export async function enqueueRemoveThreadMemberships(guildId: GuildID, userId: UserID): Promise<void> {
	await (await workerService()).addJob(
		'removeThreadMembershipsForGuildMember',
		{guildId: guildId.toString(), userId: userId.toString()},
		{jobKey: `remove-thread-memberships-${guildId}-${userId}`},
	);
}

export async function enqueueRebuildThreadAutoArchiveQueue(guildId: string): Promise<void> {
	await (await workerService()).addJob(
		'rebuildThreadAutoArchiveQueue',
		{guildId, configVersion: getCompiledChannelThreadsConfig().config.config_version},
		{jobKey: `rebuild-thread-archive-queue-${guildId}`},
	);
}

export function enqueueRepairThreadIndexes(threadIds: Array<ChannelID>): void {
	if (threadIds.length === 0) return;
	const window = Math.floor(Date.now() / REPAIR_DEBOUNCE_MS);
	const ids = [...new Set(threadIds.map((id) => id.toString()))].sort().slice(0, 1000);
	workerService()
		.then((service) =>
			service.addJob('repairThreadIndexes', {threadIds: ids}, {jobKey: `repair-thread-indexes-${ids[0]}-${window}`}),
		)
		.catch((error) => Logger.warn({error}, 'Failed to enqueue thread index repair'));
}

export function enqueueThreadSearchSync(threadId: ChannelID, opts: {activity?: boolean} = {}): void {
	const window = Math.floor(Date.now() / THREAD_SEARCH_ACTIVITY_WINDOW_MS);
	workerService()
		.then((service) =>
			service.addJob(
				'syncThreadSearchDocument',
				{threadId: threadId.toString()},
				opts.activity
					? {
							jobKey: `sync-thread-search-${threadId}-${window}`,
							runAt: new Date((window + 1) * THREAD_SEARCH_ACTIVITY_WINDOW_MS),
						}
					: undefined,
			),
		)
		.catch((error) => Logger.warn({error}, 'Failed to enqueue thread search sync'));
}

export async function enqueueThreadSearchBackfill(guildId: string): Promise<void> {
	await (await workerService()).addJob(
		'backfillThreadSearch',
		{guildId, configVersion: getCompiledChannelThreadsConfig().config.config_version},
		{jobKey: `backfill-thread-search-${guildId}`},
	);
}
