// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {guildActive} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import {
	backfillThreadSearch as backfillGuildThreadSearch,
	syncThreadSearchDocument as syncDocument,
} from '@app/api/search/thread/ThreadSearchService';
import {ensureChannelThreadsConfigVersion} from '@app/api/worker/tasks/SeedThreadPermissions';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const SnowflakeString = z.string().regex(/^\d{1,20}$/);
const ThreadPayload = z.object({threadId: SnowflakeString});
const GuildPayload = z.object({guildId: SnowflakeString, configVersion: z.number().int().min(0).optional()});

export const syncThreadSearchDocument: WorkerTaskHandler = async (payload) => {
	const threadId = createChannelID(BigInt(ThreadPayload.parse(payload).threadId));
	await syncDocument(getWorkerDependencies().channelRepository, threadId);
};

export const backfillThreadSearch: WorkerTaskHandler = async (payload) => {
	const validated = GuildPayload.parse(payload);
	await ensureChannelThreadsConfigVersion(validated.configVersion);
	const guildId = createGuildID(BigInt(validated.guildId));
	if (!guildActive(guildId)) return;
	const {channelRepository} = getWorkerDependencies();
	if ((await channelRepository.threads.getGuildMarker(guildId))?.search_backfilled_at) return;
	const indexed = await backfillGuildThreadSearch(channelRepository, guildId);
	Logger.info({guildId: guildId.toString(), indexed}, 'Backfilled thread search index');
};
