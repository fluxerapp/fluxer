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

import type {GuildID, ReportID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createGuildID} from '@fluxer/api/src/BrandedTypes';
import {GuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import {Logger} from '@fluxer/api/src/Logger';
import type {User} from '@fluxer/api/src/models/User';
import {
	getAuditLogSearchService,
	getGuildMemberSearchService,
	getGuildSearchService,
	getMessageSearchService,
	getReportSearchService,
	getUserSearchService,
} from '@fluxer/api/src/SearchFactory';
import {getWorkerDependencies} from '@fluxer/api/src/worker/WorkerContext';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@fluxer/worker/src/contracts/WorkerTask';
import {seconds} from 'itty-time';
import {z} from 'zod';

const INDEX_TYPES = [
	'guilds',
	'users',
	'reports',
	'audit_logs',
	'channel_messages',
	'guild_members',
	'discovery',
] as const;
type IndexType = (typeof INDEX_TYPES)[number];

const PayloadSchema = z
	.object({
		index_type: z.enum(INDEX_TYPES),
		job_id: z.string(),
		admin_user_id: z.string(),
		guild_id: z.string().optional(),
		user_id: z.string().optional(),
	})
	.refine(
		(data) => {
			if (data.index_type === 'channel_messages' || data.index_type === 'guild_members') {
				return data.guild_id !== undefined;
			}
			return true;
		},
		{message: 'guild_id is required for the channel_messages and guild_members index type'},
	);

type RefreshSearchIndexPayload = z.infer<typeof PayloadSchema>;

const BATCH_SIZE = 100;
const PROGRESS_TTL = seconds('1 hour');

function requireSearchService<T>(service: T | null): T {
	if (!service) {
		throw new Error('Search is not enabled');
	}
	return service;
}

async function setProgress(kvClient: IKVProvider, progressKey: string, data: Record<string, unknown>): Promise<void> {
	await kvClient.set(progressKey, JSON.stringify(data), 'EX', PROGRESS_TTL);
}

async function reportInProgress(
	kvClient: IKVProvider,
	progressKey: string,
	indexType: IndexType,
	indexed: number,
): Promise<void> {
	await setProgress(kvClient, progressKey, {
		status: 'in_progress',
		index_type: indexType,
		total: indexed,
		indexed,
		started_at: new Date().toISOString(),
	});
}

interface PaginateAndIndexOptions<TCursor, TItem> {
	fetchPage: (cursor: TCursor | undefined) => Promise<Array<TItem>>;
	indexBatch: (items: Array<TItem>) => Promise<void>;
	getCursor: (item: TItem) => TCursor;
	label: string;
	kvClient: IKVProvider;
	progressKey: string;
	indexType: IndexType;
}

async function paginateAndIndex<TCursor, TItem>(options: PaginateAndIndexOptions<TCursor, TItem>): Promise<number> {
	let cursor: TCursor | undefined;
	let indexedCount = 0;
	let hasMore = true;

	while (hasMore) {
		const items = await options.fetchPage(cursor);

		if (items.length > 0) {
			await options.indexBatch(items);
			indexedCount += items.length;
			cursor = options.getCursor(items[items.length - 1]!);
			await reportInProgress(options.kvClient, options.progressKey, options.indexType, indexedCount);
			Logger.debug({count: items.length, total: indexedCount}, `Indexed ${options.label} batch`);
		}

		hasMore = items.length === BATCH_SIZE;
	}

	Logger.debug({count: indexedCount}, `Refreshed ${options.label} search index`);
	return indexedCount;
}

type IndexHandler = (
	payload: RefreshSearchIndexPayload,
	helpers: WorkerTaskHelpers,
	kvClient: IKVProvider,
	progressKey: string,
) => Promise<number>;

const refreshGuilds: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
	const {guildRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getGuildSearchService());
	await searchService.deleteAllDocuments();

	return paginateAndIndex({
		fetchPage: (cursor?: GuildID) => guildRepository.listAllGuildsPaginated(BATCH_SIZE, cursor),
		indexBatch: (guilds) => searchService.indexGuilds(guilds),
		getCursor: (guild) => guild.id,
		label: 'guild',
		kvClient,
		progressKey,
		indexType: 'guilds',
	});
};

const refreshUsers: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
	const {userRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getUserSearchService());
	await searchService.deleteAllDocuments();

	return paginateAndIndex({
		fetchPage: (cursor?: UserID) => userRepository.listAllUsersPaginated(BATCH_SIZE, cursor),
		indexBatch: (users) => searchService.indexUsers(users),
		getCursor: (user) => user.id,
		label: 'user',
		kvClient,
		progressKey,
		indexType: 'users',
	});
};

const refreshReports: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
	const {reportRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getReportSearchService());
	await searchService.deleteAllDocuments();

	return paginateAndIndex({
		fetchPage: (cursor?: ReportID) => reportRepository.listAllReportsPaginated(BATCH_SIZE, cursor),
		indexBatch: (reports) => searchService.indexReports(reports),
		getCursor: (report) => report.reportId,
		label: 'report',
		kvClient,
		progressKey,
		indexType: 'reports',
	});
};

const refreshAuditLogs: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
	const {adminRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getAuditLogSearchService());
	await searchService.deleteAllDocuments();

	return paginateAndIndex({
		fetchPage: (cursor?: bigint) => adminRepository.listAllAuditLogsPaginated(BATCH_SIZE, cursor),
		indexBatch: (logs) => searchService.indexAuditLogs(logs),
		getCursor: (log) => log.logId,
		label: 'audit log',
		kvClient,
		progressKey,
		indexType: 'audit_logs',
	});
};

const refreshChannelMessages: IndexHandler = async (payload, helpers, _kvClient, _progressKey) => {
	const {channelRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getMessageSearchService());

	const guildId = createGuildID(BigInt(payload.guild_id!));
	await searchService.deleteGuildMessages(guildId);

	const channels = await channelRepository.listGuildChannels(guildId);

	for (const channel of channels) {
		Logger.debug({channelId: channel.id.toString()}, 'Indexing channel messages');
		await helpers.addJob(
			'indexChannelMessages',
			{channelId: channel.id.toString()},
			{jobKey: `index-channel-${channel.id}-initial`, maxAttempts: 3},
		);
	}

	Logger.debug({channels: channels.length, guildId: guildId.toString()}, 'Queued channel message indexing jobs');
	return channels.length;
};

const refreshGuildMembers: IndexHandler = async (payload, _helpers, kvClient, progressKey) => {
	const {guildRepository, userRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getGuildMemberSearchService());

	const guildId = createGuildID(BigInt(payload.guild_id!));
	await searchService.deleteGuildMembers(guildId);

	const indexedCount = await paginateAndIndex({
		fetchPage: async (cursor?: UserID) => {
			const members = await guildRepository.listMembersPaginated(guildId, BATCH_SIZE, cursor);
			const userIds = new Set(members.map((m) => m.userId));
			const userMap = new Map<UserID, User>();

			for (const uid of userIds) {
				const user = await userRepository.findUnique(uid);
				if (user) {
					userMap.set(uid, user);
				}
			}

			return members
				.map((member) => {
					const user = userMap.get(member.userId);
					return user ? {member, user} : null;
				})
				.filter((item): item is NonNullable<typeof item> => item != null);
		},
		indexBatch: (membersWithUsers) => searchService.indexMembers(membersWithUsers),
		getCursor: (item) => item.member.userId,
		label: 'guild member',
		kvClient,
		progressKey,
		indexType: 'guild_members',
	});

	const guild = await guildRepository.findUnique(guildId);
	if (guild) {
		await guildRepository.upsert({
			...guild.toRow(),
			members_indexed_at: new Date(),
		});
	}

	return indexedCount;
};

const DISCOVERY_BATCH_SIZE = 50;

const refreshDiscovery: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
	const {guildRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getGuildSearchService());
	const discoveryRepository = new GuildDiscoveryRepository();

	const approvedRows = await discoveryRepository.listByStatus(DiscoveryApplicationStatus.APPROVED, 1000);
	if (approvedRows.length === 0) {
		return 0;
	}

	const guildIds = approvedRows.map((row) => row.guild_id);

	let synced = 0;
	for (let i = 0; i < guildIds.length; i += DISCOVERY_BATCH_SIZE) {
		const batch = guildIds.slice(i, i + DISCOVERY_BATCH_SIZE);
		for (const guildId of batch) {
			const guild = await guildRepository.findUnique(guildId);
			if (!guild) continue;

			const discoveryRow = await discoveryRepository.findByGuildId(guildId);
			if (!discoveryRow) continue;

			await searchService.updateGuild(guild, {
				description: discoveryRow.description,
				categoryId: discoveryRow.category_type,
			});
			synced++;
		}

		await setProgress(kvClient, progressKey, {
			status: 'in_progress',
			index_type: 'discovery',
			total: guildIds.length,
			indexed: synced,
			started_at: new Date().toISOString(),
		});
	}

	return synced;
};

const INDEX_HANDLERS: Record<IndexType, IndexHandler> = {
	guilds: refreshGuilds,
	users: refreshUsers,
	reports: refreshReports,
	audit_logs: refreshAuditLogs,
	channel_messages: refreshChannelMessages,
	guild_members: refreshGuildMembers,
	discovery: refreshDiscovery,
};

const refreshSearchIndex: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing refreshSearchIndex task');

	const {kvClient} = getWorkerDependencies();
	const progressKey = `index_refresh_status:${validated.job_id}`;

	await reportInProgress(kvClient, progressKey, validated.index_type, 0);

	try {
		const handler = INDEX_HANDLERS[validated.index_type];
		const indexedCount = await handler(validated, helpers, kvClient, progressKey);

		await setProgress(kvClient, progressKey, {
			status: 'completed',
			index_type: validated.index_type,
			total: indexedCount,
			indexed: indexedCount,
			completed_at: new Date().toISOString(),
		});
	} catch (error) {
		Logger.error({error, payload: validated}, 'Failed to refresh search index');

		await setProgress(kvClient, progressKey, {
			status: 'failed',
			index_type: validated.index_type,
			error: error instanceof Error ? error.message : 'Unknown error',
			failed_at: new Date().toISOString(),
		});

		throw error;
	}
};

export default refreshSearchIndex;
