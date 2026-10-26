// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import type {ChannelID, GuildID, ReportID, UserID} from '@app/api/BrandedTypes';
import {createGuildID} from '@app/api/BrandedTypes';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	getAuditLogSearchService,
	getGuildMemberSearchService,
	getGuildSearchService,
	getMessageSearchService,
	getReportSearchService,
	getThreadSearchService,
	getUserSearchService,
} from '@app/api/SearchFactory';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {
	finishSearchReindexAll,
	holdSearchReindexAll,
	INDEX_REFRESH_STATUS_TTL_SECONDS,
	indexRefreshStatusKey,
} from '@app/api/search/SearchReindexAllRun';
import {backfillThreadSearch} from '@app/api/search/thread/ThreadSearchService';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {indexChannelMessageHistory} from '@app/api/worker/tasks/IndexChannelMessages';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {
	JobCancelledError,
	JobFailedError,
	type WorkerTaskHandler,
	type WorkerTaskHelpers,
} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const INDEX_TYPES = [
	'guilds',
	'users',
	'reports',
	'audit_logs',
	'channel_messages',
	'guild_members',
	'discovery',
	'threads',
	'all',
] as const;

type IndexType = (typeof INDEX_TYPES)[number];

const PayloadSchema = z
	.object({
		index_type: z.enum(INDEX_TYPES),
		job_id: z.string(),
		admin_user_id: z.string(),
		guild_id: z.string().optional(),
	})
	.refine(
		(data) => {
			if (
				data.index_type === 'channel_messages' ||
				data.index_type === 'guild_members' ||
				data.index_type === 'threads'
			) {
				return data.guild_id !== undefined;
			}
			return true;
		},
		{message: ValidationErrorCodes.GUILD_ID_REQUIRED_FOR_SEARCH_INDEX},
	);

type RefreshSearchIndexPayload = z.infer<typeof PayloadSchema>;

const BATCH_SIZE = 1000;
const CHANNEL_PAGE_SIZE = 500;
const CHANNEL_CONCURRENCY = 2;

function requireSearchService<T>(service: T | null): T {
	if (!service) {
		throw new Error('Search is not enabled');
	}
	return service;
}

async function setProgress(kvClient: IKVProvider, progressKey: string, data: Record<string, unknown>): Promise<void> {
	await kvClient.set(progressKey, JSON.stringify(data), 'EX', INDEX_REFRESH_STATUS_TTL_SECONDS);
}

type ReportProgress = (indexed: number, total?: number, phase?: IndexType | 'messages') => Promise<void>;

const ignoreProgress: ReportProgress = async () => {};

interface PaginateAndIndexOptions<TCursor, TItem> {
	fetchPage: (cursor: TCursor | undefined) => Promise<Array<TItem>>;
	indexBatch: (items: Array<TItem>) => Promise<number>;
	getCursor: (item: TItem) => TCursor;
	label: string;
	report: ReportProgress;
}

async function paginateAndIndex<TCursor, TItem>(options: PaginateAndIndexOptions<TCursor, TItem>): Promise<number> {
	let cursor: TCursor | undefined;
	let indexedCount = 0;
	let hasMore = true;
	while (hasMore) {
		const items = await options.fetchPage(cursor);
		if (items.length > 0) {
			const batchCount = await options.indexBatch(items);
			indexedCount += batchCount;
			cursor = options.getCursor(items[items.length - 1]!);
			if (batchCount > 0) {
				await options.report(indexedCount);
				Logger.debug({count: batchCount, total: indexedCount}, `Indexed ${options.label} batch`);
			}
		}
		hasMore = items.length === BATCH_SIZE;
	}
	Logger.debug({count: indexedCount}, `Refreshed ${options.label} search index`);
	return indexedCount;
}

type IndexHandler = (
	payload: RefreshSearchIndexPayload,
	helpers: WorkerTaskHelpers,
	report: ReportProgress,
) => Promise<number>;

const refreshGuilds: IndexHandler = async (_payload, _helpers, report) => {
	const {guildRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getGuildSearchService());
	await searchService.deleteAllDocuments();
	return paginateAndIndex({
		fetchPage: (cursor?: GuildID) => guildRepository.listAllGuildsPaginated(BATCH_SIZE, cursor),
		indexBatch: async (guilds) => {
			await searchService.indexGuilds(guilds);
			return guilds.length;
		},
		getCursor: (guild) => guild.id,
		label: 'guild',
		report,
	});
};
const refreshUsers: IndexHandler = async (_payload, _helpers, report) => {
	const {userRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getUserSearchService());
	await searchService.deleteAllDocuments();
	let pageState: string | null = null;
	let indexedCount = 0;
	do {
		const page = await userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		pageState = page.pageState;
		const users = page.users;
		if (users.length === 0) continue;
		await searchService.indexUsers(users);
		indexedCount += users.length;
		await report(indexedCount);
		Logger.debug({count: users.length, total: indexedCount}, 'Indexed user batch');
	} while (pageState);
	Logger.debug({count: indexedCount}, 'Refreshed user search index');
	return indexedCount;
};
const refreshReports: IndexHandler = async (_payload, _helpers, report) => {
	const {reportRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getReportSearchService());
	await searchService.deleteAllDocuments();
	return paginateAndIndex({
		fetchPage: (cursor?: ReportID) => reportRepository.listAllReportsPaginated(BATCH_SIZE, cursor),
		indexBatch: async (reports) => {
			await searchService.indexReports(reports);
			return reports.length;
		},
		getCursor: (report) => report.reportId,
		label: 'report',
		report,
	});
};
const refreshAuditLogs: IndexHandler = async (_payload, _helpers, report) => {
	const {adminRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getAuditLogSearchService());
	await searchService.deleteAllDocuments();
	return paginateAndIndex({
		fetchPage: (cursor?: bigint) => adminRepository.listAllAuditLogsPaginated(BATCH_SIZE, cursor),
		indexBatch: async (logs) => {
			await searchService.indexAuditLogs(logs);
			return logs.length;
		},
		getCursor: (log) => log.logId,
		label: 'audit log',
		report,
	});
};
const refreshChannelMessages: IndexHandler = async (payload, helpers) => {
	const {channelRepository, kvClient} = getWorkerDependencies();
	const guildId = createGuildID(BigInt(payload.guild_id!));
	const searchService = requireSearchService<IMessageSearchService>(getMessageSearchService());
	const guildChannels = await channelRepository.channelData.listGuildChannels(guildId, 'complete');
	const threadIds = await channelRepository.threads.listGuildThreadIds(guildId, {parents: guildChannels});
	const channels = [...guildChannels.map((channel) => ({id: channel.id})), ...threadIds.map((id) => ({id}))];
	if (channels.length === 0) {
		return 0;
	}
	const indexedChannels = channels;
	for (const channel of indexedChannels) {
		await deleteChannelMessageSearchDocuments(channel.id, {
			searchService,
			context: {source: 'bulk_reindex', guildId: guildId.toString()},
		});
	}
	const completionKey = `bulk_reindex:${payload.job_id}:remaining`;
	await kvClient.del(completionKey);
	for (const channel of indexedChannels) {
		Logger.debug({channelId: channel.id.toString()}, 'Queuing bulk channel indexing');
		await helpers.addJob(
			'indexChannelMessages',
			{
				channelId: channel.id.toString(),
				completionKey,
				channelCount: indexedChannels.length,
			},
			{
				jobKey: `index-channel-${channel.id}-bulk`,
				maxAttempts: 3,
			},
		);
	}
	Logger.info(
		{channels: indexedChannels.length, guildId: guildId.toString()},
		'Queued bulk channel message indexing jobs',
	);
	return indexedChannels.length;
};
const refreshGuildMembers: IndexHandler = async (payload, _helpers, report) => {
	const {guildRepository, userRepository} = getWorkerDependencies();
	const guildId = createGuildID(BigInt(payload.guild_id!));
	const searchService = requireSearchService<IGuildMemberSearchService>(getGuildMemberSearchService());
	await searchService.deleteGuildMembers(guildId);
	const indexedCount = await paginateAndIndex({
		fetchPage: (cursor?: UserID) => guildRepository.listMembersPaginated(guildId, BATCH_SIZE, cursor),
		indexBatch: async (members) => {
			const uniqueUserIds = Array.from(new Set(members.map((m) => m.userId)));
			const users = await userRepository.listUsers(uniqueUserIds);
			const userMap = new Map<UserID, User>(users.map((u) => [u.id, u]));
			const membersWithUsers = members.flatMap((member) => {
				const user = userMap.get(member.userId);
				return user ? [{member, user}] : [];
			});
			if (membersWithUsers.length > 0) {
				await searchService.indexMembers(membersWithUsers);
			}
			return membersWithUsers.length;
		},
		getCursor: (member) => member.userId,
		label: 'guild member',
		report,
	});
	const guild = await guildRepository.findUnique(guildId);
	if (guild) {
		await guildRepository.upsertPartial(guildId, {members_indexed_at: new Date()}, guild.toRow());
	}
	return indexedCount;
};
const DISCOVERY_BATCH_SIZE = 200;
const refreshDiscovery: IndexHandler = async (_payload, _helpers, report) => {
	const {guildRepository} = getWorkerDependencies();
	const searchService = requireSearchService(getGuildSearchService());
	const discoveryRepository = new GuildDiscoveryRepository();
	const approvedRows = await discoveryRepository.listByStatus(DiscoveryApplicationStatus.APPROVED);
	if (approvedRows.length === 0) {
		return 0;
	}
	const guildIds = approvedRows.map((row) => row.guild_id);
	let synced = 0;
	for (let i = 0; i < guildIds.length; i += DISCOVERY_BATCH_SIZE) {
		const batch = guildIds.slice(i, i + DISCOVERY_BATCH_SIZE);
		const [guilds, discoveryRows] = await Promise.all([
			guildRepository.listGuilds(batch),
			Promise.all(batch.map((guildId) => discoveryRepository.findByGuildId(guildId))),
		]);
		const guildMap = new Map(guilds.map((g) => [g.id.toString(), g]));
		const discoveryMap = new Map(batch.map((guildId, idx) => [guildId.toString(), discoveryRows[idx]] as const));
		const updates: Array<Promise<void>> = [];
		for (const guildId of batch) {
			const guild = guildMap.get(guildId.toString());
			if (!guild) continue;
			const discoveryRow = discoveryMap.get(guildId.toString());
			if (!discoveryRow || discoveryRow.status !== DiscoveryApplicationStatus.APPROVED) continue;
			updates.push(
				searchService.updateGuild(guild, {
					description: discoveryRow.description,
					categoryId: discoveryRow.category_type,
					primaryLanguage: discoveryRow.primary_language ?? null,
					tags: discoveryRow.custom_tags ?? [],
				}),
			);
		}
		await Promise.all(updates);
		synced += updates.length;
		await report(synced, guildIds.length);
	}
	return synced;
};
const refreshThreads: IndexHandler = async (payload) => {
	const {channelRepository} = getWorkerDependencies();
	const guildId = createGuildID(BigInt(payload.guild_id!));
	requireSearchService(getThreadSearchService());
	await channelRepository.threads.clearGuildSearchBackfilled(guildId);
	return backfillThreadSearch(channelRepository, guildId);
};
async function forEachGuild(visit: (guildId: GuildID) => Promise<void>): Promise<void> {
	const {guildRepository} = getWorkerDependencies();
	let cursor: GuildID | undefined;
	while (true) {
		const guilds = await guildRepository.listAllGuildsPaginated(BATCH_SIZE, cursor);
		for (const guild of guilds) {
			await visit(guild.id);
		}
		if (guilds.length < BATCH_SIZE) return;
		cursor = guilds[guilds.length - 1]!.id;
	}
}

async function forEachChannelWithMessages(visit: (channelIds: Array<ChannelID>) => Promise<void>): Promise<void> {
	const repository = new AttachmentDecayRepository();
	let pageState: string | null = null;
	let previous: ChannelID | null = null;
	do {
		const page = await repository.scanChannelsWithMessagesPage(CHANNEL_PAGE_SIZE, pageState);
		pageState = page.pageState;
		const channelIds = page.channelIds.filter((channelId) => channelId !== previous);
		previous = page.channelIds.at(-1) ?? previous;
		if (channelIds.length > 0) {
			await visit(channelIds);
		}
	} while (pageState);
}

async function throwIfCancelled(helpers: WorkerTaskHelpers): Promise<void> {
	if (await helpers.shouldCancel()) {
		throw new JobCancelledError();
	}
}

async function refreshEveryGuild(
	handler: IndexHandler,
	payload: RefreshSearchIndexPayload,
	helpers: WorkerTaskHelpers,
	guildCount: number,
	report: ReportProgress,
): Promise<number> {
	let visited = 0;
	let indexed = 0;
	await report(0, guildCount);
	await forEachGuild(async (guildId) => {
		indexed += await handler({...payload, guild_id: guildId.toString()}, helpers, ignoreProgress);
		visited++;
		await report(visited, Math.max(guildCount, visited));
		await throwIfCancelled(helpers);
	});
	return indexed;
}

async function refreshEveryChannel(
	searchService: IMessageSearchService,
	helpers: WorkerTaskHelpers,
	report: ReportProgress,
): Promise<number> {
	await searchService.deleteAllDocuments();
	let channelCount = 0;
	await forEachChannelWithMessages(async (channelIds) => {
		channelCount += channelIds.length;
	});
	let visited = 0;
	let indexed = 0;
	await report(0, channelCount);
	await forEachChannelWithMessages(async (channelIds) => {
		const counts = await mapWithConcurrency(channelIds, CHANNEL_CONCURRENCY, async (channelId) => {
			const count = await indexChannelMessageHistory(searchService, channelId);
			await searchService.refreshIndex().catch((error) => {
				Logger.warn({error, channelId: channelId.toString()}, 'Search refresh after channel reindex failed');
			});
			return count;
		});
		indexed += counts.reduce((sum, count) => sum + count, 0);
		visited += channelIds.length;
		await report(visited, Math.max(channelCount, visited));
		await throwIfCancelled(helpers);
	});
	return indexed;
}

const refreshAll: IndexHandler = async (payload, helpers, report) => {
	let indexed = 0;
	let guildCount = 0;
	const globalHandlers = [
		['users', refreshUsers],
		['guilds', refreshGuilds],
		['discovery', refreshDiscovery],
		['reports', refreshReports],
		['audit_logs', refreshAuditLogs],
	] as const;
	for (const [indexType, handler] of globalHandlers) {
		const count = await handler(payload, helpers, (done, total) => report(done, total, indexType));
		if (indexType === 'guilds') {
			guildCount = count;
		}
		indexed += count;
		await throwIfCancelled(helpers);
	}
	const memberSearch = getGuildMemberSearchService();
	if (memberSearch) {
		await memberSearch.deleteAllDocuments();
		indexed += await refreshEveryGuild(refreshGuildMembers, payload, helpers, guildCount, (done, total) =>
			report(done, total, 'guild_members'),
		);
	}
	const threadSearch = getThreadSearchService();
	if (threadSearch) {
		await threadSearch.deleteAllDocuments();
		indexed += await refreshEveryGuild(refreshThreads, payload, helpers, guildCount, (done, total) =>
			report(done, total, 'threads'),
		);
	}
	const messageSearch = getMessageSearchService();
	if (messageSearch) {
		indexed += await refreshEveryChannel(messageSearch, helpers, (done, total) => report(done, total, 'messages'));
	}
	return indexed;
};
const INDEX_HANDLERS: Record<IndexType, IndexHandler> = {
	guilds: refreshGuilds,
	users: refreshUsers,
	reports: refreshReports,
	audit_logs: refreshAuditLogs,
	channel_messages: refreshChannelMessages,
	guild_members: refreshGuildMembers,
	discovery: refreshDiscovery,
	threads: refreshThreads,
	all: refreshAll,
};
async function runRefresh(
	payload: RefreshSearchIndexPayload,
	helpers: WorkerTaskHelpers,
	kvClient: IKVProvider,
): Promise<number> {
	const progressKey = indexRefreshStatusKey(payload.job_id);
	const indexType = payload.index_type;
	const report: ReportProgress = async (indexed, total = indexed, phase) => {
		if (indexType === 'all') {
			await holdSearchReindexAll(kvClient, payload.job_id);
		}
		await setProgress(kvClient, progressKey, {
			status: 'in_progress',
			index_type: indexType,
			...(phase ? {phase} : {}),
			total,
			indexed,
			started_at: new Date().toISOString(),
		});
	};
	await report(0);
	try {
		const indexedCount = await INDEX_HANDLERS[indexType](payload, helpers, report);
		await setProgress(kvClient, progressKey, {
			status: 'completed',
			index_type: indexType,
			total: indexedCount,
			indexed: indexedCount,
			completed_at: new Date().toISOString(),
		});
		return indexedCount;
	} catch (error) {
		Logger.error({error, payload}, 'Failed to refresh search index');
		const message = error instanceof Error ? error.message : 'Unknown error';
		await setProgress(kvClient, progressKey, {
			status: 'failed',
			index_type: indexType,
			error: message,
			failed_at: new Date().toISOString(),
		});
		if (error instanceof JobCancelledError) {
			throw error;
		}
		throw new JobFailedError(message);
	}
}
const refreshSearchIndex: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing refreshSearchIndex task');
	const {kvClient} = getWorkerDependencies();
	if (validated.index_type !== 'all') {
		return {indexed: await runRefresh(validated, helpers, kvClient)};
	}
	if (!(await holdSearchReindexAll(kvClient, validated.job_id))) {
		helpers.logger.info({jobId: validated.job_id}, 'Another search reindex is running, stopping this one');
		return {superseded: true};
	}
	try {
		return {indexed: await runRefresh(validated, helpers, kvClient)};
	} finally {
		await finishSearchReindexAll(kvClient, validated.job_id);
	}
};

export default refreshSearchIndex;
