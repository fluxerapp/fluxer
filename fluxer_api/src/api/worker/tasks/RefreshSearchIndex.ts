// SPDX-License-Identifier: AGPL-3.0-or-later

import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {seconds} from 'itty-time';
import {z} from 'zod';
import type {GuildID, ReportID, UserID} from '../../BrandedTypes';
import {createGuildID} from '../../BrandedTypes';
import {GuildDiscoveryRepository} from '../../guild/repositories/GuildDiscoveryRepository';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import {
	getAuditLogSearchService,
	getGuildMemberSearchService,
	getGuildSearchService,
	getMessageSearchService,
	getReportSearchService,
	getUserSearchService,
} from '../../SearchFactory';
import type {IGuildMemberSearchService} from '../../search/IGuildMemberSearchService';
import type {IMessageSearchService} from '../../search/IMessageSearchService';
import {deleteChannelMessageSearchDocuments} from '../../search/MessageSearchIndexCleanup';
import {getWorkerDependencies} from '../WorkerContext';

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
		{message: ValidationErrorCodes.GUILD_ID_REQUIRED_FOR_SEARCH_INDEX},
	);

type RefreshSearchIndexPayload = z.infer<typeof PayloadSchema>;

const BATCH_SIZE = 1000;
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
	let pageState: string | null = null;
	let indexedCount = 0;
	while (true) {
		const page = await userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		const users = page.users;
		if (users.length === 0) {
			break;
		}
		await searchService.indexUsers(users);
		indexedCount += users.length;
		await reportInProgress(kvClient, progressKey, 'users', indexedCount);
		Logger.debug({count: users.length, total: indexedCount}, 'Indexed user batch');
		pageState = page.pageState;
		if (!pageState) {
			break;
		}
	}
	Logger.debug({count: indexedCount}, 'Refreshed user search index');
	return indexedCount;
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
const refreshChannelMessages: IndexHandler = async (payload, helpers, kvClient, _progressKey) => {
	const {channelRepository} = getWorkerDependencies();
	const guildId = createGuildID(BigInt(payload.guild_id!));
	const searchService = requireSearchService<IMessageSearchService>(getMessageSearchService());
	const channels = await channelRepository.listGuildChannels(guildId);
	if (channels.length === 0) {
		return 0;
	}
	for (const channel of channels) {
		await deleteChannelMessageSearchDocuments(channel.id, {
			searchService,
			context: {source: 'bulk_reindex', guildId: guildId.toString()},
		});
	}
	const completionKey = `bulk_reindex:${payload.job_id}:remaining`;
	await kvClient.del(completionKey);
	for (const channel of channels) {
		Logger.debug({channelId: channel.id.toString()}, 'Queuing bulk channel indexing');
		await helpers.addJob(
			'indexChannelMessages',
			{
				channelId: channel.id.toString(),
				completionKey,
				channelCount: channels.length,
			},
			{
				jobKey: `index-channel-${channel.id}-bulk`,
				maxAttempts: 3,
			},
		);
	}
	Logger.info({channels: channels.length, guildId: guildId.toString()}, 'Queued bulk channel message indexing jobs');
	return channels.length;
};
const refreshGuildMembers: IndexHandler = async (payload, _helpers, kvClient, progressKey) => {
	const {guildRepository, userRepository} = getWorkerDependencies();
	const guildId = createGuildID(BigInt(payload.guild_id!));
	const searchService = requireSearchService<IGuildMemberSearchService>(getGuildMemberSearchService());
	await searchService.deleteGuildMembers(guildId);
	const indexedCount = await paginateAndIndex({
		fetchPage: async (cursor?: UserID) => {
			const members = await guildRepository.listMembersPaginated(guildId, BATCH_SIZE, cursor);
			const uniqueUserIds = Array.from(new Set(members.map((m) => m.userId)));
			const users = await userRepository.listUsers(uniqueUserIds);
			const userMap = new Map<UserID, User>(users.map((u) => [u.id, u]));
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
		await guildRepository.upsertPartial(guildId, {members_indexed_at: new Date()}, guild.toRow());
	}
	return indexedCount;
};
const DISCOVERY_BATCH_SIZE = 200;
const refreshDiscovery: IndexHandler = async (_payload, _helpers, kvClient, progressKey) => {
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
