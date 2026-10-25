// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapGuildToAdminResponse} from '@app/api/admin/models/GuildTypes';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {isSyntheticUserId} from '@app/api/constants/Core';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {Logger} from '@app/api/Logger';
import {getGuildSearchService, getMessageSearchService, getUserSearchService} from '@app/api/SearchFactory';
import {
	claimSearchReindexAll,
	finishSearchReindexAll,
	INDEX_REFRESH_STATUS_TTL_SECONDS,
	indexRefreshStatusKey,
} from '@app/api/search/SearchReindexAllRun';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {UserSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

interface RefreshSearchIndexJobPayload extends WorkerJobPayload {
	index_type:
		| 'guilds'
		| 'users'
		| 'reports'
		| 'audit_logs'
		| 'channel_messages'
		| 'guild_members'
		| 'discovery'
		| 'threads'
		| 'all';
	admin_user_id: string;
	audit_log_reason: string | null;
	job_id: string;
	guild_id?: string;
}

interface AdminSearchServiceDeps {
	apiContext: ApiContext;
	guildRepository: GuildRepository;
	auditService: AdminAuditService;
}

function compareUserIds(a: UserID, b: UserID): number {
	if (a === b) return 0;
	return a < b ? -1 : 1;
}

export class AdminSearchService {
	constructor(private readonly deps: AdminSearchServiceDeps) {}

	async searchGuilds(data: {query?: string; limit: number; offset: number}) {
		const {guildRepository} = this.deps;
		Logger.debug(
			{query: data.query, limit: data.limit, offset: data.offset},
			'[AdminSearchService] searchGuilds called',
		);
		const guildSearchService = getGuildSearchService();
		if (!guildSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const query = data.query?.trim() || '';
		const isIdQuery = /^\d+$/.test(query);
		Logger.debug('[AdminSearchService] searchGuilds - Calling search service');
		const [searchResult, directGuild] = await Promise.all([
			guildSearchService.searchGuilds(
				query,
				{sortBy: 'createdAt', sortOrder: 'asc'},
				{limit: data.limit, offset: data.offset},
			),
			isIdQuery && data.offset === 0
				? guildRepository.findUnique(createGuildID(BigInt(query))).catch(() => null)
				: Promise.resolve(null),
		]);
		const {hits, total} = searchResult;
		const guildIds = hits.map((hit) => createGuildID(BigInt(hit.id)));
		Logger.debug(
			{guild_ids: guildIds.map((id) => id.toString())},
			'[AdminSearchService] searchGuilds - Fetching from DB',
		);
		const guilds = await guildRepository.listGuilds(guildIds);
		Logger.debug({guilds_count: guilds.length}, '[AdminSearchService] searchGuilds - Got guilds from DB');
		const orderIndex = new Map<string, number>();
		guildIds.forEach((id, idx) => orderIndex.set(id.toString(), idx));
		guilds.sort((a, b) => (orderIndex.get(a.id.toString()) ?? 0) - (orderIndex.get(b.id.toString()) ?? 0));
		const response = guilds.map((guild) => mapGuildToAdminResponse(guild));
		if (directGuild && data.offset === 0) {
			const directId = directGuild.id.toString();
			if (!response.some((g) => g.id === directId)) {
				response.unshift(mapGuildToAdminResponse(directGuild));
			}
		}
		Logger.debug({response_count: response.length}, '[AdminSearchService] searchGuilds - Mapped to response');
		return {
			guilds: response,
			total: directGuild && !hits.some((h) => h.id === query) ? total + 1 : total,
		};
	}

	async searchUsers(
		data: {
			query?: string;
			email?: string;
			last_active_ip?: string;
			acl?: string;
			limit: number;
			offset: number;
		},
		acls: ReadonlySet<string>,
		locale?: string,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const email = data.email?.trim();
		if (email) {
			const user = await userRepository.findByEmail(email);
			if (!user) {
				return {users: [], total: 0};
			}
			const response = await mapUserToAdminResponse(user, cacheService, acls, locale);
			return {users: [response], total: 1};
		}
		const lastActiveIp = data.last_active_ip?.trim();
		if (lastActiveIp) {
			const {userIds, total} = await userRepository.listUserIdsByLastActiveIp(lastActiveIp, data.limit, data.offset);
			const response = await this.mapOrderedUsers(userIds, acls, locale);
			return {
				users: response,
				total,
			};
		}
		if (data.acl !== undefined) {
			const userIds = (await userRepository.listUserIdsWithAcl(data.acl))
				.filter((userId) => !isSyntheticUserId(userId))
				.sort(compareUserIds);
			const response = await this.mapOrderedUsers(userIds.slice(data.offset, data.offset + data.limit), acls, locale);
			return {users: response, total: userIds.length};
		}
		const userSearchService = getUserSearchService();
		if (!userSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const query = data.query?.trim() || '';
		const isBrowseAll = query === '' || query === '*';
		const searchFilters: UserSearchFilters = isBrowseAll
			? {sortBy: 'createdAt', sortOrder: 'asc'}
			: {sortBy: 'relevance'};
		const directUserId = /^\d+$/.test(query) ? createUserID(BigInt(query)) : null;
		const canResolveDirectUser = directUserId !== null && !isSyntheticUserId(directUserId) && data.offset === 0;
		const [searchResult, directUser] = await Promise.all([
			userSearchService.search(query, searchFilters, {limit: data.limit, offset: data.offset}),
			canResolveDirectUser ? userRepository.findUnique(directUserId).catch(() => null) : Promise.resolve(null),
		]);
		const {hits, total} = searchResult;
		const userIds = hits.map((hit) => createUserID(BigInt(hit.id)));
		const response = await this.mapOrderedUsers(userIds, acls, locale);
		if (directUser && data.offset === 0) {
			const directId = directUser.id.toString();
			if (!response.some((u) => u.id === directId)) {
				response.unshift(await mapUserToAdminResponse(directUser, cacheService, acls, locale));
			}
		}
		return {
			users: response,
			total: directUser && !hits.some((h) => h.id === query) ? total + 1 : total,
		};
	}

	async refreshSearchIndex(
		data: {
			index_type:
				| 'guilds'
				| 'users'
				| 'reports'
				| 'audit_logs'
				| 'channel_messages'
				| 'guild_members'
				| 'discovery'
				| 'threads'
				| 'all';
			guild_id?: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {auditService} = this.deps;
		const {snowflake: snowflakeService} = this.deps.apiContext.services;
		const jobId = (await snowflakeService.generate()).toString();
		const payload: RefreshSearchIndexJobPayload = {
			index_type: data.index_type,
			admin_user_id: adminUserId.toString(),
			audit_log_reason: auditLogReason,
			job_id: jobId,
		};
		if (data.index_type === 'channel_messages') {
			if (!data.guild_id) {
				throw InputValidationError.create('guild_id', 'guild_id is required for the channel_messages index type');
			}
			payload.guild_id = data.guild_id.toString();
		}
		if (data.index_type === 'threads') {
			if (!data.guild_id) {
				throw InputValidationError.create('guild_id', 'guild_id is required for the threads index type');
			}
			payload.guild_id = data.guild_id.toString();
		}
		if (data.index_type === 'guild_members') {
			if (!data.guild_id) {
				throw InputValidationError.create('guild_id', 'guild_id is required for the guild_members index type');
			}
			payload.guild_id = data.guild_id.toString();
		}
		let queuedJobId = jobId;
		if (data.index_type === 'all') {
			queuedJobId = await this.queueReindexAll(payload);
		} else {
			await this.queueRefresh(payload);
		}
		Logger.debug({index_type: data.index_type, job_id: queuedJobId}, 'Queued search index refresh job');
		const metadata = new Map([
			['index_type', data.index_type],
			['job_id', queuedJobId],
		]);
		if (queuedJobId !== jobId) {
			metadata.set('already_running', 'true');
		}
		if (data.guild_id) {
			metadata.set('guild_id', data.guild_id.toString());
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'search_index',
			targetId: BigInt(0),
			action: 'queue_refresh_index',
			auditLogReason,
			metadata,
		});
		return {
			success: true,
			job_id: queuedJobId,
		};
	}

	private async queueRefresh(payload: RefreshSearchIndexJobPayload): Promise<void> {
		await this.deps.apiContext.services.worker.addJob('refreshSearchIndex', payload, {
			jobKey: `refreshSearchIndex_${payload.index_type}_${payload.job_id}`,
			maxAttempts: 1,
			requireLedger: true,
		});
	}

	private async queueReindexAll(payload: RefreshSearchIndexJobPayload): Promise<string> {
		if (!getMessageSearchService()) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {kv} = this.deps.apiContext.services;
		const running = await claimSearchReindexAll(kv, payload.job_id);
		if (running) {
			return running;
		}
		const queued = {status: 'in_progress', index_type: 'all', phase: 'queued', started_at: new Date().toISOString()};
		try {
			await kv.set(
				indexRefreshStatusKey(payload.job_id),
				JSON.stringify(queued),
				'EX',
				INDEX_REFRESH_STATUS_TTL_SECONDS,
			);
			await this.queueRefresh(payload);
		} catch (error) {
			await finishSearchReindexAll(kv, payload.job_id);
			throw error;
		}
		return payload.job_id;
	}

	async getIndexRefreshStatus(jobId: string) {
		const {cache: cacheService} = this.deps.apiContext.services;
		const status = await cacheService.get<{
			status: 'in_progress' | 'completed' | 'failed';
			index_type: string;
			phase?: string;
			total?: number;
			indexed?: number;
			started_at?: string;
			completed_at?: string;
			failed_at?: string;
			error?: string;
		}>(indexRefreshStatusKey(jobId));
		if (!status) {
			return {
				status: 'not_found' as const,
			};
		}
		return status;
	}

	private async mapOrderedUsers(userIds: Array<UserID>, acls: ReadonlySet<string>, locale?: string) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const users = await userRepository.listUsers(userIds);
		const usersById = new Map(users.map((user) => [user.id.toString(), user]));
		const orderedUsers = [];
		for (const userId of userIds) {
			const user = usersById.get(userId.toString());
			if (user) orderedUsers.push(user);
		}
		return Promise.all(orderedUsers.map((user) => mapUserToAdminResponse(user, cacheService, acls, locale)));
	}
}
