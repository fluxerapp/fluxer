// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {ApiContext} from '../../ApiContext';
import {createGuildID, createUserID, type UserID} from '../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {Logger} from '../../Logger';
import {getGuildSearchService, getUserSearchService} from '../../SearchFactory';
import {mapGuildToAdminResponse} from '../models/GuildTypes';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';

interface RefreshSearchIndexJobPayload extends WorkerJobPayload {
	index_type:
		| 'guilds'
		| 'users'
		| 'reports'
		| 'audit_logs'
		| 'channel_messages'
		| 'favorite_memes'
		| 'guild_members'
		| 'discovery';
	admin_user_id: string;
	audit_log_reason: string | null;
	job_id: string;
	guild_id?: string;
	user_id?: string;
}

interface AdminSearchServiceDeps {
	apiContext: ApiContext;
	guildRepository: IGuildRepositoryAggregate;
	auditService: AdminAuditService;
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
			limit: number;
			offset: number;
		},
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const email = data.email?.trim();
		if (email) {
			const user = await userRepository.findByEmail(email);
			if (!user) {
				return {users: [], total: 0};
			}
			const response = await mapUserToAdminResponse(user, cacheService, acls);
			return {users: [response], total: 1};
		}
		const lastActiveIp = data.last_active_ip?.trim();
		if (lastActiveIp) {
			const {userIds, total} = await userRepository.listUserIdsByLastActiveIp(lastActiveIp, data.limit, data.offset);
			const users = await userRepository.listUsers(userIds);
			const usersById = new Map(users.map((user) => [user.id.toString(), user]));
			const orderedUsers = [];
			for (const userId of userIds) {
				const user = usersById.get(userId.toString());
				if (user) {
					orderedUsers.push(user);
				}
			}
			const response = await Promise.all(orderedUsers.map((user) => mapUserToAdminResponse(user, cacheService, acls)));
			return {
				users: response,
				total,
			};
		}
		const userSearchService = getUserSearchService();
		if (!userSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const query = data.query?.trim() || '';
		const isIdQuery = /^\d+$/.test(query);
		const [searchResult, directUser] = await Promise.all([
			userSearchService.search(query, {}, {limit: data.limit, offset: data.offset}),
			isIdQuery && data.offset === 0
				? userRepository.findUnique(createUserID(BigInt(query))).catch(() => null)
				: Promise.resolve(null),
		]);
		const {hits, total} = searchResult;
		const userIds = hits.map((hit) => createUserID(BigInt(hit.id)));
		const users = await userRepository.listUsers(userIds);
		const response = await Promise.all(users.map((user) => mapUserToAdminResponse(user, cacheService, acls)));
		if (directUser && data.offset === 0) {
			const directId = directUser.id.toString();
			if (!response.some((u) => u.id === directId)) {
				response.unshift(await mapUserToAdminResponse(directUser, cacheService, acls));
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
				| 'favorite_memes'
				| 'discovery';
			guild_id?: bigint;
			user_id?: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {auditService} = this.deps;
		const {worker: workerService, snowflake: snowflakeService} = this.deps.apiContext.services;
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
		if (data.index_type === 'guild_members') {
			if (!data.guild_id) {
				throw InputValidationError.create('guild_id', 'guild_id is required for the guild_members index type');
			}
			payload.guild_id = data.guild_id.toString();
		}
		if (data.index_type === 'favorite_memes') {
			if (!data.user_id) {
				throw InputValidationError.create('user_id', 'user_id is required for the favorite_memes index type');
			}
			payload.user_id = data.user_id.toString();
		}
		await workerService.addJob('refreshSearchIndex', payload, {
			jobKey: `refreshSearchIndex_${data.index_type}_${jobId}`,
			maxAttempts: 1,
			requireLedger: true,
		});
		Logger.debug({index_type: data.index_type, job_id: jobId}, 'Queued search index refresh job');
		const metadata = new Map([
			['index_type', data.index_type],
			['job_id', jobId],
		]);
		if (data.guild_id) {
			metadata.set('guild_id', data.guild_id.toString());
		}
		if (data.user_id) {
			metadata.set('user_id', data.user_id.toString());
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
			job_id: jobId,
		};
	}

	async getIndexRefreshStatus(jobId: string) {
		const {cache: cacheService} = this.deps.apiContext.services;
		const statusKey = `index_refresh_status:${jobId}`;
		const status = await cacheService.get<{
			status: 'in_progress' | 'completed' | 'failed';
			index_type: string;
			total?: number;
			indexed?: number;
			started_at?: string;
			completed_at?: string;
			failed_at?: string;
			error?: string;
		}>(statusKey);
		if (!status) {
			return {
				status: 'not_found' as const,
			};
		}
		return status;
	}
}
