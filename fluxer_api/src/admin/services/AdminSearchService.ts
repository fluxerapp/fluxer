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

import {mapGuildToAdminResponse, mapUserToAdminResponse} from '~/admin/AdminModel';
import {createGuildID, createUserID, type UserID} from '~/BrandedTypes';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import {getGuildSearchService, getUserSearchService} from '~/Meilisearch';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {AdminAuditService} from './AdminAuditService';

interface RefreshSearchIndexJobPayload {
	index_type: 'guilds' | 'users' | 'reports' | 'audit_logs' | 'channel_messages' | 'favorite_memes';
	admin_user_id: string;
	audit_log_reason: string | null;
	job_id: string;
	guild_id?: string;
	user_id?: string;
}

interface AdminSearchServiceDeps {
	guildRepository: IGuildRepository;
	userRepository: IUserRepository;
	workerService: IWorkerService;
	cacheService: ICacheService;
	snowflakeService: SnowflakeService;
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
			Logger.error('[AdminSearchService] searchGuilds - Search service not enabled');
			throw new Error('Search is not enabled');
		}

		Logger.debug('[AdminSearchService] searchGuilds - Calling Meilisearch');
		const {hits, total} = await guildSearchService.searchGuilds(
			data.query || '',
			{},
			{
				limit: data.limit,
				offset: data.offset,
			},
		);

		const guildIds = hits.map((hit) => createGuildID(BigInt(hit.id)));
		Logger.debug(
			{guild_ids: guildIds.map((id) => id.toString())},
			'[AdminSearchService] searchGuilds - Fetching from DB',
		);

		const guilds = await guildRepository.listGuilds(guildIds);
		Logger.debug({guilds_count: guilds.length}, '[AdminSearchService] searchGuilds - Got guilds from DB');

		const response = guilds.map((guild) => mapGuildToAdminResponse(guild));
		Logger.debug({response_count: response.length}, '[AdminSearchService] searchGuilds - Mapped to response');

		return {
			guilds: response,
			total,
		};
	}

	async searchUsers(data: {query?: string; limit: number; offset: number}) {
		const {userRepository} = this.deps;
		const userSearchService = getUserSearchService();
		if (!userSearchService) {
			throw new Error('Search is not enabled');
		}

		const {hits, total} = await userSearchService.searchUsers(
			data.query || '',
			{},
			{
				limit: data.limit,
				offset: data.offset,
			},
		);

		const userIds = hits.map((hit) => createUserID(BigInt(hit.id)));
		const users = await userRepository.listUsers(userIds);

		const {cacheService} = this.deps;
		return {
			users: await Promise.all(users.map((user) => mapUserToAdminResponse(user, cacheService))),
			total,
		};
	}

	async refreshSearchIndex(
		data: {
			index_type: 'guilds' | 'users' | 'reports' | 'audit_logs' | 'channel_messages' | 'favorite_memes';
			guild_id?: bigint;
			user_id?: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {workerService, snowflakeService, auditService} = this.deps;
		const jobId = snowflakeService.generate().toString();

		const payload: RefreshSearchIndexJobPayload = {
			index_type: data.index_type,
			admin_user_id: adminUserId.toString(),
			audit_log_reason: auditLogReason,
			job_id: jobId,
		};

		if (data.index_type === 'channel_messages') {
			if (!data.guild_id) {
				throw new Error('guild_id is required for the channel_messages index type');
			}
			payload.guild_id = data.guild_id.toString();
		}

		if (data.index_type === 'favorite_memes') {
			if (!data.user_id) {
				throw new Error('user_id is required for favorite_memes index type');
			}
			payload.user_id = data.user_id.toString();
		}

		await workerService.addJob('refreshSearchIndex', payload, {
			jobKey: `refreshSearchIndex_${data.index_type}_${jobId}`,
			maxAttempts: 1,
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
		const {cacheService} = this.deps;
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
