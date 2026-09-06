// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../../BrandedTypes';
import {createChannelID, createGuildID, createUserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import {Logger} from '../../Logger';
import type {Channel} from '../../models/Channel';
import type {Guild} from '../../models/Guild';
import type {User} from '../../models/User';
import {getAuditLogSearchService} from '../../SearchFactory';
import type {IUserRepository} from '../../user/IUserRepository';
import type {AdminAuditLog, IAdminRepository} from '../IAdminRepository';

interface CreateAdminAuditLogParams {
	adminUserId: UserID;
	targetType: string;
	targetId: bigint;
	action: string;
	auditLogReason: string | null;
	metadata?: Map<string, string>;
}

export class AdminAuditService {
	constructor(
		private readonly adminRepository: IAdminRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly enrichmentDeps: AuditLogEnrichmentDeps = {},
	) {}

	async createAuditLog({
		adminUserId,
		targetType,
		targetId,
		action,
		auditLogReason,
		metadata = new Map(),
	}: CreateAdminAuditLogParams): Promise<void> {
		const log = await this.adminRepository.createAuditLog({
			log_id: await this.snowflakeService.generate(),
			admin_user_id: adminUserId,
			target_type: targetType,
			target_id: targetId,
			action,
			audit_log_reason: auditLogReason ?? null,
			metadata,
			created_at: new Date(),
		});
		const auditLogSearchService = getAuditLogSearchService();
		if (auditLogSearchService && 'indexAuditLog' in auditLogSearchService) {
			auditLogSearchService.indexAuditLog(log).catch((error) => {
				Logger.error({error, logId: log.logId}, 'Failed to index audit log to search');
			});
		}
	}

	async getAuditLog(logId: bigint): Promise<AdminAuditLogResponse | null> {
		const log = await this.adminRepository.getAuditLog(logId);
		if (!log) {
			return null;
		}
		const [response] = await this.toResponses([log]);
		return response;
	}

	async listAuditLogs(data: {
		admin_user_id?: bigint;
		target_type?: string;
		target_id?: string;
		limit?: number;
		offset?: number;
	}): Promise<{
		logs: Array<AdminAuditLogResponse>;
		total: number;
	}> {
		const auditLogSearchService = getAuditLogSearchService();
		const targetIdBigInt = data.target_id ? BigInt(data.target_id) : undefined;
		if (!auditLogSearchService || !auditLogSearchService.isAvailable()) {
			return this.listAuditLogsFromDatabase({
				adminUserId: data.admin_user_id,
				targetType: data.target_type,
				targetId: targetIdBigInt,
				limit: data.limit,
				offset: data.offset,
			});
		}
		const limit = data.limit || 50;
		const filters: Record<string, string> = {};
		if (data.admin_user_id) {
			filters['adminUserId'] = data.admin_user_id.toString();
		}
		if (data.target_type) {
			filters['targetType'] = data.target_type;
		}
		if (data.target_id) {
			filters['targetId'] = data.target_id;
		}
		const {hits, total} = await auditLogSearchService.searchAuditLogs('', filters, {
			limit,
			offset: data.offset || 0,
		});
		const orderedLogs = await this.loadLogsInSearchOrder(hits.map((hit) => BigInt(hit.logId)));
		return {
			logs: await this.toResponses(orderedLogs),
			total,
		};
	}

	async searchAuditLogs(data: {
		query?: string;
		admin_user_id?: bigint;
		target_type?: string;
		target_id?: string;
		sort_by?: 'createdAt' | 'relevance';
		sort_order?: 'asc' | 'desc';
		limit?: number;
		offset?: number;
	}): Promise<{
		logs: Array<AdminAuditLogResponse>;
		total: number;
	}> {
		const auditLogSearchService = getAuditLogSearchService();
		const targetIdBigInt = data.target_id ? BigInt(data.target_id) : undefined;
		if (!auditLogSearchService || !auditLogSearchService.isAvailable()) {
			return this.listAuditLogsFromDatabase({
				adminUserId: data.admin_user_id,
				targetType: data.target_type,
				targetId: targetIdBigInt,
				limit: data.limit,
				offset: data.offset,
			});
		}
		const filters: Record<string, string> = {};
		if (data.admin_user_id) {
			filters['adminUserId'] = data.admin_user_id.toString();
		}
		if (data.target_id) {
			filters['targetId'] = data.target_id;
		}
		if (data.target_type) {
			filters['targetType'] = data.target_type;
		}
		if (data.sort_by) {
			filters['sortBy'] = data.sort_by;
		}
		if (data.sort_order) {
			filters['sortOrder'] = data.sort_order;
		}
		const {hits, total} = await auditLogSearchService.searchAuditLogs(data.query || '', filters, {
			limit: data.limit || 50,
			offset: data.offset || 0,
		});
		const orderedLogs = await this.loadLogsInSearchOrder(hits.map((hit) => BigInt(hit.logId)));
		return {
			logs: await this.toResponses(orderedLogs),
			total,
		};
	}

	private async listAuditLogsFromDatabase(data: {
		adminUserId?: bigint;
		targetType?: string;
		targetId?: bigint;
		limit?: number;
		offset?: number;
	}): Promise<{
		logs: Array<AdminAuditLogResponse>;
		total: number;
	}> {
		const limit = data.limit || 50;
		const allLogs = await this.adminRepository.listAllAuditLogsPaginated(limit + (data.offset || 0));
		let filteredLogs = allLogs;
		if (data.adminUserId) {
			filteredLogs = filteredLogs.filter((log) => log.adminUserId === data.adminUserId);
		}
		if (data.targetType) {
			filteredLogs = filteredLogs.filter((log) => log.targetType === data.targetType);
		}
		if (data.targetId) {
			filteredLogs = filteredLogs.filter((log) => log.targetId === data.targetId);
		}
		const offset = data.offset || 0;
		const paginatedLogs = filteredLogs.slice(offset, offset + limit);
		return {
			logs: await this.toResponses(paginatedLogs),
			total: filteredLogs.length,
		};
	}

	private async loadLogsInSearchOrder(logIds: Array<bigint>): Promise<Array<AdminAuditLog>> {
		const logs = await this.adminRepository.listAuditLogsByIds(logIds);
		const logMap = new Map(logs.map((log) => [log.logId.toString(), log]));
		const result: Array<AdminAuditLog> = [];
		for (const logId of logIds) {
			const log = logMap.get(logId.toString());
			if (log) {
				result.push(log);
			}
		}
		return result;
	}

	private async toResponses(logs: Array<AdminAuditLog>): Promise<Array<AdminAuditLogResponse>> {
		const enrichment = await this.loadEnrichment(logs);
		return logs.map((log) => this.toResponse(log, enrichment));
	}

	private toResponse(log: AdminAuditLog, enrichment: AuditLogEnrichment): AdminAuditLogResponse {
		const adminUserId = log.adminUserId.toString();
		const targetId = log.targetId.toString();
		const targetUser = this.targetUserForLog(log, enrichment);
		const targetGuild = log.targetType === 'guild' ? enrichment.guilds.get(targetId) : null;
		const targetChannel = this.targetChannelForLog(log, enrichment);
		return {
			log_id: log.logId.toString(),
			admin_user_id: adminUserId,
			admin_user: mapUserSummary(enrichment.users.get(adminUserId) ?? null),
			target_type: log.targetType,
			target_id: targetId,
			target_user: mapUserSummary(targetUser),
			target_guild: mapGuildSummary(targetGuild ?? null),
			target_channel: mapChannelSummary(targetChannel ?? null),
			related_users: Object.fromEntries(
				[...enrichment.users.entries()].map(([id, user]) => [id, mapUserSummary(user)!]),
			),
			related_guilds: Object.fromEntries(
				[...enrichment.guilds.entries()].map(([id, guild]) => [id, mapGuildSummary(guild)!]),
			),
			related_channels: Object.fromEntries(
				[...enrichment.channels.entries()].map(([id, channel]) => [id, mapChannelSummary(channel)!]),
			),
			action: log.action,
			audit_log_reason: log.auditLogReason,
			metadata: Object.fromEntries(log.metadata),
			created_at: log.createdAt.toISOString(),
		};
	}

	private targetUserForLog(log: AdminAuditLog, enrichment: AuditLogEnrichment): User | null {
		const targetId = log.targetId.toString();
		if (USER_TARGET_TYPES.has(log.targetType)) {
			return enrichment.users.get(targetId) ?? null;
		}
		return null;
	}

	private targetChannelForLog(log: AdminAuditLog, enrichment: AuditLogEnrichment): Channel | null {
		const targetId = log.targetId.toString();
		if (log.targetType === 'channel') {
			return enrichment.channels.get(targetId) ?? null;
		}
		const metadataChannelId = log.metadata.get('channel_id');
		return metadataChannelId ? (enrichment.channels.get(metadataChannelId) ?? null) : null;
	}

	private async loadEnrichment(logs: Array<AdminAuditLog>): Promise<AuditLogEnrichment> {
		const userIds = new Set<string>();
		const guildIds = new Set<string>();
		const channelIds = new Set<string>();

		for (const log of logs) {
			userIds.add(log.adminUserId.toString());
			const targetId = log.targetId.toString();
			if (USER_TARGET_TYPES.has(log.targetType)) userIds.add(targetId);
			if (log.targetType === 'guild') guildIds.add(targetId);
			if (log.targetType === 'channel') channelIds.add(targetId);

			for (const [key, value] of log.metadata) {
				if (isSnowflakeString(value)) {
					if (isUserIdKey(key)) userIds.add(value);
					if (isGuildIdKey(key)) guildIds.add(value);
					if (isChannelIdKey(key)) channelIds.add(value);
				}
			}
		}

		const [users, guilds, channels] = await Promise.all([
			this.loadUsers(userIds),
			this.loadGuilds(guildIds),
			this.loadChannels(channelIds),
		]);
		return {users, guilds, channels};
	}

	private async loadUsers(ids: Set<string>): Promise<Map<string, User>> {
		const {userRepository} = this.enrichmentDeps;
		if (!userRepository) return new Map();
		const entries = await Promise.all(
			[...ids].map(async (id) => {
				const user = await userRepository.findUnique(createUserID(BigInt(id)));
				return [id, user] as const;
			}),
		);
		return new Map(entries.filter((entry): entry is readonly [string, User] => entry[1] !== null));
	}

	private async loadGuilds(ids: Set<string>): Promise<Map<string, Guild>> {
		const {guildRepository} = this.enrichmentDeps;
		if (!guildRepository) return new Map();
		const entries = await Promise.all(
			[...ids].map(async (id) => {
				const guild = await guildRepository.findUnique(createGuildID(BigInt(id)));
				return [id, guild] as const;
			}),
		);
		return new Map(entries.filter((entry): entry is readonly [string, Guild] => entry[1] !== null));
	}

	private async loadChannels(ids: Set<string>): Promise<Map<string, Channel>> {
		const {channelRepository} = this.enrichmentDeps;
		if (!channelRepository) return new Map();
		const entries = await Promise.all(
			[...ids].map(async (id) => {
				const channel = await channelRepository.findUnique(createChannelID(BigInt(id)));
				return [id, channel] as const;
			}),
		);
		return new Map(entries.filter((entry): entry is readonly [string, Channel] => entry[1] !== null));
	}
}

interface AdminAuditLogResponse {
	log_id: string;
	admin_user_id: string;
	admin_user: AuditLogUserSummary | null;
	target_type: string;
	target_id: string;
	target_user: AuditLogUserSummary | null;
	target_guild: AuditLogGuildSummary | null;
	target_channel: AuditLogChannelSummary | null;
	related_users: Record<string, AuditLogUserSummary>;
	related_guilds: Record<string, AuditLogGuildSummary>;
	related_channels: Record<string, AuditLogChannelSummary>;
	action: string;
	audit_log_reason: string | null;
	metadata: Record<string, string>;
	created_at: string;
}

interface AuditLogEnrichmentDeps {
	userRepository?: Pick<IUserRepository, 'findUnique'>;
	guildRepository?: Pick<IGuildRepositoryAggregate, 'findUnique'>;
	channelRepository?: Pick<IChannelRepository, 'findUnique'>;
}

interface AuditLogEnrichment {
	users: Map<string, User>;
	guilds: Map<string, Guild>;
	channels: Map<string, Channel>;
}

interface AuditLogUserSummary {
	id: string;
	username: string;
	discriminator: string;
	global_name: string | null;
}

interface AuditLogGuildSummary {
	id: string;
	name: string;
}

interface AuditLogChannelSummary {
	id: string;
	name: string | null;
	type: number;
	guild_id: string | null;
}

const USER_TARGET_TYPES = new Set(['user', 'guild_member', 'message_deletion', 'message_shred']);
const SNOWFLAKE_RE = /^(0|[1-9][0-9]*)$/;

function isSnowflakeString(value: string): boolean {
	return SNOWFLAKE_RE.test(value);
}

function isUserIdKey(key: string): boolean {
	return key === 'user_id' || key === 'target_user_id' || key === 'admin_user_id' || key.endsWith('_user_id');
}

function isGuildIdKey(key: string): boolean {
	return key === 'guild_id' || key.endsWith('_guild_id');
}

function isChannelIdKey(key: string): boolean {
	return key === 'channel_id' || key.endsWith('_channel_id');
}

function mapUserSummary(user: User | null): AuditLogUserSummary | null {
	if (!user) return null;
	return {
		id: user.id.toString(),
		username: user.username,
		discriminator: String(user.discriminator).padStart(4, '0'),
		global_name: user.globalName,
	};
}

function mapGuildSummary(guild: Guild | null): AuditLogGuildSummary | null {
	if (!guild) return null;
	return {
		id: guild.id.toString(),
		name: guild.name,
	};
}

function mapChannelSummary(channel: Channel | null): AuditLogChannelSummary | null {
	if (!channel) return null;
	return {
		id: channel.id.toString(),
		name: channel.name,
		type: channel.type,
		guild_id: channel.guildId?.toString() ?? null,
	};
}
