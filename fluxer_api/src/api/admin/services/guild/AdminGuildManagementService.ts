// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createGuildID, type GuildID, type UserID} from '../../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../../../guild/services/GuildService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {AdminAuditService} from '../AdminAuditService';

interface AdminGuildManagementServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
	gatewayService: IGatewayService;
	guildService: GuildService;
	auditService: AdminAuditService;
}

export class AdminGuildManagementService {
	constructor(private readonly deps: AdminGuildManagementServiceDeps) {}

	async reloadGuild(guildIdRaw: bigint, adminUserId: UserID, auditLogReason: string | null): Promise<SuccessResponse> {
		const {guildRepository, gatewayService, auditService} = this.deps;
		const guildId = createGuildID(guildIdRaw);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		await gatewayService.reloadGuild(guildId);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: guildIdRaw,
			action: 'reload_guild',
			auditLogReason,
			metadata: new Map([['guild_id', guildIdRaw.toString()]]),
		});
		return {success: true};
	}

	async shutdownGuild(
		guildIdRaw: bigint,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<SuccessResponse> {
		const {guildRepository, gatewayService, auditService} = this.deps;
		const guildId = createGuildID(guildIdRaw);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		await gatewayService.shutdownGuild(guildId);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: guildIdRaw,
			action: 'shutdown_guild',
			auditLogReason,
			metadata: new Map([['guild_id', guildIdRaw.toString()]]),
		});
		return {success: true};
	}

	async deleteGuild(guildIdRaw: bigint, adminUserId: UserID, auditLogReason: string | null): Promise<SuccessResponse> {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(guildIdRaw);
		await guildService.data.deleteGuildForAdmin(guildId, auditLogReason);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: guildIdRaw,
			action: 'delete_guild',
			auditLogReason,
			metadata: new Map([['guild_id', guildIdRaw.toString()]]),
		});
		return {success: true};
	}

	async getGuildMemoryStats(limit: number) {
		const {gatewayService, guildRepository} = this.deps;
		const memoryStats = await gatewayService.getGuildMemoryStats(limit);
		const guildIds = memoryStats.guilds
			.map((guild) => guild.guild_id)
			.filter((guildId): guildId is string => guildId !== null)
			.map((guildId) => createGuildID(BigInt(guildId)));
		const guilds = await guildRepository.listGuilds(guildIds);
		const guildNsfwLevels = new Map(guilds.map((guild) => [guild.id.toString(), guild.nsfwLevel]));
		return {
			guilds: memoryStats.guilds.map((guild) => ({
				...guild,
				nsfw_level: guild.guild_id ? (guildNsfwLevels.get(guild.guild_id) ?? null) : null,
			})),
		};
	}

	async reloadAllGuilds(guildIds: Array<GuildID>) {
		const {gatewayService} = this.deps;
		return gatewayService.reloadAllGuilds(guildIds);
	}

	async getNodeStats() {
		const {gatewayService} = this.deps;
		return gatewayService.getNodeStats();
	}

	async getVoiceStateCounts() {
		const {gatewayService} = this.deps;
		return gatewayService.getVoiceStateCounts();
	}
}
