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

import {createGuildID, type GuildID, type UserID} from '~/BrandedTypes';
import {UnknownGuildError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {AdminAuditService} from '../AdminAuditService';

interface AdminGuildManagementServiceDeps {
	guildRepository: IGuildRepository;
	gatewayService: IGatewayService;
	guildService: GuildService;
	auditService: AdminAuditService;
}

export class AdminGuildManagementService {
	constructor(private readonly deps: AdminGuildManagementServiceDeps) {}

	async reloadGuild(guildIdRaw: bigint, adminUserId: UserID, auditLogReason: string | null) {
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

	async shutdownGuild(guildIdRaw: bigint, adminUserId: UserID, auditLogReason: string | null) {
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

	async deleteGuild(guildIdRaw: bigint, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(guildIdRaw);

		await guildService.deleteGuildAsAdmin(guildId, auditLogReason);

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
		const {gatewayService} = this.deps;
		return await gatewayService.getGuildMemoryStats(limit);
	}

	async reloadAllGuilds(guildIds: Array<GuildID>) {
		const {gatewayService} = this.deps;
		return await gatewayService.reloadAllGuilds(guildIds);
	}

	async getNodeStats() {
		const {gatewayService} = this.deps;
		return await gatewayService.getNodeStats();
	}
}
