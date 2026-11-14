// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {mapGuildToGuildResponse} from '@app/api/guild/GuildModel';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getGuildAuth} from '@app/api/guild/services/GuildMfaEnforcement';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Guild} from '@app/api/models/Guild';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {serializeGuildForAudit} from '@app/api/utils/AuditSerializationUtils';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export class GuildDataHelpers {
	constructor(
		private readonly gatewayService: IGatewayService,
		public readonly guildAuditLogService: GuildAuditLogService,
		private readonly userRepository: UserRepository,
	) {}

	private readonly guildRepository = new GuildRepository();

	async getGuildAuthenticated(params: {userId: UserID; guildId: GuildID}) {
		const guildData = await this.getGuildData(params);
		return getGuildAuth(this.gatewayService, this.userRepository, {...params, guildData});
	}

	async getGuildData({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildResponse> {
		try {
			const guildData = await this.gatewayService.getGuildData({guildId, userId});
			if (!guildData) throw new UnknownGuildError();
			return guildData;
		} catch (error) {
			if (error instanceof UnknownGuildError && (await this.guildRepository.findUnique(guildId)))
				throw new AccessDeniedError();
			throw error;
		}
	}

	computeGuildChanges(
		previousSnapshot: Record<string, unknown>,
		guild: Guild,
		keys: ReadonlySet<string>,
	): GuildAuditLogChange {
		const currentSnapshot = serializeGuildForAudit(guild);
		const changes = this.guildAuditLogService.computeChanges(previousSnapshot, currentSnapshot);
		return changes.filter((change) => keys.has(change.key));
	}

	async dispatchGuildUpdate(guild: Guild): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId: guild.id,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(guild),
		});
	}
}
