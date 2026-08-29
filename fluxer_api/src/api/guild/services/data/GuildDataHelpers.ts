// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '../../../BrandedTypes';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import {Logger} from '../../../Logger';
import type {Guild} from '../../../models/Guild';
import type {IUserRepository} from '../../../user/IUserRepository';
import {serializeGuildForAudit as serializeGuildForAuditUtil} from '../../../utils/AuditSerializationUtils';
import {requirePermission} from '../../../utils/PermissionUtils';
import type {GuildAuditLogService} from '../../GuildAuditLogService';
import type {GuildAuditLogChange} from '../../GuildAuditLogTypes';
import {mapGuildToGuildResponse} from '../../GuildModel';
import {GuildRepository} from '../../repositories/GuildRepository';
import {createGuildMfaEnforcer} from '../GuildMfaEnforcement';

interface GuildAuth {
	guildData: GuildResponse;
	checkPermission: (permission: bigint) => Promise<void>;
}

export class GuildDataHelpers {
	constructor(
		private readonly gatewayService: IGatewayService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly userRepository: IUserRepository,
	) {}

	private readonly guildRepository = new GuildRepository();

	async getGuildAuthenticated(params: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const {userId, guildId} = params;
		try {
			const guildData = await this.gatewayService.getGuildData({guildId, userId});
			if (!guildData) throw new UnknownGuildError();
			return await this.createGuildAuth({guildData, guildId, userId});
		} catch (error) {
			if (error instanceof UnknownGuildError && (await this.guildExists(guildId))) {
				throw new AccessDeniedError();
			}
			throw error;
		}
	}

	private async createGuildAuth(params: {
		guildData: GuildResponse;
		guildId: GuildID;
		userId: UserID;
	}): Promise<GuildAuth> {
		const {guildData, guildId, userId} = params;
		const enforceGuildMfa = await createGuildMfaEnforcer({userRepository: this.userRepository, guildData, userId});
		const checkPermission = async (permission: bigint) => {
			await requirePermission(this.gatewayService, {guildId, userId, permission});
			enforceGuildMfa(permission);
		};
		return {guildData, checkPermission};
	}

	private async guildExists(guildId: GuildID): Promise<boolean> {
		const guild = await this.guildRepository.findUnique(guildId);
		return guild !== null;
	}

	serializeGuildForAudit(guild: Guild): Record<string, unknown> {
		return serializeGuildForAuditUtil(guild);
	}

	computeGuildChanges(
		previousSnapshot: Record<string, unknown> | null,
		guildOrSnapshot: Guild | Record<string, unknown> | null,
	): GuildAuditLogChange {
		const currentSnapshot = guildOrSnapshot
			? 'id' in guildOrSnapshot
				? this.serializeGuildForAudit(guildOrSnapshot as Guild)
				: guildOrSnapshot
			: null;
		return this.guildAuditLogService.computeChanges(previousSnapshot, currentSnapshot);
	}

	async dispatchGuildUpdate(guild: Guild): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId: guild.id,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(guild),
		});
	}

	async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: GuildID | ChannelID | RoleID | UserID | EmojiID | StickerID | string | null;
		auditLogReason?: string | null;
		metadata?: Map<string, string> | Record<string, string>;
		changes?: GuildAuditLogChange | null;
		createdAt?: Date;
	}): Promise<void> {
		const targetId =
			params.targetId === undefined || params.targetId === null
				? null
				: typeof params.targetId === 'string'
					? params.targetId
					: params.targetId.toString();
		try {
			const builder = this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(params.action, targetId)
				.withReason(params.auditLogReason ?? null);
			if (params.metadata) {
				builder.withMetadata(params.metadata);
			}
			if (params.changes) {
				builder.withChanges(params.changes);
			}
			if (params.createdAt) {
				builder.withCreatedAt(params.createdAt);
			}
			await builder.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.guildId.toString(),
					userId: params.userId.toString(),
					action: params.action,
					targetId,
				},
				'Failed to record guild audit log',
			);
		}
	}
}
