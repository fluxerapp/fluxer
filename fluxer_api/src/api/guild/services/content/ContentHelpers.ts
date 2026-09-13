// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, StickerID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {GuildSticker} from '@app/api/models/GuildSticker';
import {serializeEmojiForAudit, serializeStickerForAudit} from '@app/api/utils/AuditSerializationUtils';
import {hasPermission, requirePermission} from '@app/api/utils/PermissionUtils';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

export class ContentHelpers {
	constructor(
		private readonly gatewayService: IGatewayService,
		public readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async getGuildData(params: {userId: UserID; guildId: GuildID}) {
		const {userId, guildId} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		return guildData;
	}

	async checkPermission(params: {userId: UserID; guildId: GuildID; permission: bigint}) {
		const {userId, guildId, permission} = params;
		await requirePermission(this.gatewayService, {guildId, userId, permission});
	}

	async checkManageExpressionsPermission(params: {userId: UserID; guildId: GuildID}) {
		return this.checkPermission({...params, permission: Permissions.MANAGE_EXPRESSIONS});
	}

	async checkCreateExpressionsPermission(params: {userId: UserID; guildId: GuildID}) {
		return this.checkPermission({...params, permission: Permissions.CREATE_EXPRESSIONS});
	}

	async checkModifyExpressionPermission(params: {userId: UserID; guildId: GuildID; creatorId: UserID}) {
		const {userId, guildId, creatorId} = params;
		if (userId === creatorId) {
			return this.checkCreateExpressionsPermission({userId, guildId});
		}
		return this.checkManageExpressionsPermission({userId, guildId});
	}

	async hasManageExpressionsPermission(params: {userId: UserID; guildId: GuildID}): Promise<boolean> {
		const {userId, guildId} = params;
		return hasPermission(this.gatewayService, {guildId, userId, permission: Permissions.MANAGE_EXPRESSIONS});
	}

	serializeEmojiForAudit(emoji: GuildEmoji): Record<string, unknown> {
		return serializeEmojiForAudit(emoji);
	}

	serializeStickerForAudit(sticker: GuildSticker): Record<string, unknown> {
		return serializeStickerForAudit(sticker);
	}

	async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: GuildID | EmojiID | StickerID | string | null;
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
