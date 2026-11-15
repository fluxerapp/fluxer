// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {requirePermission} from '@app/api/utils/PermissionUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

export class ContentHelpers {
	constructor(
		private readonly gatewayService: IGatewayService,
		public readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async checkPermission(params: {userId: UserID; guildId: GuildID; permission: bigint}) {
		const {userId, guildId, permission} = params;
		await requirePermission(this.gatewayService, {guildId, userId, permission});
	}

	async checkModifyExpressionPermission(params: {userId: UserID; guildId: GuildID; creatorId: UserID}) {
		const {userId, guildId, creatorId} = params;
		return this.checkPermission({
			userId,
			guildId,
			permission: userId === creatorId ? Permissions.CREATE_EXPRESSIONS : Permissions.MANAGE_EXPRESSIONS,
		});
	}
}
