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

import type {GuildID, UserID} from '~/BrandedTypes';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {Logger} from '~/Logger';
import type {GuildMember} from '~/Models';
import type {GuildAuditLogChange, GuildAuditLogService} from '../../GuildAuditLogService';

export class GuildMemberAuditService {
	constructor(private readonly guildAuditLogService: GuildAuditLogService) {}

	serializeMemberForAudit(member: GuildMember): Record<string, unknown> {
		const roleIds = Array.from(member.roleIds)
			.map((roleId) => roleId.toString())
			.sort();
		return {
			user_id: member.userId.toString(),
			nick: member.nickname,
			roles: roleIds,
			avatar_hash: member.avatarHash ?? null,
			banner_hash: member.bannerHash ?? null,
			bio: member.bio ?? null,
			pronouns: member.pronouns ?? null,
			accent_color: member.accentColor ?? null,
			deaf: member.isDeaf,
			mute: member.isMute,
			communication_disabled_until: member.communicationDisabledUntil
				? member.communicationDisabledUntil.toISOString()
				: null,
			temporary: member.isTemporary,
		};
	}

	async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: UserID | string | null;
		auditLogReason?: string | null;
		metadata?: Map<string, string> | Record<string, string>;
		changes?: GuildAuditLogChange | null;
	}): Promise<void> {
		const targetId =
			params.targetId === undefined || params.targetId === null
				? null
				: typeof params.targetId === 'string'
					? params.targetId
					: params.targetId.toString();
		const changes = params.action === AuditLogActionType.MEMBER_KICK ? null : (params.changes ?? null);

		try {
			const builder = this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(params.action, targetId)
				.withReason(params.auditLogReason ?? null);

			if (params.metadata) {
				builder.withMetadata(params.metadata);
			}
			if (changes) {
				builder.withChanges(changes);
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
