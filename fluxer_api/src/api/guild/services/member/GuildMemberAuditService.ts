// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {Logger} from '@app/api/Logger';
import type {GuildMember} from '@app/api/models/GuildMember';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';

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
