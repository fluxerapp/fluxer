// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/api/models/GuildMember';

export class GuildMemberAuditService {
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
}
