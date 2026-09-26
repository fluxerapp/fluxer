// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import type {SearchableGuildMember} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {extractTimestampFromSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';

const MIN_USERNAME_SUFFIX_LENGTH = 2;

function buildUsernameSearch(username: string): string {
	const normalized = username.trim().toLowerCase();
	if (normalized.length < MIN_USERNAME_SUFFIX_LENGTH) {
		return normalized;
	}
	const tokens = new Set<string>();
	for (let i = 0; i <= normalized.length - MIN_USERNAME_SUFFIX_LENGTH; i++) {
		tokens.add(normalized.slice(i));
	}
	return Array.from(tokens).join(' ');
}

export function convertToSearchableGuildMember(member: GuildMember, user: User): SearchableGuildMember {
	return {
		id: `${member.guildId}_${member.userId}`,
		guildId: member.guildId.toString(),
		userId: member.userId.toString(),
		username: user.username,
		usernameSearch: buildUsernameSearch(user.username),
		discriminator: String(user.discriminator).padStart(4, '0'),
		globalName: user.globalName ?? null,
		nickname: member.nickname,
		roleIds: Array.from(member.roleIds).map((id) => id.toString()),
		joinedAt: Math.floor(member.joinedAt.getTime() / 1000),
		joinSourceType: member.joinSourceType,
		sourceInviteCode: member.sourceInviteCode?.toString() ?? null,
		inviterId: member.inviterId?.toString() ?? null,
		userCreatedAt: Math.floor(extractTimestampFromSnowflake(member.userId.toString()) / 1000),
		isBot: user.isBot,
	};
}
