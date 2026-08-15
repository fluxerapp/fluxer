// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GuildMembers from '@app/features/member/state/GuildMembers';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Relationships from '@app/features/relationship/state/Relationships';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import type {User} from '@app/features/user/models/User';

export interface UserDisplayNameLike {
	username: string;
	displayName?: string | null;
	globalName?: string | null;
	global_name?: string | null;
}

export function truncateStreamerModeName(name: string): string {
	const trimmed = name.trim();
	if (!trimmed) return '…';
	return `${Array.from(trimmed)[0]}…`;
}

export function formatNameForStreamerMode(name: string): string {
	return StreamerMode.shouldTruncateUsernames ? truncateStreamerModeName(name) : name;
}

export function formatTagForStreamerMode(tag: string): string {
	return StreamerMode.shouldTruncateUsernames ? truncateStreamerModeName(tag) : tag;
}

export function formatUserTagForStreamerMode(user: Pick<User, 'tag' | 'username' | 'discriminator'>): string {
	return formatTagForStreamerMode(user.tag || `${user.username}#${user.discriminator}`);
}

export function getDisplayName(user: UserDisplayNameLike): string {
	return formatNameForStreamerMode(user.displayName || user.globalName || user.global_name || user.username || '');
}

function resolveRelationshipNickname(userId: string): string | null {
	const relationship = Relationships.getRelationship(userId);
	if (relationship === null || relationship === undefined) {
		return null;
	}
	if (!relationship.nickname) {
		return null;
	}
	return relationship.nickname;
}

function resolveGuildMemberNickname(guildId: string, userId: string): string | null {
	const member = GuildMembers.getMember(guildId, userId);
	if (member === null || member === undefined) {
		return null;
	}
	if (!member.nick) {
		return null;
	}
	return member.nick;
}

export function getUntruncatedNickname(user: User, guildId?: string | null, channelId?: string): string {
	let name = user.displayName || user.globalName || user.username || user.id || '';
	const relationshipNickname = resolveRelationshipNickname(user.id);
	if (relationshipNickname !== null) {
		name = relationshipNickname;
	}
	const channel = channelId ? Channels.getChannel(channelId) : null;
	let selectedGuildId: string | undefined;
	if (!channelId && SelectedGuild.selectedGuildId !== null) {
		selectedGuildId = SelectedGuild.selectedGuildId;
	}
	let resolvedGuildId: string | undefined;
	if (guildId === null) {
		resolvedGuildId = undefined;
	} else if (guildId !== undefined) {
		resolvedGuildId = guildId;
	} else if (channel !== null && channel !== undefined && channel.guildId !== undefined) {
		resolvedGuildId = channel.guildId;
	} else {
		resolvedGuildId = selectedGuildId;
	}
	if (resolvedGuildId) {
		const memberNickname = resolveGuildMemberNickname(resolvedGuildId, user.id);
		if (memberNickname !== null) {
			name = memberNickname;
		}
	} else if (channel !== null && channel !== undefined) {
		const channelNickname = channel.nicks[user.id];
		if (channelNickname) {
			name = channelNickname;
		}
	}
	return name;
}

export function getUntruncatedGuildMemberNickname(member: {guildId: string; nick: string | null; user: User}): string {
	const relationshipNickname = resolveRelationshipNickname(member.user.id);
	if (relationshipNickname !== null) {
		return relationshipNickname;
	}
	if (member.nick !== null && member.nick.length > 0) {
		return member.nick;
	}
	return member.user.displayName || member.user.globalName || member.user.username || member.user.id || '';
}

export function getGuildMemberNickname(member: {guildId: string; nick: string | null; user: User}): string {
	return formatNameForStreamerMode(getUntruncatedGuildMemberNickname(member));
}

export function getNickname(user: User, guildId?: string | null, channelId?: string): string {
	return formatNameForStreamerMode(getUntruncatedNickname(user, guildId, channelId));
}
