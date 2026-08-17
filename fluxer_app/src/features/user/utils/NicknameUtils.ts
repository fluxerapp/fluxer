// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GuildMembers from '@app/features/member/state/GuildMembers';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import Relationships from '@app/features/relationship/state/Relationships';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
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

export function formatNicknameForStreamerMode(nickname: string): string {
	const resolved = formatNameForStreamerMode(nickname);
	noteText(resolved);
	return resolved;
}

export function getDisplayName(user: UserDisplayNameLike): string {
	const name = formatNameForStreamerMode(
		user.displayName || user.globalName || user.global_name || user.username || '',
	);
	noteText(name);
	return name;
}

export function getNickname(user: User, guildId?: string | null, channelId?: string): string {
	let name = user.displayName || user.globalName || user.username || user.id || '';
	const relationship = Relationships.getRelationship(user.id);
	if (relationship?.nickname) {
		name = relationship.nickname;
	}
	const channel = channelId ? Channels.getChannel(channelId) : null;
	const selectedGuildId = channelId ? undefined : (SelectedGuild.selectedGuildId ?? undefined);
	const resolvedGuildId = guildId === null ? undefined : (guildId ?? channel?.guildId ?? selectedGuildId);
	if (resolvedGuildId) {
		const member = GuildMembers.getMember(resolvedGuildId, user.id);
		if (member?.nick) {
			name = member.nick;
		}
	} else if (channel) {
		if (channel?.nicks?.[user.id]) {
			name = channel.nicks[user.id];
		}
	}
	return formatNicknameForStreamerMode(name);
}
