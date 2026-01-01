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

import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';

export const isMentioned = (user: UserRecord, message: MessageRecord): boolean => {
	const channel = ChannelStore.getChannel(message.channelId);
	if (channel == null) {
		console.warn(`${message.channelId} does not exist!`);
		return false;
	}
	const suppressEveryone = UserGuildSettingsStore.isSuppressEveryoneEnabled(channel.guildId ?? null);
	const mentionEveryone = message.mentionEveryone && !suppressEveryone;
	if (mentionEveryone) {
		return true;
	}
	if (message.mentions.some((mention) => mention.id === user.id)) {
		return true;
	}
	if (channel.guildId == null) {
		return false;
	}
	const guild = GuildStore.getGuild(channel.guildId);
	if (!guild) {
		return false;
	}
	const guildMember = GuildMemberStore.getMember(guild.id, user.id);
	if (!guildMember) {
		return false;
	}
	return message.mentionRoles.some((roleId) => guildMember.roles.has(roleId));
};
