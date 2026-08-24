// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';

const logger = new Logger('MessageUtils');

export function isMentioned(user: User, message: Message): boolean {
	const channel = Channels.getChannel(message.channelId);
	if (channel == null) {
		logger.warn(`${message.channelId} does not exist!`);
		return false;
	}
	const suppressEveryone = UserGuildSettings.isEveryoneMentionSuppressed(channel.guildId ?? null);
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
	const guild = Guilds.getGuild(channel.guildId);
	if (!guild) {
		return false;
	}
	const guildMember = GuildMembers.getMember(guild.id, user.id);
	if (!guildMember) {
		return false;
	}
	const suppressRoles = UserGuildSettings.isRoleMentionSuppressed(channel.guildId);
	return !suppressRoles && message.mentionRoles.some((roleId) => guildMember.roles.has(roleId));
}
