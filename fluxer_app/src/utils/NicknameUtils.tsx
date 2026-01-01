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

import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';

export const getNickname = (user: UserRecord, guildId?: string | null, channelId?: string | null) => {
	let name = user.displayName;

	const relationship = RelationshipStore.getRelationship(user.id);
	if (relationship?.nickname) {
		name = relationship.nickname;
	}

	guildId ??= SelectedGuildStore.selectedGuildId;
	if (guildId) {
		const member = GuildMemberStore.getMember(guildId, user.id);
		if (member?.nick) {
			name = member.nick;
		}
	} else if (channelId) {
		const channel = ChannelStore.getChannel(channelId);
		if (channel?.nicks?.[user.id]) {
			name = channel.nicks[user.id];
		}
	}

	return name;
};
