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

import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';

interface SystemMessageData {
	author: UserRecord;
	channel: ChannelRecord | null;
	guild: GuildRecord | undefined;
}

export function useSystemMessageData(message: MessageRecord): SystemMessageData {
	const authorFromStore = UserStore.getUser(message.author.id);
	const author = authorFromStore ?? message.author;
	const channel = ChannelStore.getChannel(message.channelId);
	const guild = GuildStore.getGuild(channel?.guildId ?? '');

	return {
		author,
		channel: channel ?? null,
		guild,
	};
}
