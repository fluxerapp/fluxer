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

import type {GuildMember} from '~/records/GuildMemberRecord';
import type {Message} from '~/records/MessageRecord';
import CallStateStore from '~/stores/CallStateStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import MessageReferenceStore from '~/stores/MessageReferenceStore';
import MessageStore from '~/stores/MessageStore';
import NotificationStore from '~/stores/NotificationStore';
import ReadStateStore from '~/stores/ReadStateStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import TypingStore from '~/stores/TypingStore';
import * as TtsUtils from '~/utils/TtsUtils';
import type {GatewayHandlerContext} from '../index';

export function handleMessageCreate(data: Message, _context: GatewayHandlerContext): void {
	if (data.guild_id && data.member) {
		GuildMemberStore.handleMemberAdd(data.guild_id, {
			...data.member,
			user: data.author,
		} as GuildMember);
	}

	if (data.mentions && data.guild_id) {
		for (const mention of data.mentions) {
			if (mention.member) {
				GuildMemberStore.handleMemberAdd(data.guild_id, {
					...mention.member,
					user: mention,
				} as GuildMember);
			}
		}
	}

	TypingStore.stopTypingOnMessageCreate(data);
	MessageStore.handleIncomingMessage({channelId: data.channel_id, message: data});
	MessageReferenceStore.handleMessageCreate(data, false);
	NotificationStore.handleMessageCreate({message: data});
	ReadStateStore.handleIncomingMessage({channelId: data.channel_id, message: data});
	GuildReadStateStore.handleGenericUpdate(data.channel_id);
	RecentMentionsStore.handleMessageCreate(data);
	TtsUtils.handleIncomingTtsMessage(data);
	if (data.call && data.channel_id) {
		CallStateStore.handleCallParticipants(data.channel_id, data.call.participants);
	}
}
