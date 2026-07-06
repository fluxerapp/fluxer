// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import CallState from '@app/features/voice/state/CallState';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

interface MessageUpdatePayload {
	id: string;
	channel_id?: string;
}

export function handleMessageUpdate(data: MessageUpdatePayload, _context: GatewayHandlerContext): void {
	const message = data as Message;
	if (message.guild_id && message.member) {
		GuildMembers.hydrateIfMissing(message.guild_id, {
			...message.member,
			user: message.author,
		} as GuildMemberData);
	}
	if (message.mentions && message.guild_id) {
		for (const mention of message.mentions) {
			if (mention.member) {
				GuildMembers.hydrateIfMissing(message.guild_id, {
					...mention.member,
					user: mention,
				} as GuildMemberData);
			}
		}
	}
	SavedMessages.handleMessageUpdate(message);
	ChannelPins.handleMessageUpdate(message);
	Messages.handleMessageUpdate({message, preservePollViewerSelection: true});
	MessageReferences.handleMessageUpdate(message);
	MentionFeed.handleMessageUpdate(message);
	if (message.channel_id && message.call) {
		CallState.handleCallParticipants(message.channel_id, [...message.call.participants]);
	}
}
