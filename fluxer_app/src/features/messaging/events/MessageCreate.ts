// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import Messages from '@app/features/messaging/state/MessagingMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import Notification from '@app/features/ui/state/Notification';
import CallState from '@app/features/voice/state/CallState';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export function handleMessageCreate(data: Message, _context: GatewayHandlerContext): void {
	if (data.guild_id && data.member) {
		GuildMembers.hydrateIfMissing(data.guild_id, {
			...data.member,
			user: data.author,
		} as GuildMemberData);
	}
	if (data.mentions && data.guild_id) {
		for (const mention of data.mentions) {
			if (mention.member) {
				GuildMembers.hydrateIfMissing(data.guild_id, {
					...mention.member,
					user: mention,
				} as GuildMemberData);
			}
		}
	}
	if (data.thread !== undefined) {
		ChannelThreads.ingestMessageThreads([data]);
	}
	TypingIndicator.stopTypingOnMessageCreate(data);
	Messages.handleIncomingMessage({channelId: data.channel_id, message: data});
	MessageReferences.handleMessageCreate(data, false);
	Notification.handleMessageCreate({message: data});
	ReadStates.handleIncomingMessage({channelId: data.channel_id, message: data});
	GuildReadState.handleGenericUpdate(data.channel_id);
	MentionFeed.handleMessageCreate(data);
	TtsUtils.handleIncomingTtsMessage(data);
	if (data.call && data.channel_id) {
		CallState.handleCallParticipants(data.channel_id, [...data.call.participants]);
	}
}
