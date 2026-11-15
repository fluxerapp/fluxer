// SPDX-License-Identifier: AGPL-3.0-or-later

import { Endpoints } from '@app/features/app/constants/Endpoints';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import Messages from '@app/features/messaging/state/MessagingMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import { http } from '@app/features/platform/transport/RestTransport';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import Notification from '@app/features/ui/state/Notification';
import CallState from '@app/features/voice/state/CallState';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export function handleMessageCreate(data: Message, _context: GatewayHandlerContext): void {
	if (data.webhook_id && !data.author.bot && data.author.username?.includes("~ @")) {
		// probably persona fallback behavior
		// Get the message over the API, then handle that instead (that way, persona data is received)
		// when the fallback behavior is deprecated, this if block can be deleted with it
		http.get<Message>(Endpoints.CHANNEL_MESSAGE(data.channel_id, data.id)).then((response) => {
			if (response.ok) handleMessageCreate(response.body, _context);
		});
		return;
	}
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
	RollingTypingStore.remove(data.channel_id, data.author.id);
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
