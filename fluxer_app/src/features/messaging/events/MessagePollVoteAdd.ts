// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import Messages from '@app/features/messaging/state/MessagingMessages';

interface MessagePollVoteAddPayload {
	user_id: string;
	channel_id: string;
	message_id: string;
	guild_id?: string;
	answer_id: number;
}

export function handleMessagePollVoteAdd(data: MessagePollVoteAddPayload, _context: GatewayHandlerContext): void {
	Messages.handlePollVote({
		type: 'MESSAGE_POLE_VOTE_ADD',
		channelId: data.channel_id,
		messageId: data.message_id,
		userId: data.user_id,
		answerId: data.answer_id,
	});
}
