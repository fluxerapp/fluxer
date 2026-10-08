// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import MentionFeed from '@app/features/notification/state/MentionFeed';

interface ReactionEmojiPayload {
	id?: string | null;
	name?: string | null;
}

interface MessageReactionRemoveEmojiPayload {
	channel_id: string;
	message_id: string;
	emoji: ReactionEmojiPayload;
}

export function handleMessageReactionRemoveEmoji(
	data: MessageReactionRemoveEmojiPayload,
	_context: GatewayHandlerContext,
): void {
	const emoji = data.emoji as ReactionEmoji;
	SavedMessages.handleMessageReactionRemoveEmoji(data.message_id);
	MessageReactions.handleReactionRemoveEmoji(data.message_id, emoji);
	ChannelPins.handleMessageReactionRemoveEmoji(data.channel_id, data.message_id);
	ForumPosts.handleReactionClear(data.channel_id, data.message_id, emoji);
	MentionFeed.handleMessageReactionRemoveEmoji(data.message_id);
	Messages.handleRemoveReactionEmoji({
		channelId: data.channel_id,
		messageId: data.message_id,
		emoji,
	});
}
