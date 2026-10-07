// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';

interface MessageReactionRemoveAllPayload {
	channel_id: string;
	message_id: string;
}

export function handleMessageReactionRemoveAll(
	data: MessageReactionRemoveAllPayload,
	_context: GatewayHandlerContext,
): void {
	SavedMessages.handleMessageReactionRemoveAll(data.message_id);
	MessageReactions.handleReactionRemoveAll(data.message_id);
	ChannelPins.handleMessageReactionRemoveAll(data.channel_id, data.message_id);
	ForumPosts.handleReactionClear(data.channel_id, data.message_id);
	MentionFeed.handleMessageReactionRemoveAll(data.message_id);
	Messages.handleRemoveAllReactions({channelId: data.channel_id, messageId: data.message_id});
}
