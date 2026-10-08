// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import ChannelPins from '@app/features/channel/state/ChannelPins';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

interface ReactionEmojiPayload {
	id?: string | null;
	name?: string | null;
}

interface MessageReactionRemovePayload {
	user_id: string;
	channel_id: string;
	message_id: string;
	emoji: ReactionEmojiPayload;
	guild_id?: string;
	member?: GuildMemberData;
}

export function handleMessageReactionRemove(data: MessageReactionRemovePayload, _context: GatewayHandlerContext): void {
	const emoji = data.emoji as ReactionEmoji;
	if (data.guild_id && data.member) {
		GuildMembers.hydrateIfMissing(data.guild_id, data.member);
	}
	SavedMessages.handleMessageReactionRemove(data.message_id);
	MessageReactions.handleReactionRemove(data.message_id, data.user_id, emoji);
	ChannelPins.handleMessageReactionRemove(data.channel_id, data.message_id);
	ForumPosts.handleReaction(
		data.channel_id,
		data.message_id,
		emoji,
		false,
		data.user_id === Authentication.currentUserId,
	);
	MentionFeed.handleMessageReactionRemove(data.message_id);
	Messages.handleReaction({
		type: 'MESSAGE_REACTION_REMOVE',
		channelId: data.channel_id,
		messageId: data.message_id,
		userId: data.user_id,
		emoji,
		skipReactionStore: true,
	});
}
