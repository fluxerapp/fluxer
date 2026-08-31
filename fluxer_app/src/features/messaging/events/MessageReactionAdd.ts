// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import PollVotes from '../state/PollVotes';

interface ReactionEmojiPayload {
	id?: string | null;
	name?: string | null;
}

interface MessageReactionAddPayload {
	user_id: string;
	channel_id: string;
	message_id: string;
	emoji: ReactionEmojiPayload;
	guild_id?: string;
	member?: GuildMemberData;
	reaction_type?: number;
}

export function handleMessageReactionAdd(data: MessageReactionAddPayload, _context: GatewayHandlerContext): void {
	const emoji = data.emoji as ReactionEmoji;
	if (data.guild_id && data.member) {
		GuildMembers.hydrateIfMissing(data.guild_id, data.member);
	}
	SavedMessages.handleMessageReactionAdd(data.message_id);
	ChannelPins.handleMessageReactionAdd(data.channel_id, data.message_id);
	MentionFeed.handleMessageReactionAdd(data.message_id);
	if ((data.reaction_type ?? 0) === 2) PollVotes.handlePollVoteAdd(data.message_id, data.user_id, Number(emoji.id));
	else MessageReactions.handleReactionAdd(data.message_id, data.user_id, emoji);
	Messages.handleReaction({
		type: 'MESSAGE_REACTION_ADD',
		channelId: data.channel_id,
		messageId: data.message_id,
		userId: data.user_id,
		emoji,
		skipReactionStore: true,
		reactionType: data.reaction_type ?? 0,
	});
}
