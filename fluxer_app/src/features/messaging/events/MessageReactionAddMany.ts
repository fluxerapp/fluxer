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

interface ReactionEntry {
	user_id: string;
	emoji: ReactionEmojiPayload;
	member?: GuildMemberData;
	reaction_type?: number;
}

interface MessageReactionAddManyPayload {
	channel_id: string;
	message_id: string;
	guild_id?: string;
	reactions: Array<ReactionEntry>;
}

export function handleMessageReactionAddMany(
	data: MessageReactionAddManyPayload,
	_context: GatewayHandlerContext,
): void {
	MessageReactions.batch(() => {
		for (const reaction of data.reactions) {
			const emoji = reaction.emoji as ReactionEmoji;
			if (data.guild_id && reaction.member) {
				GuildMembers.hydrateIfMissing(data.guild_id, reaction.member);
			}
			SavedMessages.handleMessageReactionAdd(data.message_id);
			if ((reaction.reaction_type ?? 0) === 2) PollVotes.handlePollVoteAdd(data.message_id, reaction.user_id, Number(emoji.id));
			else MessageReactions.handleReactionAdd(data.message_id, reaction.user_id, emoji);
			ChannelPins.handleMessageReactionAdd(data.channel_id, data.message_id);
			MentionFeed.handleMessageReactionAdd(data.message_id);
			Messages.handleReaction({
				type: 'MESSAGE_REACTION_ADD',
				channelId: data.channel_id,
				messageId: data.message_id,
				userId: reaction.user_id,
				emoji,
				skipReactionStore: true,
			});
		}
	});
}
