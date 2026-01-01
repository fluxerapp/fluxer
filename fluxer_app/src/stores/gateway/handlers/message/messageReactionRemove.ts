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

import ChannelPinsStore from '~/stores/ChannelPinsStore';
import MessageReactionsStore from '~/stores/MessageReactionsStore';
import MessageStore from '~/stores/MessageStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import type {ReactionEmoji} from '~/utils/ReactionUtils';
import type {GatewayHandlerContext} from '../index';

interface ReactionEmojiPayload {
	id?: string | null;
	name?: string | null;
}

interface MessageReactionRemovePayload {
	user_id: string;
	channel_id: string;
	message_id: string;
	emoji: ReactionEmojiPayload;
}

export function handleMessageReactionRemove(data: MessageReactionRemovePayload, _context: GatewayHandlerContext): void {
	const emoji = data.emoji as ReactionEmoji;

	SavedMessagesStore.handleMessageReactionRemove(data.message_id, data.user_id, emoji);
	MessageReactionsStore.handleReactionRemove(data.message_id, data.user_id, emoji);
	ChannelPinsStore.handleMessageReactionRemove(data.channel_id, data.message_id, data.user_id, emoji);
	RecentMentionsStore.handleMessageReactionRemove(data.message_id, data.user_id, emoji);
	MessageStore.handleReaction({
		type: 'MESSAGE_REACTION_REMOVE',
		channelId: data.channel_id,
		messageId: data.message_id,
		userId: data.user_id,
		emoji,
	});
}
