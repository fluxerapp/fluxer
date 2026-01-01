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
import type {GatewayHandlerContext} from '../index';

interface MessageReactionRemoveAllPayload {
	channel_id: string;
	message_id: string;
}

export function handleMessageReactionRemoveAll(
	data: MessageReactionRemoveAllPayload,
	_context: GatewayHandlerContext,
): void {
	SavedMessagesStore.handleMessageReactionRemoveAll(data.message_id);
	MessageReactionsStore.handleReactionRemoveAll(data.message_id);
	ChannelPinsStore.handleMessageReactionRemoveAll(data.channel_id, data.message_id);
	RecentMentionsStore.handleMessageReactionRemoveAll(data.message_id);
	MessageStore.handleRemoveAllReactions({channelId: data.channel_id, messageId: data.message_id});
}
