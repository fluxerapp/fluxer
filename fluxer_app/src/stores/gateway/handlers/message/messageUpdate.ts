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

import type {Message} from '~/records/MessageRecord';
import CallStateStore from '~/stores/CallStateStore';
import ChannelPinsStore from '~/stores/ChannelPinsStore';
import MessageStore from '~/stores/MessageStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import type {GatewayHandlerContext} from '../index';

interface MessageUpdatePayload {
	id: string;
	channel_id?: string;
}

export function handleMessageUpdate(data: MessageUpdatePayload, _context: GatewayHandlerContext): void {
	const message = data as Message;

	SavedMessagesStore.handleMessageUpdate(message);
	ChannelPinsStore.handleMessageUpdate(message);
	MessageStore.handleMessageUpdate({message});
	RecentMentionsStore.handleMessageUpdate(message);
	if (message.channel_id && message.call) {
		CallStateStore.handleCallParticipants(message.channel_id, message.call.participants);
	}
}
