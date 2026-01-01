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

import MessageReferenceStore from '~/stores/MessageReferenceStore';
import MessageStore from '~/stores/MessageStore';
import NotificationStore from '~/stores/NotificationStore';
import ReadStateStore from '~/stores/ReadStateStore';
import type {GatewayHandlerContext} from '../index';

interface MessageDeleteBulkPayload {
	channel_id: string;
	ids: Array<string>;
}

export function handleMessageDeleteBulk(data: MessageDeleteBulkPayload, _context: GatewayHandlerContext): void {
	MessageStore.handleMessageDeleteBulk({channelId: data.channel_id, ids: data.ids});
	MessageReferenceStore.handleMessageDeleteBulk(data.channel_id, data.ids);
	ReadStateStore.handleMessageDelete({channelId: data.channel_id});
	NotificationStore.handleMessageDelete({channelId: data.channel_id});
}
