// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Messages from '@app/features/messaging/state/MessagingMessages';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Notification from '@app/features/ui/state/Notification';

interface MessageDeleteBulkPayload {
	channel_id: string;
	ids: Array<string>;
}

export function handleMessageDeleteBulk(data: MessageDeleteBulkPayload, _context: GatewayHandlerContext): void {
	Messages.handleMessageDeleteBulk({channelId: data.channel_id, ids: data.ids});
	MessageReferences.handleMessageDeleteBulk(data.channel_id, data.ids);
	MessageReply.handleMessageDeleteBulk(data.channel_id, data.ids);
	ReadStates.handleMessageDelete({channelId: data.channel_id});
	Notification.handleMessageDelete({channelId: data.channel_id});
}
