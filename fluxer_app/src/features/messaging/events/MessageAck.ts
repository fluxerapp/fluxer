// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Notification from '@app/features/ui/state/Notification';

export interface MessageAckPayload {
	channel_id: string;
	message_id: string;
	mention_count: number;
	manual?: boolean;
	version?: string;
}

export function handleMessageAck(data: MessageAckPayload, _context: GatewayHandlerContext): void {
	ReadStates.handleMessageAck({
		channelId: data.channel_id,
		messageId: data.message_id,
		mentionCount: data.mention_count,
		manual: data.manual ?? false,
		version: data.version,
	});
	Notification.handleMessageAck({channelId: data.channel_id});
}
