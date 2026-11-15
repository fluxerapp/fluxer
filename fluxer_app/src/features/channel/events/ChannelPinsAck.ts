// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ReadStates from '@app/features/read_state/state/ReadStates';

export interface ChannelPinsAckPayload {
	channel_id: string;
	last_pin_timestamp?: string | null;
}

export function handleChannelPinsAck(data: ChannelPinsAckPayload, _context: GatewayHandlerContext): void {
	ReadStates.handleChannelPinsAck({
		channelId: data.channel_id,
		timestamp: data.last_pin_timestamp ?? undefined,
	});
}
