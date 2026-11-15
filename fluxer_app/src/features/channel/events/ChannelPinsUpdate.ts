// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import ReadStates from '@app/features/read_state/state/ReadStates';

export interface ChannelPinsUpdatePayload {
	channel_id: string;
	last_pin_timestamp?: string | null;
}

export function handleChannelPinsUpdate(data: ChannelPinsUpdatePayload, _context: GatewayHandlerContext): void {
	if (data.last_pin_timestamp) {
		ReadStates.handleChannelPinsUpdate({
			channelId: data.channel_id,
			lastPinTimestamp: data.last_pin_timestamp,
		});
	}
	ChannelPins.handleChannelPinsUpdate(data.channel_id, data.last_pin_timestamp ?? null);
}
