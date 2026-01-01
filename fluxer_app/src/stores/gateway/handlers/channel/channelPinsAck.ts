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

import ReadStateStore from '~/stores/ReadStateStore';
import type {GatewayHandlerContext} from '../index';

interface ChannelPinsAckPayload {
	channel_id: string;
	last_pin_timestamp?: string | null;
}

export function handleChannelPinsAck(data: ChannelPinsAckPayload, _context: GatewayHandlerContext): void {
	ReadStateStore.handleChannelPinsAck({
		channelId: data.channel_id,
		timestamp: data.last_pin_timestamp ?? undefined,
	});
}
