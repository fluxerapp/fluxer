// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

export function handleThreadUpdate(data: ThreadResponse, _context: GatewayHandlerContext): void {
	Threads.handleThreadUpdate(data);
}
