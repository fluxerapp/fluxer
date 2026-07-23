// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';

interface ThreadDeletePayload {
	thread_id: string;
}

export function handleThreadDelete(data: ThreadDeletePayload, _context: GatewayHandlerContext): void {
	Threads.handleThreadDelete({threadId: data.thread_id});
}
