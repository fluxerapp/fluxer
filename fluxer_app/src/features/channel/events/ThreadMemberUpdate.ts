// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';

interface ThreadMemberUpdatePayload {
	thread_id: string;
	user_id: string;
	joined_at: string;
}

export function handleThreadMemberUpdate(data: ThreadMemberUpdatePayload, _context: GatewayHandlerContext): void {
	Threads.handleThreadMemberAdd({threadId: data.thread_id});
}
