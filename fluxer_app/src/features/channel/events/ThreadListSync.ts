// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

interface ThreadListSyncPayload {
	threads: ThreadResponse[];
	joined_thread_ids: string[];
}

export function handleThreadListSync(data: ThreadListSyncPayload, _context: GatewayHandlerContext): void {
	Threads.handleThreadListSync({
		threads: data.threads,
		joinedThreadIds: data.joined_thread_ids,
	});
}
