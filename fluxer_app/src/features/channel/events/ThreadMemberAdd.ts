// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

interface ThreadMemberAddPayload extends ThreadMemberResponse {}

export function handleThreadMemberAdd(data: ThreadMemberAddPayload, _context: GatewayHandlerContext): void {
	Threads.handleThreadMemberAdd({threadId: data.thread_id});
}
