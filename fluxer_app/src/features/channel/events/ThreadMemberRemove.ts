// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

interface ThreadMemberRemovePayload extends ThreadMemberResponse {}

export function handleThreadMemberRemove(data: ThreadMemberRemovePayload, _context: GatewayHandlerContext): void {
	Threads.handleThreadMemberRemove({threadId: data.thread_id});
}
