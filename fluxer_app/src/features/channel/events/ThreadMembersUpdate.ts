// SPDX-License-Identifier: AGPL-3.0-or-later

import Threads from '@app/features/channel/state/Threads';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';

interface ThreadMembersUpdatePayload {
	thread_id: string;
	added_members?: Array<{user_id: string; joined_at: string}>;
	removed_member_ids?: string[];
}

export function handleThreadMembersUpdate(data: ThreadMembersUpdatePayload, _context: GatewayHandlerContext): void {
	for (const member of data.added_members ?? []) {
		Threads.handleThreadMemberAdd({threadId: data.thread_id, userId: member.user_id});
	}
	for (const userId of data.removed_member_ids ?? []) {
		Threads.handleThreadMemberRemove({threadId: data.thread_id, userId});
	}
}
