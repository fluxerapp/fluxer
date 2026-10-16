// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import type {ThreadStatsRow} from '@app/api/database/types/ThreadTypes';

export class ThreadStats {
	readonly threadId: ChannelID;
	readonly messageCount: number;
	readonly totalMessageSent: number;

	constructor(row: ThreadStatsRow) {
		this.threadId = row.thread_id;
		this.messageCount = Math.max(0, row.message_count ?? 0);
		this.totalMessageSent = Math.max(0, row.total_message_sent ?? 0);
	}

	static empty(threadId: ChannelID): ThreadStats {
		return new ThreadStats({thread_id: threadId, message_count: 0, total_message_sent: 0});
	}
}
