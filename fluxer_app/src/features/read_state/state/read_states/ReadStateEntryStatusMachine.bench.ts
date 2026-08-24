// SPDX-License-Identifier: AGPL-3.0-or-later

import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {bench, describe} from 'vitest';
import {type ReadStateEntryStatusInput, resolveReadStateEntryStatus} from './ReadStateEntryStatusMachine';

const BASE_TIMESTAMP = Date.UTC(2024, 0, 1);
const ACK_ID = fromTimestamp(BASE_TIMESTAMP + 1_000);
const LAST_ID = fromTimestamp(BASE_TIMESTAMP + 2_000);
const INPUTS = Object.freeze(
	Array.from({length: 100_000}, (_, index): ReadStateEntryStatusInput => {
		switch (index % 6) {
			case 0:
				return {
					supportsUnreadTracking: false,
					hasBlockedDirectMessageRecipient: false,
					readStateKnown: true,
					lastMessageId: LAST_ID,
					ackMessageId: ACK_ID,
					mentionCount: 1,
				};
			case 1:
				return {
					supportsUnreadTracking: true,
					hasBlockedDirectMessageRecipient: true,
					readStateKnown: true,
					lastMessageId: LAST_ID,
					ackMessageId: ACK_ID,
					mentionCount: 1,
				};
			case 2:
				return {
					supportsUnreadTracking: true,
					hasBlockedDirectMessageRecipient: false,
					readStateKnown: false,
					lastMessageId: LAST_ID,
					ackMessageId: ACK_ID,
					mentionCount: 0,
				};
			case 3:
				return {
					supportsUnreadTracking: true,
					hasBlockedDirectMessageRecipient: false,
					readStateKnown: true,
					lastMessageId: null,
					ackMessageId: ACK_ID,
					mentionCount: 0,
				};
			case 4:
				return {
					supportsUnreadTracking: true,
					hasBlockedDirectMessageRecipient: false,
					readStateKnown: true,
					lastMessageId: LAST_ID,
					ackMessageId: ACK_ID,
					mentionCount: 2,
				};
			default:
				return {
					supportsUnreadTracking: true,
					hasBlockedDirectMessageRecipient: false,
					readStateKnown: true,
					lastMessageId: LAST_ID,
					ackMessageId: LAST_ID,
					mentionCount: 0,
				};
		}
	}),
);

describe('ReadStateEntryStatusMachine benchmarks', () => {
	bench('resolves 100k mixed read-state entry statuses', () => {
		let unreadOrMentionCount = 0;
		for (const input of INPUTS) {
			if (resolveReadStateEntryStatus(input).isUnreadOrMentioned) {
				unreadOrMentionCount++;
			}
		}
		(globalThis as {__readStateEntryStatusBenchSink?: number}).__readStateEntryStatusBenchSink = unreadOrMentionCount;
	});
});
