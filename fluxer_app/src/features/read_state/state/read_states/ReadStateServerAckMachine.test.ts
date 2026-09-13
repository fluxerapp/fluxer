// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createReadStateServerAckSnapshot,
	type ReadStateServerAckInput,
	resolveReadStateServerAckDecision,
	selectReadStateServerAckDecision,
	transitionReadStateServerAckSnapshot,
} from '@app/features/read_state/state/read_states/ReadStateServerAckMachine';
import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {describe, expect, it} from 'vitest';

const BASE_TIMESTAMP = Date.UTC(2024, 0, 1);
const OLDER_ID = fromTimestamp(BASE_TIMESTAMP + 1000);
const ACK_ID = fromTimestamp(BASE_TIMESTAMP + 2000);
const NEWER_ID = fromTimestamp(BASE_TIMESTAMP + 3000);

function input(overrides: Partial<ReadStateServerAckInput> = {}): ReadStateServerAckInput {
	return {
		messageId: NEWER_ID,
		ackMessageId: ACK_ID,
		version: '3',
		serverVersion: '2',
		manual: false,
		readStateWasKnown: true,
		hasMentionCount: false,
		...overrides,
	};
}

describe('readStateServerAckMachine', () => {
	it('ignores stale versions before applying any other server ack policy', () => {
		expect(resolveReadStateServerAckDecision(input({version: '1', manual: true}))).toEqual({
			type: 'ignoreStaleVersion',
		});
	});

	it('routes manual server acknowledgements separately', () => {
		expect(resolveReadStateServerAckDecision(input({manual: true}))).toEqual({type: 'applyManualAck'});
	});

	it('marks older automatic acknowledgements as read-state-known without advancing ack state', () => {
		expect(resolveReadStateServerAckDecision(input({messageId: OLDER_ID}))).toEqual({
			type: 'ignoreOlderMessage',
			shouldMarkReadStateKnown: true,
		});
	});

	it('refreshes an existing ack only when read-state knowledge or mention count changes', () => {
		expect(
			resolveReadStateServerAckDecision(
				input({
					messageId: ACK_ID,
					readStateWasKnown: true,
					hasMentionCount: false,
				}),
			),
		).toEqual({
			type: 'refreshCurrentAck',
			shouldMarkReadStateKnown: true,
			shouldUpdateMentionCount: false,
			shouldRefreshUnreadEstimate: false,
			shouldNotify: false,
		});
		expect(
			resolveReadStateServerAckDecision(
				input({
					messageId: ACK_ID,
					readStateWasKnown: false,
					hasMentionCount: true,
				}),
			),
		).toEqual({
			type: 'refreshCurrentAck',
			shouldMarkReadStateKnown: true,
			shouldUpdateMentionCount: true,
			shouldRefreshUnreadEstimate: true,
			shouldNotify: true,
		});
	});

	it('advances newer automatic acknowledgements and carries mention update intent', () => {
		expect(resolveReadStateServerAckDecision(input({messageId: NEWER_ID, hasMentionCount: true}))).toEqual({
			type: 'advanceAck',
			shouldMarkReadStateKnown: true,
			shouldUpdateMentionCount: true,
		});
	});

	it('updates the decision from later server ack input', () => {
		const olderSnapshot = createReadStateServerAckSnapshot(input({messageId: OLDER_ID}));
		expect(selectReadStateServerAckDecision(olderSnapshot)).toEqual({
			type: 'ignoreOlderMessage',
			shouldMarkReadStateKnown: true,
		});

		const newerSnapshot = transitionReadStateServerAckSnapshot(olderSnapshot, {
			type: 'readStateServerAck.updated',
			input: input({messageId: NEWER_ID}),
		});

		expect(selectReadStateServerAckDecision(newerSnapshot)).toEqual({
			type: 'advanceAck',
			shouldMarkReadStateKnown: true,
			shouldUpdateMentionCount: false,
		});
	});
});
