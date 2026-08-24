// SPDX-License-Identifier: AGPL-3.0-or-later

import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {describe, expect, it} from 'vitest';
import {
	createReadStateEntryStatusSnapshot,
	type ReadStateEntryStatusInput,
	type ReadStateEntryStatusValue,
	resolveReadStateEntryStatus,
	selectReadStateEntryStatusModel,
	transitionReadStateEntryStatusSnapshot,
} from './ReadStateEntryStatusMachine';

const BASE_TIMESTAMP = Date.UTC(2024, 0, 1);
const ACK_ID = fromTimestamp(BASE_TIMESTAMP + 1000);
const LAST_ID = fromTimestamp(BASE_TIMESTAMP + 2000);

function input(overrides: Partial<ReadStateEntryStatusInput> = {}): ReadStateEntryStatusInput {
	return {
		supportsUnreadTracking: true,
		hasBlockedDirectMessageRecipient: false,
		readStateKnown: true,
		lastMessageId: LAST_ID,
		ackMessageId: LAST_ID,
		mentionCount: 0,
		...overrides,
	};
}

function expectResolvedState(overrides: Partial<ReadStateEntryStatusInput>, expected: ReadStateEntryStatusValue): void {
	expect(resolveReadStateEntryStatus(input(overrides)).state).toBe(expected);
}

describe('readStateEntryStatusMachine', () => {
	it('routes the read-state status by priority', () => {
		expectResolvedState({supportsUnreadTracking: false, mentionCount: 1, ackMessageId: ACK_ID}, 'untracked');
		expectResolvedState({hasBlockedDirectMessageRecipient: true, mentionCount: 1, ackMessageId: ACK_ID}, 'blocked');
		expectResolvedState({readStateKnown: false, ackMessageId: ACK_ID}, 'unknown');
		expectResolvedState({lastMessageId: null, ackMessageId: ACK_ID}, 'unknown');
		expectResolvedState({ackMessageId: ACK_ID}, 'unread');
		expectResolvedState({ackMessageId: LAST_ID}, 'read');
	});

	it('derives unread and mention capabilities from the routed state', () => {
		expect(resolveReadStateEntryStatus(input({ackMessageId: ACK_ID, mentionCount: 2}))).toMatchObject({
			state: 'unread',
			canBeUnread: true,
			supportsMentions: true,
			hasUnread: true,
			hasMentions: true,
			isUnreadOrMentioned: true,
		});
		expect(resolveReadStateEntryStatus(input({hasBlockedDirectMessageRecipient: true, mentionCount: 2}))).toMatchObject(
			{
				state: 'blocked',
				canBeUnread: true,
				supportsMentions: false,
				hasUnread: false,
				hasMentions: true,
				isUnreadOrMentioned: false,
			},
		);
		expect(resolveReadStateEntryStatus(input({supportsUnreadTracking: false, mentionCount: 2}))).toMatchObject({
			state: 'untracked',
			canBeUnread: false,
			supportsMentions: false,
			hasUnread: false,
			hasMentions: true,
			isUnreadOrMentioned: false,
		});
	});

	it('re-routes when entry inputs change', () => {
		const readSnapshot = createReadStateEntryStatusSnapshot(input());
		expect(selectReadStateEntryStatusModel(readSnapshot).state).toBe('read');

		const unreadSnapshot = transitionReadStateEntryStatusSnapshot(readSnapshot, {
			type: 'readStateEntry.updated',
			input: input({ackMessageId: ACK_ID, mentionCount: 1}),
		});

		expect(selectReadStateEntryStatusModel(unreadSnapshot)).toMatchObject({
			state: 'unread',
			isUnreadOrMentioned: true,
		});
	});
});
