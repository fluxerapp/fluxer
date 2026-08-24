// SPDX-License-Identifier: AGPL-3.0-or-later

import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {describe, expect, it} from 'vitest';
import {
	createReadStateAckSnapshot,
	type ReadStateAckInput,
	resolveReadStateAckDecision,
	selectReadStateAckDecision,
	transitionReadStateAckSnapshot,
} from './ReadStateAckMachine';

const BASE_TIMESTAMP = Date.UTC(2024, 0, 1);
const OLDER_ID = fromTimestamp(BASE_TIMESTAMP + 1000);
const ACK_ID = fromTimestamp(BASE_TIMESTAMP + 2000);
const NEWER_ID = fromTimestamp(BASE_TIMESTAMP + 3000);

function input(overrides: Partial<ReadStateAckInput> = {}): ReadStateAckInput {
	return {
		requestedMessageId: NEWER_ID,
		lastMessageId: NEWER_ID,
		ackMessageId: ACK_ID,
		ackedManually: false,
		messagesLoaded: true,
		supportsUnreadTracking: true,
		hasMentions: false,
		hasOldestUnreadMessage: false,
		hasStickyUnreadMessage: false,
		local: false,
		force: false,
		isExplicitUserAction: false,
		preserveStickyUnread: false,
		...overrides,
	};
}

describe('readStateAckMachine', () => {
	it('allows a loaded tracked channel to acknowledge the requested message', () => {
		expect(resolveReadStateAckDecision(input({hasMentions: true}))).toEqual({
			type: 'ack',
			messageId: NEWER_ID,
			hadMentions: true,
			shouldPreserveStickyUnread: false,
			shouldClearManualAck: false,
		});
	});

	it('falls back to the last message when no requested message is provided', () => {
		expect(resolveReadStateAckDecision(input({requestedMessageId: null, lastMessageId: NEWER_ID}))).toMatchObject({
			type: 'ack',
			messageId: NEWER_ID,
		});
	});

	it('blocks automatic ack while manual ack is held, messages are unloaded, or the channel is untracked', () => {
		expect(resolveReadStateAckDecision(input({ackedManually: true}))).toEqual({
			type: 'ignored',
			reason: 'manualAck',
		});
		expect(resolveReadStateAckDecision(input({messagesLoaded: false}))).toEqual({
			type: 'ignored',
			reason: 'notLoaded',
		});
		expect(resolveReadStateAckDecision(input({supportsUnreadTracking: false}))).toEqual({
			type: 'ignored',
			reason: 'untracked',
		});
	});

	it('lets local, forced, and explicit user acknowledgements override automatic ack guards', () => {
		expect(resolveReadStateAckDecision(input({ackedManually: true, isExplicitUserAction: true}))).toMatchObject({
			type: 'ack',
			shouldClearManualAck: true,
		});
		expect(resolveReadStateAckDecision(input({messagesLoaded: false, local: true}))).toMatchObject({
			type: 'ack',
			shouldClearManualAck: true,
		});
		expect(resolveReadStateAckDecision(input({supportsUnreadTracking: false, force: true}))).toMatchObject({
			type: 'ack',
			shouldClearManualAck: true,
		});
	});

	it('rejects missing and older message acknowledgements unless forced', () => {
		expect(resolveReadStateAckDecision(input({requestedMessageId: null, lastMessageId: null}))).toEqual({
			type: 'ignored',
			reason: 'missingMessage',
		});
		expect(resolveReadStateAckDecision(input({requestedMessageId: OLDER_ID}))).toEqual({
			type: 'ignored',
			reason: 'olderThanCurrentAck',
		});
		expect(resolveReadStateAckDecision(input({requestedMessageId: OLDER_ID, force: true}))).toMatchObject({
			type: 'ack',
			messageId: OLDER_ID,
		});
	});

	it('preserves sticky unread only when requested and no sticky unread already exists', () => {
		expect(
			resolveReadStateAckDecision(
				input({
					preserveStickyUnread: true,
					hasOldestUnreadMessage: true,
					hasStickyUnreadMessage: false,
				}),
			),
		).toMatchObject({
			type: 'ack',
			shouldPreserveStickyUnread: true,
		});
		expect(
			resolveReadStateAckDecision(
				input({
					preserveStickyUnread: true,
					hasOldestUnreadMessage: true,
					hasStickyUnreadMessage: true,
				}),
			),
		).toMatchObject({
			type: 'ack',
			shouldPreserveStickyUnread: false,
		});
	});

	it('updates the decision from later ack input', () => {
		const ignoredSnapshot = createReadStateAckSnapshot(input({ackedManually: true}));
		expect(selectReadStateAckDecision(ignoredSnapshot)).toEqual({type: 'ignored', reason: 'manualAck'});

		const ackSnapshot = transitionReadStateAckSnapshot(ignoredSnapshot, {
			type: 'readStateAck.updated',
			input: input({ackedManually: false}),
		});

		expect(selectReadStateAckDecision(ackSnapshot)).toMatchObject({
			type: 'ack',
			messageId: NEWER_ID,
		});
	});
});
