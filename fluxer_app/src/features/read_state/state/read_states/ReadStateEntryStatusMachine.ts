// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';
import {compareMessageIds} from './shared';

export interface ReadStateEntryStatusInput {
	supportsUnreadTracking: boolean;
	hasBlockedDirectMessageRecipient: boolean;
	lastMessageId: string | null;
	ackMessageId: string | null;
	ackTimestamp: number;
	lastMessageTimestamp: number;
	mentionCount: number;
}

export type ReadStateEntryStatusEvent = {
	type: 'readStateEntry.updated';
	input: ReadStateEntryStatusInput;
};

export type ReadStateEntryStatusValue = 'untracked' | 'blocked' | 'read' | 'unread';

export interface ReadStateEntryStatusModel {
	state: ReadStateEntryStatusValue;
	canBeUnread: boolean;
	supportsMentions: boolean;
	hasUnread: boolean;
	hasMentions: boolean;
	isUnreadOrMentioned: boolean;
}

function getStatusValue(snapshot: ReadStateEntryStatusSnapshot): ReadStateEntryStatusValue {
	switch (snapshot.value) {
		case 'untracked':
			return 'untracked';
		case 'blocked':
			return 'blocked';
		case 'unread':
			return 'unread';
		default:
			return 'read';
	}
}

function isUnread(context: ReadStateEntryStatusInput): boolean {
	if (context.lastMessageId == null) return false;
	if (context.ackMessageId != null) {
		return compareMessageIds(context.ackMessageId, context.lastMessageId) < 0;
	}
	return context.ackTimestamp < context.lastMessageTimestamp;
}

function getStatusValueFromInput(input: ReadStateEntryStatusInput): ReadStateEntryStatusValue {
	if (!input.supportsUnreadTracking) return 'untracked';
	if (input.hasBlockedDirectMessageRecipient) return 'blocked';
	if (isUnread(input)) return 'unread';
	return 'read';
}

function buildStatusModel(
	state: ReadStateEntryStatusValue,
	input: ReadStateEntryStatusInput,
): ReadStateEntryStatusModel {
	const hasMentions = input.mentionCount > 0;
	const canBeUnread = state !== 'untracked';
	const supportsMentions = hasMentions && state !== 'untracked' && state !== 'blocked';
	const hasUnread = state === 'unread';
	return {
		state,
		canBeUnread,
		supportsMentions,
		hasUnread,
		hasMentions,
		isUnreadOrMentioned: hasUnread || supportsMentions,
	};
}

export const readStateEntryStatusMachine = setup({
	types: {} as {
		context: ReadStateEntryStatusInput;
		events: ReadStateEntryStatusEvent;
		input: ReadStateEntryStatusInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'readStateEntry.updated') return {};
			return event.input;
		}),
	},
	guards: {
		isUntracked: ({context}) => !context.supportsUnreadTracking,
		isBlocked: ({context}) => context.hasBlockedDirectMessageRecipient,
		isUnread: ({context}) => isUnread(context),
	},
}).createMachine({
	id: 'readStateEntryStatus',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'isUntracked', target: 'untracked'},
				{guard: 'isBlocked', target: 'blocked'},
				{guard: 'isUnread', target: 'unread'},
				{target: 'read'},
			],
		},
		untracked: {
			on: {'readStateEntry.updated': {target: 'routing', actions: 'applyInput'}},
		},
		blocked: {
			on: {'readStateEntry.updated': {target: 'routing', actions: 'applyInput'}},
		},
		read: {
			on: {'readStateEntry.updated': {target: 'routing', actions: 'applyInput'}},
		},
		unread: {
			on: {'readStateEntry.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ReadStateEntryStatusSnapshot = SnapshotFrom<typeof readStateEntryStatusMachine>;

export function createReadStateEntryStatusSnapshot(input: ReadStateEntryStatusInput): ReadStateEntryStatusSnapshot {
	return initialTransition(readStateEntryStatusMachine, input)[0];
}

export function transitionReadStateEntryStatusSnapshot(
	snapshot: ReadStateEntryStatusSnapshot,
	event: ReadStateEntryStatusEvent,
): ReadStateEntryStatusSnapshot {
	return transition(readStateEntryStatusMachine, snapshot, event)[0] as ReadStateEntryStatusSnapshot;
}

export function selectReadStateEntryStatusModel(snapshot: ReadStateEntryStatusSnapshot): ReadStateEntryStatusModel {
	return buildStatusModel(getStatusValue(snapshot), snapshot.context);
}

export function resolveReadStateEntryStatus(input: ReadStateEntryStatusInput): ReadStateEntryStatusModel {
	return buildStatusModel(getStatusValueFromInput(input), input);
}
