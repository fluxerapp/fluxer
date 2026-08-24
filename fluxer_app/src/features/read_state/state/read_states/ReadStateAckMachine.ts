// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';
import {compareMessageIds} from './shared';

export interface ReadStateAckInput {
	requestedMessageId?: string | null;
	lastMessageId: string | null;
	ackMessageId: string | null;
	ackedManually: boolean;
	messagesLoaded: boolean;
	supportsUnreadTracking: boolean;
	hasMentions: boolean;
	hasOldestUnreadMessage: boolean;
	hasStickyUnreadMessage: boolean;
	local: boolean;
	force: boolean;
	isExplicitUserAction: boolean;
	preserveStickyUnread: boolean;
}

export type ReadStateAckIgnoredReason =
	| 'manualAck'
	| 'notLoaded'
	| 'untracked'
	| 'missingMessage'
	| 'olderThanCurrentAck';

export type ReadStateAckDecision =
	| {
			type: 'ignored';
			reason: ReadStateAckIgnoredReason;
	  }
	| {
			type: 'ack';
			messageId: string;
			hadMentions: boolean;
			shouldPreserveStickyUnread: boolean;
			shouldClearManualAck: boolean;
	  };

export type ReadStateAckEvent = {
	type: 'readStateAck.updated';
	input: ReadStateAckInput;
};

function isOverrideAck(context: ReadStateAckInput): boolean {
	return context.force || context.local || context.isExplicitUserAction;
}

function getFinalMessageId(context: ReadStateAckInput): string | null {
	return context.requestedMessageId ?? context.lastMessageId;
}

function getIgnoredReason(snapshot: ReadStateAckSnapshot): ReadStateAckIgnoredReason {
	switch (snapshot.value) {
		case 'manualAckHeld':
			return 'manualAck';
		case 'notLoaded':
			return 'notLoaded';
		case 'untracked':
			return 'untracked';
		case 'missingMessage':
			return 'missingMessage';
		default:
			return 'olderThanCurrentAck';
	}
}

export const readStateAckMachine = setup({
	types: {} as {
		context: ReadStateAckInput;
		events: ReadStateAckEvent;
		input: ReadStateAckInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'readStateAck.updated') return {};
			return event.input;
		}),
	},
	guards: {
		isManualAckHeld: ({context}) => !isOverrideAck(context) && context.ackedManually,
		isNotLoaded: ({context}) => !isOverrideAck(context) && !context.messagesLoaded,
		isUntracked: ({context}) => !isOverrideAck(context) && !context.supportsUnreadTracking,
		isMissingMessage: ({context}) => getFinalMessageId(context) == null,
		isOlderThanCurrentAck: ({context}) => {
			const finalMessageId = getFinalMessageId(context);
			return !context.force && finalMessageId != null && compareMessageIds(finalMessageId, context.ackMessageId) < 0;
		},
	},
}).createMachine({
	id: 'readStateAck',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'isManualAckHeld', target: 'manualAckHeld'},
				{guard: 'isNotLoaded', target: 'notLoaded'},
				{guard: 'isUntracked', target: 'untracked'},
				{guard: 'isMissingMessage', target: 'missingMessage'},
				{guard: 'isOlderThanCurrentAck', target: 'olderThanCurrentAck'},
				{target: 'ack'},
			],
		},
		manualAckHeld: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		notLoaded: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		untracked: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		missingMessage: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		olderThanCurrentAck: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		ack: {
			on: {'readStateAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ReadStateAckSnapshot = SnapshotFrom<typeof readStateAckMachine>;

export function createReadStateAckSnapshot(input: ReadStateAckInput): ReadStateAckSnapshot {
	return getInitialSnapshot(readStateAckMachine, input);
}

export function transitionReadStateAckSnapshot(
	snapshot: ReadStateAckSnapshot,
	event: ReadStateAckEvent,
): ReadStateAckSnapshot {
	return transition(readStateAckMachine, snapshot, event)[0] as ReadStateAckSnapshot;
}

export function selectReadStateAckDecision(snapshot: ReadStateAckSnapshot): ReadStateAckDecision {
	if (snapshot.value !== 'ack') {
		return {type: 'ignored', reason: getIgnoredReason(snapshot)};
	}
	return {
		type: 'ack',
		messageId: getFinalMessageId(snapshot.context) as string,
		hadMentions: snapshot.context.hasMentions,
		shouldPreserveStickyUnread:
			snapshot.context.preserveStickyUnread &&
			snapshot.context.hasOldestUnreadMessage &&
			!snapshot.context.hasStickyUnreadMessage,
		shouldClearManualAck: isOverrideAck(snapshot.context),
	};
}

export function resolveReadStateAckDecision(input: ReadStateAckInput): ReadStateAckDecision {
	return selectReadStateAckDecision(createReadStateAckSnapshot(input));
}
