// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';
import {compareMessageIds, compareReadStateVersions} from './shared';

export interface ReadStateServerAckInput {
	messageId: string;
	ackMessageId: string | null;
	version?: string | null;
	serverVersion: string | null;
	manual: boolean;
	readStateWasKnown: boolean;
	hasMentionCount: boolean;
}

export type ReadStateServerAckDecision =
	| {
			type: 'ignoreStaleVersion';
	  }
	| {
			type: 'applyManualAck';
	  }
	| {
			type: 'ignoreOlderMessage';
			shouldMarkReadStateKnown: true;
	  }
	| {
			type: 'refreshCurrentAck';
			shouldMarkReadStateKnown: true;
			shouldUpdateMentionCount: boolean;
			shouldRefreshUnreadEstimate: boolean;
			shouldNotify: boolean;
	  }
	| {
			type: 'advanceAck';
			shouldMarkReadStateKnown: true;
			shouldUpdateMentionCount: boolean;
	  };

export type ReadStateServerAckEvent = {
	type: 'readStateServerAck.updated';
	input: ReadStateServerAckInput;
};

function hasStaleVersion(context: ReadStateServerAckInput): boolean {
	return context.version != null && compareReadStateVersions(context.version, context.serverVersion) < 0;
}

function isOlderThanCurrentAck(context: ReadStateServerAckInput): boolean {
	return context.ackMessageId != null && compareMessageIds(context.messageId, context.ackMessageId) < 0;
}

function isCurrentAck(context: ReadStateServerAckInput): boolean {
	return context.messageId === context.ackMessageId;
}

export const readStateServerAckMachine = setup({
	types: {} as {
		context: ReadStateServerAckInput;
		events: ReadStateServerAckEvent;
		input: ReadStateServerAckInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'readStateServerAck.updated') return {};
			return event.input;
		}),
	},
	guards: {
		hasStaleVersion: ({context}) => hasStaleVersion(context),
		ackedManually: ({context}) => context.manual,
		isOlderThanCurrentAck: ({context}) => isOlderThanCurrentAck(context),
		isCurrentAck: ({context}) => isCurrentAck(context),
	},
}).createMachine({
	id: 'readStateServerAck',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'hasStaleVersion', target: 'staleVersion'},
				{guard: 'ackedManually', target: 'manualAck'},
				{guard: 'isOlderThanCurrentAck', target: 'olderMessage'},
				{guard: 'isCurrentAck', target: 'currentAck'},
				{target: 'newerAck'},
			],
		},
		staleVersion: {
			on: {'readStateServerAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		manualAck: {
			on: {'readStateServerAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		olderMessage: {
			on: {'readStateServerAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		currentAck: {
			on: {'readStateServerAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
		newerAck: {
			on: {'readStateServerAck.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ReadStateServerAckSnapshot = SnapshotFrom<typeof readStateServerAckMachine>;

export function createReadStateServerAckSnapshot(input: ReadStateServerAckInput): ReadStateServerAckSnapshot {
	return getInitialSnapshot(readStateServerAckMachine, input);
}

export function transitionReadStateServerAckSnapshot(
	snapshot: ReadStateServerAckSnapshot,
	event: ReadStateServerAckEvent,
): ReadStateServerAckSnapshot {
	return transition(readStateServerAckMachine, snapshot, event)[0] as ReadStateServerAckSnapshot;
}

export function selectReadStateServerAckDecision(snapshot: ReadStateServerAckSnapshot): ReadStateServerAckDecision {
	switch (snapshot.value) {
		case 'staleVersion':
			return {type: 'ignoreStaleVersion'};
		case 'manualAck':
			return {type: 'applyManualAck'};
		case 'olderMessage':
			return {type: 'ignoreOlderMessage', shouldMarkReadStateKnown: true};
		case 'currentAck':
			return {
				type: 'refreshCurrentAck',
				shouldMarkReadStateKnown: true,
				shouldUpdateMentionCount: snapshot.context.hasMentionCount,
				shouldRefreshUnreadEstimate: snapshot.context.hasMentionCount,
				shouldNotify: !snapshot.context.readStateWasKnown || snapshot.context.hasMentionCount,
			};
		default:
			return {
				type: 'advanceAck',
				shouldMarkReadStateKnown: true,
				shouldUpdateMentionCount: snapshot.context.hasMentionCount,
			};
	}
}

export function resolveReadStateServerAckDecision(input: ReadStateServerAckInput): ReadStateServerAckDecision {
	return selectReadStateServerAckDecision(createReadStateServerAckSnapshot(input));
}
