// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';
import {compareMessageIds} from './shared';

export interface ReadStateIncomingMessageInput {
	isCurrentUserAuthor: boolean;
	automaticAckEnabled: boolean;
	isAtBottom: boolean;
	authorBlocked: boolean;
	hadUnreadOrMentions: boolean;
	messageId: string;
	ackMessageId: string | null;
	coveredByLastMessage: boolean;
}

export type ReadStateIncomingMessageDecision =
	| {
			type: 'ackCurrentUserMessage';
	  }
	| {
			type: 'ackAutomaticMessage';
	  }
	| {
			type: 'ackBlockedMessage';
	  }
	| {
			type: 'ignoreBlockedMessage';
	  }
	| {
			type: 'coveredByAck';
	  }
	| {
			type: 'recordUnread';
			coveredByLastMessage: boolean;
	  };

export type ReadStateIncomingMessageEvent = {
	type: 'incomingMessage.updated';
	input: ReadStateIncomingMessageInput;
};

function isCoveredByAck(context: ReadStateIncomingMessageInput): boolean {
	if (context.ackMessageId == null) return false;
	return compareMessageIds(context.messageId, context.ackMessageId) <= 0;
}

function getDecision(snapshot: ReadStateIncomingMessageSnapshot): ReadStateIncomingMessageDecision {
	switch (snapshot.value) {
		case 'currentUserMessage':
			return {type: 'ackCurrentUserMessage'};
		case 'automaticAckMessage':
			return {type: 'ackAutomaticMessage'};
		case 'blockedMessageAck':
			return {type: 'ackBlockedMessage'};
		case 'blockedMessageIgnored':
			return {type: 'ignoreBlockedMessage'};
		case 'coveredByAck':
			return {type: 'coveredByAck'};
		default:
			return {
				type: 'recordUnread',
				coveredByLastMessage: snapshot.context.coveredByLastMessage,
			};
	}
}

export const readStateIncomingMessageMachine = setup({
	types: {} as {
		context: ReadStateIncomingMessageInput;
		events: ReadStateIncomingMessageEvent;
		input: ReadStateIncomingMessageInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'incomingMessage.updated') return {};
			return event.input;
		}),
	},
	guards: {
		isCurrentUserMessage: ({context}) => context.isCurrentUserAuthor,
		shouldAutomaticallyAck: ({context}) => context.automaticAckEnabled && context.isAtBottom,
		shouldAckBlockedMessage: ({context}) => context.authorBlocked && !context.hadUnreadOrMentions,
		shouldIgnoreBlockedMessage: ({context}) => context.authorBlocked,
		isCoveredByAck: ({context}) => isCoveredByAck(context),
	},
}).createMachine({
	id: 'readStateIncomingMessage',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'isCurrentUserMessage', target: 'currentUserMessage'},
				{guard: 'shouldAutomaticallyAck', target: 'automaticAckMessage'},
				{guard: 'shouldAckBlockedMessage', target: 'blockedMessageAck'},
				{guard: 'shouldIgnoreBlockedMessage', target: 'blockedMessageIgnored'},
				{guard: 'isCoveredByAck', target: 'coveredByAck'},
				{target: 'unreadMessage'},
			],
		},
		currentUserMessage: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		automaticAckMessage: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		blockedMessageAck: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		blockedMessageIgnored: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		coveredByAck: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		unreadMessage: {
			on: {'incomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ReadStateIncomingMessageSnapshot = SnapshotFrom<typeof readStateIncomingMessageMachine>;

export function createReadStateIncomingMessageSnapshot(
	input: ReadStateIncomingMessageInput,
): ReadStateIncomingMessageSnapshot {
	return initialTransition(readStateIncomingMessageMachine, input)[0];
}

export function transitionReadStateIncomingMessageSnapshot(
	snapshot: ReadStateIncomingMessageSnapshot,
	event: ReadStateIncomingMessageEvent,
): ReadStateIncomingMessageSnapshot {
	return transition(readStateIncomingMessageMachine, snapshot, event)[0] as ReadStateIncomingMessageSnapshot;
}

export function selectReadStateIncomingMessageDecision(
	snapshot: ReadStateIncomingMessageSnapshot,
): ReadStateIncomingMessageDecision {
	return getDecision(snapshot);
}

export function resolveReadStateIncomingMessageDecision(
	input: ReadStateIncomingMessageInput,
): ReadStateIncomingMessageDecision {
	return selectReadStateIncomingMessageDecision(createReadStateIncomingMessageSnapshot(input));
}
