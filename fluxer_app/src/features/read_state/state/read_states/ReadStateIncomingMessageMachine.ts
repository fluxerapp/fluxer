// SPDX-License-Identifier: AGPL-3.0-or-later

import {compareMessageIds} from '@app/features/read_state/state/read_states/shared';
import {assign, initialTransition, type SnapshotFrom, setup} from 'xstate';

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

type ReadStateIncomingMessageEvent = {
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

const readStateIncomingMessageMachine = setup({
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

type ReadStateIncomingMessageSnapshot = SnapshotFrom<typeof readStateIncomingMessageMachine>;

function createReadStateIncomingMessageSnapshot(
	input: ReadStateIncomingMessageInput,
): ReadStateIncomingMessageSnapshot {
	return initialTransition(readStateIncomingMessageMachine, input)[0];
}

function selectReadStateIncomingMessageDecision(
	snapshot: ReadStateIncomingMessageSnapshot,
): ReadStateIncomingMessageDecision {
	return getDecision(snapshot);
}

export function resolveReadStateIncomingMessageDecision(
	input: ReadStateIncomingMessageInput,
): ReadStateIncomingMessageDecision {
	return selectReadStateIncomingMessageDecision(createReadStateIncomingMessageSnapshot(input));
}
