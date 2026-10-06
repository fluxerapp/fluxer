// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface ChannelIncomingMessageInput {
	hasNonceMatch: boolean;
	isUploadPlaceholder: boolean;
	hasMoreAfter: boolean;
	afterBufferAtBoundary: boolean;
}

export type ChannelIncomingMessageDecision =
	| {
			type: 'completeUploadPlaceholder';
	  }
	| {
			type: 'replaceNonceMessage';
	  }
	| {
			type: 'ignorePastVisibleWindow';
			shouldClearAfterBoundary: boolean;
	  }
	| {
			type: 'appendIncoming';
	  };

export type ChannelIncomingMessageEvent = {
	type: 'channelIncomingMessage.updated';
	input: ChannelIncomingMessageInput;
};

type ChannelIncomingMessageStateValue =
	| 'completeUploadPlaceholder'
	| 'replaceNonceMessage'
	| 'ignorePastVisibleWindow'
	| 'appendIncoming';

function resolveChannelIncomingMessageState(input: ChannelIncomingMessageInput): ChannelIncomingMessageStateValue {
	if (input.hasNonceMatch && input.isUploadPlaceholder) return 'completeUploadPlaceholder';
	if (input.hasNonceMatch) return 'replaceNonceMessage';
	if (input.hasMoreAfter) return 'ignorePastVisibleWindow';
	return 'appendIncoming';
}

function buildChannelIncomingMessageDecision(
	state: ChannelIncomingMessageStateValue,
	input: ChannelIncomingMessageInput,
): ChannelIncomingMessageDecision {
	switch (state) {
		case 'completeUploadPlaceholder':
			return {type: 'completeUploadPlaceholder'};
		case 'replaceNonceMessage':
			return {type: 'replaceNonceMessage'};
		case 'ignorePastVisibleWindow':
			return {
				type: 'ignorePastVisibleWindow',
				shouldClearAfterBoundary: input.afterBufferAtBoundary,
			};
		case 'appendIncoming':
			return {type: 'appendIncoming'};
	}
}

export const channelIncomingMessageMachine = setup({
	types: {} as {
		context: ChannelIncomingMessageInput;
		events: ChannelIncomingMessageEvent;
		input: ChannelIncomingMessageInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'channelIncomingMessage.updated') return {};
			return event.input;
		}),
	},
	guards: {
		shouldCompleteUploadPlaceholder: ({context}) => context.hasNonceMatch && context.isUploadPlaceholder,
		shouldReplaceNonceMessage: ({context}) => context.hasNonceMatch,
		shouldIgnorePastVisibleWindow: ({context}) => context.hasMoreAfter,
	},
}).createMachine({
	id: 'channelIncomingMessage',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'shouldCompleteUploadPlaceholder', target: 'completeUploadPlaceholder'},
				{guard: 'shouldReplaceNonceMessage', target: 'replaceNonceMessage'},
				{guard: 'shouldIgnorePastVisibleWindow', target: 'ignorePastVisibleWindow'},
				{target: 'appendIncoming'},
			],
		},
		completeUploadPlaceholder: {
			on: {'channelIncomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		replaceNonceMessage: {
			on: {'channelIncomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		ignorePastVisibleWindow: {
			on: {'channelIncomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
		appendIncoming: {
			on: {'channelIncomingMessage.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ChannelIncomingMessageSnapshot = SnapshotFrom<typeof channelIncomingMessageMachine>;

export function createChannelIncomingMessageSnapshot(
	input: ChannelIncomingMessageInput,
): ChannelIncomingMessageSnapshot {
	return initialTransition(channelIncomingMessageMachine, input)[0];
}

export function transitionChannelIncomingMessageSnapshot(
	snapshot: ChannelIncomingMessageSnapshot,
	event: ChannelIncomingMessageEvent,
): ChannelIncomingMessageSnapshot {
	return transition(channelIncomingMessageMachine, snapshot, event)[0] as ChannelIncomingMessageSnapshot;
}

export function selectChannelIncomingMessageDecision(
	snapshot: ChannelIncomingMessageSnapshot,
): ChannelIncomingMessageDecision {
	return buildChannelIncomingMessageDecision(snapshot.value as ChannelIncomingMessageStateValue, snapshot.context);
}

export function resolveChannelIncomingMessageDecision(
	input: ChannelIncomingMessageInput,
): ChannelIncomingMessageDecision {
	return buildChannelIncomingMessageDecision(resolveChannelIncomingMessageState(input), input);
}
