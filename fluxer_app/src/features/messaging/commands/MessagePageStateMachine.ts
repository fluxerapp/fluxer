// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AroundPaginationState,
	calculateAroundPaginationState,
} from '@app/features/messaging/utils/MessagePaginationUtils';
import {compare as compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeUtils';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export interface MessagePageStateInput {
	before: string | null;
	after: string | null;
	limit: number;
	messageCount: number;
	aroundMessageId: string | null;
	aroundTargetIndex: number;
	newestFetchedMessageId: string | null;
	knownLatestMessageId: string | null;
}

export interface MessagePageStateModel {
	isBefore: boolean;
	isAfter: boolean;
	hasMoreBefore: boolean;
	hasMoreAfter: boolean;
	shouldWarnMissingAroundTarget: boolean;
	aroundDebug: {
		messagesNewerThanTarget: number;
		messagesOlderThanTarget: number;
		expectedNewer: number;
		expectedOlder: number;
		pageFilled: boolean;
	} | null;
}

export type MessagePageStateEvent = {
	type: 'messagePageState.updated';
	input: MessagePageStateInput;
};

function getBasePageState(
	context: MessagePageStateInput,
): Pick<MessagePageStateModel, 'isBefore' | 'isAfter' | 'hasMoreBefore' | 'hasMoreAfter'> {
	const isBefore = context.before != null;
	const isAfter = context.after != null;
	const isReplacement = !isBefore && !isAfter;
	const pageFilled = context.messageCount === context.limit;
	return {
		isBefore,
		isAfter,
		hasMoreBefore: pageFilled && (isBefore || isReplacement),
		hasMoreAfter: isAfter && pageFilled,
	};
}

type MessagePageStateValue = 'aroundMissing' | 'aroundFound' | 'standard';

function resolveMessagePageStateValue(input: MessagePageStateInput): MessagePageStateValue {
	if (input.aroundMessageId != null && input.aroundTargetIndex === -1) return 'aroundMissing';
	if (input.aroundMessageId != null) return 'aroundFound';
	return 'standard';
}

function getMissingAroundPageState(
	input: MessagePageStateInput,
	aroundState: AroundPaginationState,
): Pick<MessagePageStateModel, 'hasMoreBefore' | 'hasMoreAfter'> {
	if (input.messageCount === 0) {
		return {hasMoreBefore: false, hasMoreAfter: false};
	}
	const fetchedPastTarget =
		input.newestFetchedMessageId == null || compareSnowflakes(input.newestFetchedMessageId, input.aroundMessageId) > 0;
	return {
		hasMoreBefore: aroundState.hasMoreBefore,
		hasMoreAfter: aroundState.expectedNewer > 0 && fetchedPastTarget && !aroundState.isAtKnownLatest,
	};
}

function buildMessagePageState(state: MessagePageStateValue, input: MessagePageStateInput): MessagePageStateModel {
	const base = getBasePageState(input);
	if (state === 'standard') {
		return {
			...base,
			shouldWarnMissingAroundTarget: false,
			aroundDebug: null,
		};
	}
	const aroundState = calculateAroundPaginationState({
		limit: input.limit,
		messageCount: input.messageCount,
		targetIndex: input.aroundTargetIndex,
		newestFetchedMessageId: input.newestFetchedMessageId,
		knownLatestMessageId: input.knownLatestMessageId,
	});
	if (state === 'aroundMissing') {
		return {
			...base,
			...getMissingAroundPageState(input, aroundState),
			shouldWarnMissingAroundTarget: true,
			aroundDebug: null,
		};
	}
	return {
		...base,
		hasMoreBefore: aroundState.hasMoreBefore,
		hasMoreAfter: aroundState.hasMoreAfter,
		shouldWarnMissingAroundTarget: false,
		aroundDebug: {
			messagesNewerThanTarget: aroundState.messagesNewer,
			messagesOlderThanTarget: aroundState.messagesOlder,
			expectedNewer: aroundState.expectedNewer,
			expectedOlder: aroundState.expectedOlder,
			pageFilled: input.messageCount === input.limit,
		},
	};
}

export const messagePageStateMachine = setup({
	types: {} as {
		context: MessagePageStateInput;
		events: MessagePageStateEvent;
		input: MessagePageStateInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messagePageState.updated') return {};
			return event.input;
		}),
	},
	guards: {
		hasMissingAroundTarget: ({context}) => context.aroundMessageId != null && context.aroundTargetIndex === -1,
		hasAroundTarget: ({context}) => context.aroundMessageId != null,
	},
}).createMachine({
	id: 'messagePageState',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'hasMissingAroundTarget', target: 'aroundMissing'},
				{guard: 'hasAroundTarget', target: 'aroundFound'},
				{target: 'standard'},
			],
		},
		aroundMissing: {
			on: {'messagePageState.updated': {target: 'routing', actions: 'applyInput'}},
		},
		aroundFound: {
			on: {'messagePageState.updated': {target: 'routing', actions: 'applyInput'}},
		},
		standard: {
			on: {'messagePageState.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type MessagePageStateSnapshot = SnapshotFrom<typeof messagePageStateMachine>;

export function createMessagePageStateSnapshot(input: MessagePageStateInput): MessagePageStateSnapshot {
	return getInitialSnapshot(messagePageStateMachine, input);
}

export function transitionMessagePageStateSnapshot(
	snapshot: MessagePageStateSnapshot,
	event: MessagePageStateEvent,
): MessagePageStateSnapshot {
	return transition(messagePageStateMachine, snapshot, event)[0] as MessagePageStateSnapshot;
}

export function selectMessagePageState(snapshot: MessagePageStateSnapshot): MessagePageStateModel {
	return buildMessagePageState(snapshot.value as MessagePageStateValue, snapshot.context);
}

export function resolveMessagePageState(input: MessagePageStateInput): MessagePageStateModel {
	return buildMessagePageState(resolveMessagePageStateValue(input), input);
}
