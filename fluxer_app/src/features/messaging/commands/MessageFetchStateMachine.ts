// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type MessageFetchCacheHit = 'jump' | 'before' | 'after';

export interface MessageFetchPreflightInput {
	hasInFlightRequest: boolean;
	shouldBlockForGate: boolean;
	cacheHit: MessageFetchCacheHit | null;
}

export type MessageFetchPreflightDecision =
	| {
			type: 'useInFlightRequest';
	  }
	| {
			type: 'blockForGate';
	  }
	| {
			type: 'useCache';
			cacheHit: MessageFetchCacheHit;
	  }
	| {
			type: 'startFetch';
	  };

export type MessageFetchPreflightEvent = {
	type: 'messageFetch.preflightChanged';
	input: MessageFetchPreflightInput;
};

export type MessageFetchPreflightState = 'inFlight' | 'blocked' | 'cached' | 'network';

export interface MessageFetchExecutionInput {
	forceFailure: boolean;
}

export type MessageFetchExecutionDecision =
	| {
			type: 'simulateFailure';
	  }
	| {
			type: 'requestNetwork';
	  };

export type MessageFetchExecutionEvent = {
	type: 'messageFetch.executionChanged';
	input: MessageFetchExecutionInput;
};

function resolvePreflightState(input: MessageFetchPreflightInput): MessageFetchPreflightState {
	if (input.hasInFlightRequest) return 'inFlight';
	if (input.shouldBlockForGate) return 'blocked';
	if (input.cacheHit != null) return 'cached';
	return 'network';
}

function buildPreflightDecision(
	state: MessageFetchPreflightState,
	input: MessageFetchPreflightInput,
): MessageFetchPreflightDecision {
	switch (state) {
		case 'inFlight':
			return {type: 'useInFlightRequest'};
		case 'blocked':
			return {type: 'blockForGate'};
		case 'cached':
			return {type: 'useCache', cacheHit: input.cacheHit as MessageFetchCacheHit};
		case 'network':
			return {type: 'startFetch'};
	}
}

function buildExecutionDecision(input: MessageFetchExecutionInput): MessageFetchExecutionDecision {
	return input.forceFailure ? {type: 'simulateFailure'} : {type: 'requestNetwork'};
}

export const messageFetchPreflightMachine = setup({
	types: {} as {
		context: MessageFetchPreflightInput;
		events: MessageFetchPreflightEvent;
		input: MessageFetchPreflightInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageFetch.preflightChanged') return {};
			return event.input;
		}),
	},
	guards: {
		hasInFlightRequest: ({context}) => context.hasInFlightRequest,
		shouldBlockForGate: ({context}) => context.shouldBlockForGate,
		hasCacheHit: ({context}) => context.cacheHit != null,
	},
}).createMachine({
	id: 'messageFetchPreflight',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'hasInFlightRequest', target: 'inFlight'},
				{guard: 'shouldBlockForGate', target: 'blocked'},
				{guard: 'hasCacheHit', target: 'cached'},
				{target: 'network'},
			],
		},
		inFlight: {
			on: {'messageFetch.preflightChanged': {target: 'routing', actions: 'applyInput'}},
		},
		blocked: {
			on: {'messageFetch.preflightChanged': {target: 'routing', actions: 'applyInput'}},
		},
		cached: {
			on: {'messageFetch.preflightChanged': {target: 'routing', actions: 'applyInput'}},
		},
		network: {
			on: {'messageFetch.preflightChanged': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export const messageFetchExecutionMachine = setup({
	types: {} as {
		context: MessageFetchExecutionInput;
		events: MessageFetchExecutionEvent;
		input: MessageFetchExecutionInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageFetch.executionChanged') return {};
			return event.input;
		}),
	},
	guards: {
		shouldSimulateFailure: ({context}) => context.forceFailure,
	},
}).createMachine({
	id: 'messageFetchExecution',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'shouldSimulateFailure', target: 'forcedFailure'}, {target: 'network'}],
		},
		forcedFailure: {
			on: {'messageFetch.executionChanged': {target: 'routing', actions: 'applyInput'}},
		},
		network: {
			on: {'messageFetch.executionChanged': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type MessageFetchPreflightSnapshot = SnapshotFrom<typeof messageFetchPreflightMachine>;
export type MessageFetchExecutionSnapshot = SnapshotFrom<typeof messageFetchExecutionMachine>;

export function createMessageFetchPreflightSnapshot(input: MessageFetchPreflightInput): MessageFetchPreflightSnapshot {
	return initialTransition(messageFetchPreflightMachine, input)[0];
}

export function transitionMessageFetchPreflightSnapshot(
	snapshot: MessageFetchPreflightSnapshot,
	event: MessageFetchPreflightEvent,
): MessageFetchPreflightSnapshot {
	return transition(messageFetchPreflightMachine, snapshot, event)[0] as MessageFetchPreflightSnapshot;
}

export function selectMessageFetchPreflightDecision(
	snapshot: MessageFetchPreflightSnapshot,
): MessageFetchPreflightDecision {
	return buildPreflightDecision(snapshot.value as MessageFetchPreflightState, snapshot.context);
}

export function resolveMessageFetchPreflightDecision(input: MessageFetchPreflightInput): MessageFetchPreflightDecision {
	return buildPreflightDecision(resolvePreflightState(input), input);
}

export function createMessageFetchExecutionSnapshot(input: MessageFetchExecutionInput): MessageFetchExecutionSnapshot {
	return initialTransition(messageFetchExecutionMachine, input)[0];
}

export function transitionMessageFetchExecutionSnapshot(
	snapshot: MessageFetchExecutionSnapshot,
	event: MessageFetchExecutionEvent,
): MessageFetchExecutionSnapshot {
	return transition(messageFetchExecutionMachine, snapshot, event)[0] as MessageFetchExecutionSnapshot;
}

export function selectMessageFetchExecutionDecision(
	snapshot: MessageFetchExecutionSnapshot,
): MessageFetchExecutionDecision {
	return buildExecutionDecision(snapshot.context);
}

export function resolveMessageFetchExecutionDecision(input: MessageFetchExecutionInput): MessageFetchExecutionDecision {
	return buildExecutionDecision(input);
}

export interface MessageFetchWindowTrustInput {
	connectedAtRequest: boolean;
	connectedAtResponse: boolean;
	epochAtRequest: number;
	epochAtResponse: number;
}

export function resolveMessageFetchWindowCached(input: MessageFetchWindowTrustInput): boolean {
	if (!input.connectedAtRequest || !input.connectedAtResponse) return true;
	return input.epochAtRequest !== input.epochAtResponse;
}
