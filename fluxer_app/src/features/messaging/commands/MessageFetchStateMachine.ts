export type MessageFetchCacheHit = 'jump' | 'before' | 'after';

export interface MessageFetchPreflightInput {
	hasInFlightRequest: boolean;
	accountTransitionActive: boolean;
	shouldBlockForGate: boolean;
	cacheHit: MessageFetchCacheHit | null;
}

export type MessageFetchPreflightDecision =
	| {
			type: 'useInFlightRequest';
	  }
	| {
			type: 'waitForAccountTransition';
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

export type MessageFetchPreflightState = 'inFlight' | 'accountTransition' | 'blocked' | 'cached' | 'network';

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

function resolvePreflightState(input: MessageFetchPreflightInput): MessageFetchPreflightState {
	if (input.hasInFlightRequest) return 'inFlight';
	if (input.accountTransitionActive) return 'accountTransition';
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
		case 'accountTransition':
			return {type: 'waitForAccountTransition'};
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

export function resolveMessageFetchPreflightDecision(input: MessageFetchPreflightInput): MessageFetchPreflightDecision {
	return buildPreflightDecision(resolvePreflightState(input), input);
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
