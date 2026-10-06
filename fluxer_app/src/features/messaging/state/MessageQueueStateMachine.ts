// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface MessageQueuePayloadRouteInput {
	payloadType?: string;
}

export type MessageQueuePayloadRouteDecision = {type: 'send'} | {type: 'edit'} | {type: 'unknown'};

export type MessageQueuePayloadRouteEvent = {
	type: 'messageQueue.payloadChanged';
	input: MessageQueuePayloadRouteInput;
};

export interface MessageQueueSendExecutionInput {
	forceFailure: boolean;
}

export type MessageQueueSendExecutionDecision = {type: 'simulateFailure'} | {type: 'requestNetwork'};

export type MessageQueueSendExecutionEvent = {
	type: 'messageQueue.sendExecutionChanged';
	input: MessageQueueSendExecutionInput;
};

export type MessageQueueRequestOutcomeStatus = 'success' | 'rateLimit' | 'failure';

export interface MessageQueueRequestOutcomeInput {
	status: MessageQueueRequestOutcomeStatus;
}

export type MessageQueueRequestOutcomeDecision =
	| {type: 'completeSuccess'}
	| {type: 'retryRateLimit'}
	| {type: 'completeFailure'};

export type MessageQueueRequestOutcomeEvent = {
	type: 'messageQueue.requestOutcomeChanged';
	input: MessageQueueRequestOutcomeInput;
};

export interface MessageLocalSendRateLimitState {
	windowStartedAt: number | null;
	sentCount: number;
	blockedUntil: number | null;
}

export interface MessageLocalSendRateLimitInput extends MessageLocalSendRateLimitState {
	now: number;
	maxSends: number;
	windowMs: number;
	blockMs: number;
}

export type MessageLocalSendRateLimitDecision =
	| {
			type: 'allow';
			next: MessageLocalSendRateLimitState;
	  }
	| {
			type: 'block';
			retryAfterMs: number;
			next: MessageLocalSendRateLimitState;
	  };

export type MessageLocalSendRateLimitEvent = {
	type: 'messageQueue.localSendAttempted';
	input: MessageLocalSendRateLimitInput;
};

function isTimedBlockActive(context: MessageLocalSendRateLimitInput): boolean {
	return context.blockedUntil != null && context.now < context.blockedUntil;
}

function isWindowExpired(context: MessageLocalSendRateLimitInput): boolean {
	if (context.windowStartedAt == null) return true;
	return context.now - context.windowStartedAt >= context.windowMs;
}

function shouldBlockNewAttempt(context: MessageLocalSendRateLimitInput): boolean {
	if (context.maxSends <= 0) return true;
	if (isWindowExpired(context)) return false;
	return context.sentCount >= context.maxSends;
}

function getCurrentRateLimitState(context: MessageLocalSendRateLimitInput): MessageLocalSendRateLimitState {
	return {
		windowStartedAt: context.windowStartedAt,
		sentCount: context.sentCount,
		blockedUntil: context.blockedUntil,
	};
}

function getAllowedRateLimitState(context: MessageLocalSendRateLimitInput): MessageLocalSendRateLimitState {
	if (isWindowExpired(context)) {
		return {
			windowStartedAt: context.now,
			sentCount: 1,
			blockedUntil: null,
		};
	}
	return {
		windowStartedAt: context.windowStartedAt,
		sentCount: context.sentCount + 1,
		blockedUntil: null,
	};
}

function getNewBlockedRateLimitState(context: MessageLocalSendRateLimitInput): MessageLocalSendRateLimitState {
	return {
		windowStartedAt: context.now,
		sentCount: 0,
		blockedUntil: context.now + Math.max(0, context.blockMs),
	};
}

function getBlockRetryAfterMs(context: MessageLocalSendRateLimitInput): number {
	if (isTimedBlockActive(context)) {
		return Math.max(0, (context.blockedUntil ?? context.now) - context.now);
	}
	return Math.max(0, context.blockMs);
}

export const messageQueuePayloadRouteMachine = setup({
	types: {} as {
		context: MessageQueuePayloadRouteInput;
		events: MessageQueuePayloadRouteEvent;
		input: MessageQueuePayloadRouteInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageQueue.payloadChanged') return {};
			return event.input;
		}),
	},
	guards: {
		isSendPayload: ({context}) => context.payloadType === 'send',
		isEditPayload: ({context}) => context.payloadType === 'edit',
	},
}).createMachine({
	id: 'messageQueuePayloadRoute',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'isSendPayload', target: 'send'}, {guard: 'isEditPayload', target: 'edit'}, {target: 'unknown'}],
		},
		send: {
			on: {'messageQueue.payloadChanged': {target: 'routing', actions: 'applyInput'}},
		},
		edit: {
			on: {'messageQueue.payloadChanged': {target: 'routing', actions: 'applyInput'}},
		},
		unknown: {
			on: {'messageQueue.payloadChanged': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export const messageQueueSendExecutionMachine = setup({
	types: {} as {
		context: MessageQueueSendExecutionInput;
		events: MessageQueueSendExecutionEvent;
		input: MessageQueueSendExecutionInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageQueue.sendExecutionChanged') return {};
			return event.input;
		}),
	},
	guards: {
		shouldSimulateFailure: ({context}) => context.forceFailure,
	},
}).createMachine({
	id: 'messageQueueSendExecution',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'shouldSimulateFailure', target: 'forcedFailure'}, {target: 'network'}],
		},
		forcedFailure: {
			on: {'messageQueue.sendExecutionChanged': {target: 'routing', actions: 'applyInput'}},
		},
		network: {
			on: {'messageQueue.sendExecutionChanged': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export const messageQueueRequestOutcomeMachine = setup({
	types: {} as {
		context: MessageQueueRequestOutcomeInput;
		events: MessageQueueRequestOutcomeEvent;
		input: MessageQueueRequestOutcomeInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageQueue.requestOutcomeChanged') return {};
			return event.input;
		}),
	},
	guards: {
		isSuccess: ({context}) => context.status === 'success',
		isRateLimit: ({context}) => context.status === 'rateLimit',
	},
}).createMachine({
	id: 'messageQueueRequestOutcome',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'isSuccess', target: 'success'},
				{guard: 'isRateLimit', target: 'rateLimited'},
				{target: 'failure'},
			],
		},
		success: {
			on: {'messageQueue.requestOutcomeChanged': {target: 'routing', actions: 'applyInput'}},
		},
		rateLimited: {
			on: {'messageQueue.requestOutcomeChanged': {target: 'routing', actions: 'applyInput'}},
		},
		failure: {
			on: {'messageQueue.requestOutcomeChanged': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export const messageLocalSendRateLimitMachine = setup({
	types: {} as {
		context: MessageLocalSendRateLimitInput;
		events: MessageLocalSendRateLimitEvent;
		input: MessageLocalSendRateLimitInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'messageQueue.localSendAttempted') return {};
			return event.input;
		}),
	},
	guards: {
		isTimedBlockActive: ({context}) => isTimedBlockActive(context),
		shouldBlockNewAttempt: ({context}) => shouldBlockNewAttempt(context),
	},
}).createMachine({
	id: 'messageLocalSendRateLimit',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'isTimedBlockActive', target: 'blocked'},
				{guard: 'shouldBlockNewAttempt', target: 'blocked'},
				{target: 'allowed'},
			],
		},
		allowed: {
			on: {'messageQueue.localSendAttempted': {target: 'routing', actions: 'applyInput'}},
		},
		blocked: {
			on: {'messageQueue.localSendAttempted': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type MessageQueuePayloadRouteSnapshot = SnapshotFrom<typeof messageQueuePayloadRouteMachine>;
export type MessageQueueSendExecutionSnapshot = SnapshotFrom<typeof messageQueueSendExecutionMachine>;
export type MessageQueueRequestOutcomeSnapshot = SnapshotFrom<typeof messageQueueRequestOutcomeMachine>;
export type MessageLocalSendRateLimitSnapshot = SnapshotFrom<typeof messageLocalSendRateLimitMachine>;

export function createMessageQueuePayloadRouteSnapshot(
	input: MessageQueuePayloadRouteInput,
): MessageQueuePayloadRouteSnapshot {
	return initialTransition(messageQueuePayloadRouteMachine, input)[0];
}

export function transitionMessageQueuePayloadRouteSnapshot(
	snapshot: MessageQueuePayloadRouteSnapshot,
	event: MessageQueuePayloadRouteEvent,
): MessageQueuePayloadRouteSnapshot {
	return transition(messageQueuePayloadRouteMachine, snapshot, event)[0] as MessageQueuePayloadRouteSnapshot;
}

export function selectMessageQueuePayloadRouteDecision(
	snapshot: MessageQueuePayloadRouteSnapshot,
): MessageQueuePayloadRouteDecision {
	switch (snapshot.value) {
		case 'send':
			return {type: 'send'};
		case 'edit':
			return {type: 'edit'};
		default:
			return {type: 'unknown'};
	}
}

export function resolveMessageQueuePayloadRouteDecision(
	input: MessageQueuePayloadRouteInput,
): MessageQueuePayloadRouteDecision {
	return selectMessageQueuePayloadRouteDecision(createMessageQueuePayloadRouteSnapshot(input));
}

export function createMessageQueueSendExecutionSnapshot(
	input: MessageQueueSendExecutionInput,
): MessageQueueSendExecutionSnapshot {
	return initialTransition(messageQueueSendExecutionMachine, input)[0];
}

export function transitionMessageQueueSendExecutionSnapshot(
	snapshot: MessageQueueSendExecutionSnapshot,
	event: MessageQueueSendExecutionEvent,
): MessageQueueSendExecutionSnapshot {
	return transition(messageQueueSendExecutionMachine, snapshot, event)[0] as MessageQueueSendExecutionSnapshot;
}

export function selectMessageQueueSendExecutionDecision(
	snapshot: MessageQueueSendExecutionSnapshot,
): MessageQueueSendExecutionDecision {
	switch (snapshot.value) {
		case 'forcedFailure':
			return {type: 'simulateFailure'};
		default:
			return {type: 'requestNetwork'};
	}
}

export function resolveMessageQueueSendExecutionDecision(
	input: MessageQueueSendExecutionInput,
): MessageQueueSendExecutionDecision {
	return selectMessageQueueSendExecutionDecision(createMessageQueueSendExecutionSnapshot(input));
}

export function createMessageQueueRequestOutcomeSnapshot(
	input: MessageQueueRequestOutcomeInput,
): MessageQueueRequestOutcomeSnapshot {
	return initialTransition(messageQueueRequestOutcomeMachine, input)[0];
}

export function transitionMessageQueueRequestOutcomeSnapshot(
	snapshot: MessageQueueRequestOutcomeSnapshot,
	event: MessageQueueRequestOutcomeEvent,
): MessageQueueRequestOutcomeSnapshot {
	return transition(messageQueueRequestOutcomeMachine, snapshot, event)[0] as MessageQueueRequestOutcomeSnapshot;
}

export function selectMessageQueueRequestOutcomeDecision(
	snapshot: MessageQueueRequestOutcomeSnapshot,
): MessageQueueRequestOutcomeDecision {
	switch (snapshot.value) {
		case 'success':
			return {type: 'completeSuccess'};
		case 'rateLimited':
			return {type: 'retryRateLimit'};
		default:
			return {type: 'completeFailure'};
	}
}

export function resolveMessageQueueRequestOutcomeDecision(
	input: MessageQueueRequestOutcomeInput,
): MessageQueueRequestOutcomeDecision {
	return selectMessageQueueRequestOutcomeDecision(createMessageQueueRequestOutcomeSnapshot(input));
}

export function createMessageLocalSendRateLimitSnapshot(
	input: MessageLocalSendRateLimitInput,
): MessageLocalSendRateLimitSnapshot {
	return initialTransition(messageLocalSendRateLimitMachine, input)[0];
}

export function transitionMessageLocalSendRateLimitSnapshot(
	snapshot: MessageLocalSendRateLimitSnapshot,
	event: MessageLocalSendRateLimitEvent,
): MessageLocalSendRateLimitSnapshot {
	return transition(messageLocalSendRateLimitMachine, snapshot, event)[0] as MessageLocalSendRateLimitSnapshot;
}

export function selectMessageLocalSendRateLimitDecision(
	snapshot: MessageLocalSendRateLimitSnapshot,
): MessageLocalSendRateLimitDecision {
	switch (snapshot.value) {
		case 'blocked':
			if (isTimedBlockActive(snapshot.context)) {
				return {
					type: 'block',
					retryAfterMs: getBlockRetryAfterMs(snapshot.context),
					next: getCurrentRateLimitState(snapshot.context),
				};
			}
			return {
				type: 'block',
				retryAfterMs: getBlockRetryAfterMs(snapshot.context),
				next: getNewBlockedRateLimitState(snapshot.context),
			};
		default:
			return {
				type: 'allow',
				next: getAllowedRateLimitState(snapshot.context),
			};
	}
}

export function resolveMessageLocalSendRateLimitDecision(
	input: MessageLocalSendRateLimitInput,
): MessageLocalSendRateLimitDecision {
	return selectMessageLocalSendRateLimitDecision(createMessageLocalSendRateLimitSnapshot(input));
}
