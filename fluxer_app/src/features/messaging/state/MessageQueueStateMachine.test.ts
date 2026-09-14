// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMessageLocalSendRateLimitSnapshot,
	createMessageQueuePayloadRouteSnapshot,
	createMessageQueueRequestOutcomeSnapshot,
	createMessageQueueSendExecutionSnapshot,
	type MessageLocalSendRateLimitInput,
	resolveMessageLocalSendRateLimitDecision,
	resolveMessageQueuePayloadRouteDecision,
	resolveMessageQueueRequestOutcomeDecision,
	resolveMessageQueueSendExecutionDecision,
	selectMessageLocalSendRateLimitDecision,
	selectMessageQueuePayloadRouteDecision,
	selectMessageQueueRequestOutcomeDecision,
	selectMessageQueueSendExecutionDecision,
	transitionMessageLocalSendRateLimitSnapshot,
	transitionMessageQueuePayloadRouteSnapshot,
	transitionMessageQueueRequestOutcomeSnapshot,
	transitionMessageQueueSendExecutionSnapshot,
} from '@app/features/messaging/state/MessageQueueStateMachine';
import {describe, expect, it} from 'vitest';

function localLimiter(overrides: Partial<MessageLocalSendRateLimitInput> = {}): MessageLocalSendRateLimitInput {
	return {
		now: 1_000,
		windowStartedAt: null,
		sentCount: 0,
		blockedUntil: null,
		maxSends: 5,
		windowMs: 2_000,
		blockMs: 3_000,
		...overrides,
	};
}

describe('messageQueuePayloadRouteMachine', () => {
	it('routes known queue payload types', () => {
		expect(resolveMessageQueuePayloadRouteDecision({payloadType: 'send'})).toEqual({type: 'send'});
		expect(resolveMessageQueuePayloadRouteDecision({payloadType: 'edit'})).toEqual({type: 'edit'});
	});

	it('routes unknown payload types to an explicit fallback', () => {
		expect(resolveMessageQueuePayloadRouteDecision({payloadType: 'delete'})).toEqual({type: 'unknown'});
		expect(resolveMessageQueuePayloadRouteDecision({})).toEqual({type: 'unknown'});
	});

	it('re-routes when the payload type changes', () => {
		const sendSnapshot = createMessageQueuePayloadRouteSnapshot({payloadType: 'send'});
		expect(selectMessageQueuePayloadRouteDecision(sendSnapshot)).toEqual({type: 'send'});

		const editSnapshot = transitionMessageQueuePayloadRouteSnapshot(sendSnapshot, {
			type: 'messageQueue.payloadChanged',
			input: {payloadType: 'edit'},
		});

		expect(selectMessageQueuePayloadRouteDecision(editSnapshot)).toEqual({type: 'edit'});
	});
});

describe('messageQueueSendExecutionMachine', () => {
	it('requests the network by default', () => {
		expect(resolveMessageQueueSendExecutionDecision({forceFailure: false})).toEqual({type: 'requestNetwork'});
	});

	it('routes developer-forced failures before network requests', () => {
		expect(resolveMessageQueueSendExecutionDecision({forceFailure: true})).toEqual({type: 'simulateFailure'});
	});

	it('re-routes when execution inputs change', () => {
		const forcedSnapshot = createMessageQueueSendExecutionSnapshot({forceFailure: true});
		expect(selectMessageQueueSendExecutionDecision(forcedSnapshot)).toEqual({type: 'simulateFailure'});

		const networkSnapshot = transitionMessageQueueSendExecutionSnapshot(forcedSnapshot, {
			type: 'messageQueue.sendExecutionChanged',
			input: {forceFailure: false},
		});

		expect(selectMessageQueueSendExecutionDecision(networkSnapshot)).toEqual({type: 'requestNetwork'});
	});
});

describe('messageQueueRequestOutcomeMachine', () => {
	it('routes request outcomes into queue actions', () => {
		expect(resolveMessageQueueRequestOutcomeDecision({status: 'success'})).toEqual({type: 'completeSuccess'});
		expect(resolveMessageQueueRequestOutcomeDecision({status: 'rateLimit'})).toEqual({type: 'retryRateLimit'});
		expect(resolveMessageQueueRequestOutcomeDecision({status: 'failure'})).toEqual({type: 'completeFailure'});
	});

	it('re-routes when the outcome status changes', () => {
		const failureSnapshot = createMessageQueueRequestOutcomeSnapshot({status: 'failure'});
		expect(selectMessageQueueRequestOutcomeDecision(failureSnapshot)).toEqual({type: 'completeFailure'});

		const successSnapshot = transitionMessageQueueRequestOutcomeSnapshot(failureSnapshot, {
			type: 'messageQueue.requestOutcomeChanged',
			input: {status: 'success'},
		});

		expect(selectMessageQueueRequestOutcomeDecision(successSnapshot)).toEqual({type: 'completeSuccess'});
	});
});

describe('messageLocalSendRateLimitMachine', () => {
	it('allows the first send and starts a local send window', () => {
		expect(resolveMessageLocalSendRateLimitDecision(localLimiter())).toEqual({
			type: 'allow',
			next: {
				windowStartedAt: 1_000,
				sentCount: 1,
				blockedUntil: null,
			},
		});
	});

	it('increments allowed sends within the active window', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 1_500,
					windowStartedAt: 1_000,
					sentCount: 2,
				}),
			),
		).toEqual({
			type: 'allow',
			next: {
				windowStartedAt: 1_000,
				sentCount: 3,
				blockedUntil: null,
			},
		});
	});

	it('blocks the first attempt above the max send count', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 1_800,
					windowStartedAt: 1_000,
					sentCount: 5,
				}),
			),
		).toEqual({
			type: 'block',
			retryAfterMs: 3_000,
			next: {
				windowStartedAt: 1_800,
				sentCount: 0,
				blockedUntil: 4_800,
			},
		});
	});

	it('keeps blocking attempts while a local block is active', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 2_500,
					windowStartedAt: 1_800,
					sentCount: 0,
					blockedUntil: 4_800,
				}),
			),
		).toEqual({
			type: 'block',
			retryAfterMs: 2_300,
			next: {
				windowStartedAt: 1_800,
				sentCount: 0,
				blockedUntil: 4_800,
			},
		});
	});

	it('allows sends after the local block timer expires', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 5_000,
					windowStartedAt: 1_800,
					sentCount: 0,
					blockedUntil: 4_800,
				}),
			),
		).toEqual({
			type: 'allow',
			next: {
				windowStartedAt: 5_000,
				sentCount: 1,
				blockedUntil: null,
			},
		});
	});

	it('allows sends from an empty limiter state', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 5_000,
					windowStartedAt: null,
					sentCount: 0,
					blockedUntil: null,
				}),
			),
		).toEqual({
			type: 'allow',
			next: {
				windowStartedAt: 5_000,
				sentCount: 1,
				blockedUntil: null,
			},
		});
	});

	it('resets the counter when the send window expires before the max is reached', () => {
		expect(
			resolveMessageLocalSendRateLimitDecision(
				localLimiter({
					now: 3_100,
					windowStartedAt: 1_000,
					sentCount: 4,
				}),
			),
		).toEqual({
			type: 'allow',
			next: {
				windowStartedAt: 3_100,
				sentCount: 1,
				blockedUntil: null,
			},
		});
	});

	it('re-routes when limiter inputs change', () => {
		const allowedSnapshot = createMessageLocalSendRateLimitSnapshot(localLimiter());
		expect(selectMessageLocalSendRateLimitDecision(allowedSnapshot)).toMatchObject({type: 'allow'});

		const blockedSnapshot = transitionMessageLocalSendRateLimitSnapshot(allowedSnapshot, {
			type: 'messageQueue.localSendAttempted',
			input: localLimiter({
				windowStartedAt: 1_000,
				sentCount: 5,
			}),
		});

		expect(selectMessageLocalSendRateLimitDecision(blockedSnapshot)).toMatchObject({
			type: 'block',
			retryAfterMs: 3_000,
		});
	});
});
