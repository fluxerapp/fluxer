// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DISPATCH_IDLE_MIN_BUDGET_MS,
	isCriticalGatewayDispatch,
	selectGatewayDispatchFlushMode,
	shouldRetryIdleWait,
	shouldSkipIdleWait,
} from '@app/features/gateway/transport/GatewayDispatchScheduling';
import {describe, expect, it} from 'vitest';

describe('selectGatewayDispatchFlushMode', () => {
	it('flushes transport events that carry no dispatch type immediately', () => {
		expect(selectGatewayDispatchFlushMode(null)).toBe('immediate');
	});

	it('flushes the session-establishing dispatches immediately', () => {
		expect(selectGatewayDispatchFlushMode('READY')).toBe('immediate');
		expect(selectGatewayDispatchFlushMode('RESUMED')).toBe('immediate');
	});

	it('defers ordinary dispatches so an in-flight REST page commits first', () => {
		expect(selectGatewayDispatchFlushMode('MESSAGE_CREATE')).toBe('deferred');
		expect(selectGatewayDispatchFlushMode('TYPING_START')).toBe('deferred');
		expect(selectGatewayDispatchFlushMode('PRESENCE_UPDATE')).toBe('deferred');
	});
});

describe('isCriticalGatewayDispatch', () => {
	it('treats voice and call dispatches as critical', () => {
		expect(isCriticalGatewayDispatch('VOICE_STATE_UPDATE')).toBe(true);
		expect(isCriticalGatewayDispatch('VOICE_SERVER_UPDATE')).toBe(true);
		expect(isCriticalGatewayDispatch('CALL_CREATE')).toBe(true);
		expect(isCriticalGatewayDispatch('SESSIONS_REPLACE')).toBe(true);
	});

	it('does not treat ordinary dispatches as critical', () => {
		expect(isCriticalGatewayDispatch('MESSAGE_CREATE')).toBe(false);
		expect(isCriticalGatewayDispatch('TYPING_START')).toBe(false);
		expect(isCriticalGatewayDispatch(null)).toBe(false);
	});
});

describe('shouldSkipIdleWait', () => {
	it('skips the idle wait when critical work is queued', () => {
		expect(shouldSkipIdleWait(true, true)).toBe(true);
	});

	it('skips the idle wait when the environment has no idle callback', () => {
		expect(shouldSkipIdleWait(false, false)).toBe(true);
	});

	it('waits for idle for ordinary work in a capable environment', () => {
		expect(shouldSkipIdleWait(false, true)).toBe(false);
	});
});

describe('shouldRetryIdleWait', () => {
	it('does not retry once the idle callback has timed out', () => {
		expect(shouldRetryIdleWait(true, 0)).toBe(false);
	});

	it('retries when the granted budget is below the minimum', () => {
		expect(shouldRetryIdleWait(false, DISPATCH_IDLE_MIN_BUDGET_MS / 2)).toBe(true);
	});

	it('flushes when the granted budget is sufficient', () => {
		expect(shouldRetryIdleWait(false, DISPATCH_IDLE_MIN_BUDGET_MS)).toBe(false);
		expect(shouldRetryIdleWait(false, 50)).toBe(false);
	});
});
