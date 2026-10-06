// SPDX-License-Identifier: AGPL-3.0-or-later

export const DISPATCH_FLUSH_DELAY_MS = 50;
export const DISPATCH_IDLE_TIMEOUT_MS = 1000;
export const DISPATCH_IDLE_RETRY_TIMEOUT_MS = 200;
export const DISPATCH_IDLE_MIN_BUDGET_MS = 1000 / 60 / 8;

const IMMEDIATE_DISPATCH_TYPES = new Set(['READY', 'RESUMED']);

const CRITICAL_DISPATCH_TYPES = new Set([
	'READY',
	'RESUMED',
	'VOICE_STATE_UPDATE',
	'VOICE_SERVER_UPDATE',
	'CALL_CREATE',
	'CALL_UPDATE',
	'CALL_DELETE',
	'SESSIONS_REPLACE',
	'STREAM_CREATE',
	'STREAM_UPDATE',
	'STREAM_DELETE',
	'STREAM_SERVER_UPDATE',
]);

export type GatewayDispatchFlushMode = 'immediate' | 'deferred';

export function selectGatewayDispatchFlushMode(dispatchType: string | null): GatewayDispatchFlushMode {
	if (dispatchType == null) return 'immediate';
	return IMMEDIATE_DISPATCH_TYPES.has(dispatchType) ? 'immediate' : 'deferred';
}

export function isCriticalGatewayDispatch(dispatchType: string | null): boolean {
	return dispatchType != null && CRITICAL_DISPATCH_TYPES.has(dispatchType);
}

export function shouldSkipIdleWait(criticalWorkScheduled: boolean, idleCallbackSupported: boolean): boolean {
	return criticalWorkScheduled || !idleCallbackSupported;
}

export function shouldRetryIdleWait(didTimeout: boolean, timeRemainingMs: number): boolean {
	return !didTimeout && timeRemainingMs < DISPATCH_IDLE_MIN_BUDGET_MS;
}
