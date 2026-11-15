// SPDX-License-Identifier: AGPL-3.0-or-later

export const MAX_RECONNECT_ATTEMPTS = 8;
export type SourceFault = 'captureDeviceLost' | 'gpuDeviceLost' | 'encoderError' | 'networkError' | 'decoderError';

export type SourceLifecycleState =
	| {kind: 'active'; since: bigint}
	| {kind: 'reconnecting'; since: bigint; attempts: number; lastFault: SourceFault}
	| {kind: 'failed'; since: bigint; finalFault: SourceFault; totalAttempts: number};

export type SourceLifecycleEvent =
	| {kind: 'fault'; fault: SourceFault}
	| {kind: 'reconnectAttempted'}
	| {kind: 'recovered'}
	| {kind: 'reset'};

export type SourceLifecycleAction = 'releaseResources' | 'triggerReconnect' | 'reportFailure' | 'noop';

export interface SourceLifecycleClock {
	nowNs(): bigint;
}

export interface SourceLifecycleTransitionResult {
	state: SourceLifecycleState;
	action: SourceLifecycleAction;
}

const VALID_KINDS: ReadonlySet<SourceLifecycleState['kind']> = new Set(['active', 'reconnecting', 'failed']);
const VALID_EVENT_KINDS: ReadonlySet<SourceLifecycleEvent['kind']> = new Set([
	'fault',
	'reconnectAttempted',
	'recovered',
	'reset',
]);
const VALID_FAULTS: ReadonlySet<SourceFault> = new Set([
	'captureDeviceLost',
	'gpuDeviceLost',
	'encoderError',
	'networkError',
	'decoderError',
]);

export class SourceLifecycleError extends Error {
	readonly code: 'invalidState' | 'invalidEvent' | 'invalidTransition' | 'invariantViolated';

	constructor(code: SourceLifecycleError['code'], message: string) {
		super(message);
		this.code = code;
		this.name = 'SourceLifecycleError';
	}
}

export function createInitialActiveState(clock: SourceLifecycleClock): SourceLifecycleState {
	assertClock(clock);
	const since = clock.nowNs();
	assertFiniteBigint(since, 'clock.nowNs');
	const next: SourceLifecycleState = {kind: 'active', since};
	assertStateShape(next);
	return next;
}

export function transitionSourceLifecycle(
	state: SourceLifecycleState,
	event: SourceLifecycleEvent,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	assertStateShape(state);
	assertEventShape(event);
	assertClock(clock);
	const result = dispatchTransition(state, event, clock);
	assertStateShape(result.state);
	assertActionShape(result.action);
	return result;
}

function dispatchTransition(
	state: SourceLifecycleState,
	event: SourceLifecycleEvent,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	switch (state.kind) {
		case 'active':
			return transitionFromActive(state, event, clock);
		case 'reconnecting':
			return transitionFromReconnecting(state, event, clock);
		case 'failed':
			return transitionFromFailed(state, event, clock);
		default:
			return assertNeverState(state);
	}
}

function transitionFromActive(
	state: SourceLifecycleState & {kind: 'active'},
	event: SourceLifecycleEvent,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	switch (event.kind) {
		case 'fault': {
			assertFault(event.fault);
			const since = clock.nowNs();
			assertFiniteBigint(since, 'clock.nowNs');
			return {
				state: {kind: 'reconnecting', since, attempts: 1, lastFault: event.fault},
				action: 'releaseResources',
			};
		}
		case 'reconnectAttempted':
			return {state, action: 'noop'};
		case 'recovered':
			return {state, action: 'noop'};
		case 'reset':
			return {state, action: 'noop'};
		default:
			return assertNeverEvent(event);
	}
}

function transitionFromReconnecting(
	state: SourceLifecycleState & {kind: 'reconnecting'},
	event: SourceLifecycleEvent,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	switch (event.kind) {
		case 'fault':
			return handleFaultWhileReconnecting(state, event.fault, clock);
		case 'reconnectAttempted':
			return handleReconnectAttempt(state, clock);
		case 'recovered': {
			const since = clock.nowNs();
			assertFiniteBigint(since, 'clock.nowNs');
			return {state: {kind: 'active', since}, action: 'noop'};
		}
		case 'reset': {
			const since = clock.nowNs();
			assertFiniteBigint(since, 'clock.nowNs');
			return {state: {kind: 'active', since}, action: 'noop'};
		}
		default:
			return assertNeverEvent(event);
	}
}

function handleFaultWhileReconnecting(
	state: SourceLifecycleState & {kind: 'reconnecting'},
	fault: SourceFault,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	assertFault(fault);
	assertBoundedAttempts(state.attempts);
	if (state.attempts >= MAX_RECONNECT_ATTEMPTS) {
		const since = clock.nowNs();
		assertFiniteBigint(since, 'clock.nowNs');
		return {
			state: {kind: 'failed', since, finalFault: fault, totalAttempts: state.attempts},
			action: 'reportFailure',
		};
	}
	return {state: {...state, lastFault: fault}, action: 'noop'};
}

function handleReconnectAttempt(
	state: SourceLifecycleState & {kind: 'reconnecting'},
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	assertBoundedAttempts(state.attempts);
	const nextAttempts = state.attempts + 1;
	if (nextAttempts > MAX_RECONNECT_ATTEMPTS) {
		const since = clock.nowNs();
		assertFiniteBigint(since, 'clock.nowNs');
		return {
			state: {kind: 'failed', since, finalFault: state.lastFault, totalAttempts: state.attempts},
			action: 'reportFailure',
		};
	}
	return {state: {...state, attempts: nextAttempts}, action: 'triggerReconnect'};
}

function transitionFromFailed(
	state: SourceLifecycleState & {kind: 'failed'},
	event: SourceLifecycleEvent,
	clock: SourceLifecycleClock,
): SourceLifecycleTransitionResult {
	switch (event.kind) {
		case 'fault':
			return {state, action: 'noop'};
		case 'reconnectAttempted':
			return {state, action: 'noop'};
		case 'recovered':
			return {state, action: 'noop'};
		case 'reset': {
			const since = clock.nowNs();
			assertFiniteBigint(since, 'clock.nowNs');
			return {state: {kind: 'active', since}, action: 'noop'};
		}
		default:
			return assertNeverEvent(event);
	}
}

export function assertStateShape(state: SourceLifecycleState): void {
	if (state === null || typeof state !== 'object') {
		throw new SourceLifecycleError('invalidState', 'state must be an object');
	}
	if (!VALID_KINDS.has(state.kind)) {
		throw new SourceLifecycleError('invalidState', `state.kind invalid: ${String(state.kind)}`);
	}
	assertStateInvariants(state);
}

function assertStateInvariants(state: SourceLifecycleState): void {
	assertFiniteBigint(state.since, 'state.since');
	if (state.kind === 'reconnecting') {
		assertBoundedAttempts(state.attempts);
		assertFault(state.lastFault);
		if (state.attempts < 1) {
			throw new SourceLifecycleError('invariantViolated', `reconnecting.attempts must be >= 1 (got ${state.attempts})`);
		}
	}
	if (state.kind === 'failed') {
		assertFault(state.finalFault);
		assertBoundedAttempts(state.totalAttempts);
	}
}

export function assertEventShape(event: SourceLifecycleEvent): void {
	if (event === null || typeof event !== 'object') {
		throw new SourceLifecycleError('invalidEvent', 'event must be an object');
	}
	if (!VALID_EVENT_KINDS.has(event.kind)) {
		throw new SourceLifecycleError('invalidEvent', `event.kind invalid: ${String(event.kind)}`);
	}
	if (event.kind === 'fault') {
		assertFault(event.fault);
	}
}

function assertActionShape(action: SourceLifecycleAction): void {
	switch (action) {
		case 'releaseResources':
		case 'triggerReconnect':
		case 'reportFailure':
		case 'noop':
			return;
		default:
			throw new SourceLifecycleError('invariantViolated', `action invalid: ${String(action)}`);
	}
}

function assertFault(fault: SourceFault): void {
	if (!VALID_FAULTS.has(fault)) {
		throw new SourceLifecycleError('invalidEvent', `fault kind invalid: ${String(fault)}`);
	}
}

function assertBoundedAttempts(attempts: number): void {
	if (!Number.isInteger(attempts)) {
		throw new SourceLifecycleError('invariantViolated', `attempts must be integer (got ${attempts})`);
	}
	if (attempts < 0) {
		throw new SourceLifecycleError('invariantViolated', `attempts must be >= 0 (got ${attempts})`);
	}
	if (attempts > MAX_RECONNECT_ATTEMPTS) {
		throw new SourceLifecycleError('invariantViolated', `attempts exceeds cap (got ${attempts})`);
	}
}

function assertFiniteBigint(value: bigint, label: string): void {
	if (typeof value !== 'bigint') {
		throw new SourceLifecycleError('invariantViolated', `${label} must be bigint (got ${typeof value})`);
	}
	if (value < 0n) {
		throw new SourceLifecycleError('invariantViolated', `${label} must be >= 0 (got ${value})`);
	}
}

function assertClock(clock: SourceLifecycleClock): void {
	if (clock === null || typeof clock !== 'object') {
		throw new SourceLifecycleError('invariantViolated', 'clock must be an object');
	}
	if (typeof clock.nowNs !== 'function') {
		throw new SourceLifecycleError('invariantViolated', 'clock.nowNs must be a function');
	}
}

function assertNeverState(state: never): never {
	throw new SourceLifecycleError('invalidState', `unhandled state kind: ${JSON.stringify(state)}`);
}

function assertNeverEvent(event: never): never {
	throw new SourceLifecycleError('invalidEvent', `unhandled event kind: ${JSON.stringify(event)}`);
}
