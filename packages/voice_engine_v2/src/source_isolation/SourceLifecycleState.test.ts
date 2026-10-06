// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	computeReconnectBackoffMs,
	createInitialActiveState,
	MAX_RECONNECT_ATTEMPTS,
	MAX_TRACKED_SOURCES,
	RECONNECT_BACKOFF_CAP_MS,
	RECONNECT_BACKOFF_STEP_MS,
	type SourceLifecycleClock,
	type SourceLifecycleEvent,
	SourceLifecycleRegistry,
	type SourceLifecycleState,
	transitionSourceLifecycle,
} from '@fluxer/voice_engine_v2/src/source_isolation/index';
import {describe, expect, it} from 'vitest';

function makeClock(startNs: bigint = 1_000n, stepNs: bigint = 1_000n): SourceLifecycleClock {
	let value = startNs;
	return {
		nowNs(): bigint {
			const current = value;
			value += stepNs;
			return current;
		},
	};
}

function makeFixedClock(valueNs: bigint): SourceLifecycleClock {
	return {nowNs: () => valueNs};
}

function drainToReconnecting(
	state: SourceLifecycleState,
	clock: SourceLifecycleClock,
	attemptCount: number,
): SourceLifecycleState {
	let current = state;
	for (let i = 0; i < attemptCount; i += 1) {
		const result = transitionSourceLifecycle(current, {kind: 'reconnectAttempted'}, clock);
		current = result.state;
	}
	return current;
}

describe('transitionSourceLifecycle', () => {
	it('Active + fault transitions to Reconnecting with attempts=1', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const event: SourceLifecycleEvent = {kind: 'fault', fault: 'captureDeviceLost'};
		const result = transitionSourceLifecycle(active, event, clock);
		expect(result.state.kind).toBe('reconnecting');
		expect(result.action).toBe('releaseResources');
		if (result.state.kind !== 'reconnecting') throw new Error('unreachable');
		expect(result.state.attempts).toBe(1);
		expect(result.state.lastFault).toBe('captureDeviceLost');
	});

	it('Reconnecting + reconnectAttempted bumps attempts and emits triggerReconnect', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const faulted = transitionSourceLifecycle(active, {kind: 'fault', fault: 'networkError'}, clock);
		const next = transitionSourceLifecycle(faulted.state, {kind: 'reconnectAttempted'}, clock);
		expect(next.state.kind).toBe('reconnecting');
		expect(next.action).toBe('triggerReconnect');
		if (next.state.kind !== 'reconnecting') throw new Error('unreachable');
		expect(next.state.attempts).toBe(2);
	});

	it('Reconnecting + recovered resets to Active', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const faulted = transitionSourceLifecycle(active, {kind: 'fault', fault: 'encoderError'}, clock);
		const next = transitionSourceLifecycle(faulted.state, {kind: 'reconnectAttempted'}, clock);
		const recovered = transitionSourceLifecycle(next.state, {kind: 'recovered'}, clock);
		expect(recovered.state.kind).toBe('active');
		expect(recovered.action).toBe('noop');
	});

	it('Reconnecting at MAX_RECONNECT_ATTEMPTS + fault transitions to Failed', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const initial = transitionSourceLifecycle(active, {kind: 'fault', fault: 'networkError'}, clock);
		const drained = drainToReconnecting(initial.state, clock, MAX_RECONNECT_ATTEMPTS - 1);
		if (drained.kind !== 'reconnecting') throw new Error('expected reconnecting');
		expect(drained.attempts).toBe(MAX_RECONNECT_ATTEMPTS);
		const failed = transitionSourceLifecycle(drained, {kind: 'fault', fault: 'gpuDeviceLost'}, clock);
		expect(failed.state.kind).toBe('failed');
		expect(failed.action).toBe('reportFailure');
		if (failed.state.kind !== 'failed') throw new Error('unreachable');
		expect(failed.state.finalFault).toBe('gpuDeviceLost');
		expect(failed.state.totalAttempts).toBe(MAX_RECONNECT_ATTEMPTS);
	});

	it('Reconnecting exhausting reconnectAttempted past cap transitions to Failed', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const initial = transitionSourceLifecycle(active, {kind: 'fault', fault: 'networkError'}, clock);
		const drained = drainToReconnecting(initial.state, clock, MAX_RECONNECT_ATTEMPTS - 1);
		if (drained.kind !== 'reconnecting') throw new Error('expected reconnecting');
		const failed = transitionSourceLifecycle(drained, {kind: 'reconnectAttempted'}, clock);
		expect(failed.state.kind).toBe('failed');
		expect(failed.action).toBe('reportFailure');
	});

	it('Failed + recovered does not auto-rehabilitate', () => {
		const clock = makeClock();
		const failed: SourceLifecycleState = {
			kind: 'failed',
			since: 5_000n,
			finalFault: 'decoderError',
			totalAttempts: MAX_RECONNECT_ATTEMPTS,
		};
		const result = transitionSourceLifecycle(failed, {kind: 'recovered'}, clock);
		expect(result.state).toEqual(failed);
		expect(result.action).toBe('noop');
	});

	it('Failed + reset returns to Active', () => {
		const clock = makeFixedClock(99n);
		const failed: SourceLifecycleState = {
			kind: 'failed',
			since: 5_000n,
			finalFault: 'decoderError',
			totalAttempts: MAX_RECONNECT_ATTEMPTS,
		};
		const result = transitionSourceLifecycle(failed, {kind: 'reset'}, clock);
		expect(result.state.kind).toBe('active');
		if (result.state.kind !== 'active') throw new Error('unreachable');
		expect(result.state.since).toBe(99n);
	});

	it("Action 'releaseResources' fires exactly once on fault transitioning from Active", () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const firstFault = transitionSourceLifecycle(active, {kind: 'fault', fault: 'captureDeviceLost'}, clock);
		expect(firstFault.action).toBe('releaseResources');
		const secondFault = transitionSourceLifecycle(firstFault.state, {kind: 'fault', fault: 'captureDeviceLost'}, clock);
		expect(secondFault.action).not.toBe('releaseResources');
	});

	it("Action 'triggerReconnect' fires on each reconnectAttempted while Reconnecting", () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const faulted = transitionSourceLifecycle(active, {kind: 'fault', fault: 'networkError'}, clock);
		let current = faulted.state;
		const triggers: Array<string> = [];
		for (let i = 0; i < 3; i += 1) {
			const next = transitionSourceLifecycle(current, {kind: 'reconnectAttempted'}, clock);
			triggers.push(next.action);
			current = next.state;
		}
		expect(triggers).toEqual(['triggerReconnect', 'triggerReconnect', 'triggerReconnect']);
	});

	it('Backoff schedule respects cap', () => {
		expect(computeReconnectBackoffMs(1)).toBe(RECONNECT_BACKOFF_STEP_MS);
		expect(computeReconnectBackoffMs(2)).toBe(2 * RECONNECT_BACKOFF_STEP_MS);
		expect(computeReconnectBackoffMs(MAX_RECONNECT_ATTEMPTS)).toBeLessThanOrEqual(RECONNECT_BACKOFF_CAP_MS);
	});

	it('Backoff schedule is exponential matching the Rust sibling', () => {
		const sequence = [1, 2, 3, 4, 5, 6, 7, 8].map((attempt) => computeReconnectBackoffMs(attempt));
		expect(sequence).toEqual([100, 200, 400, 800, 1600, 3200, 5000, 5000]);
	});

	it('Backoff caps at RECONNECT_BACKOFF_CAP_MS once the raw doubling crosses it', () => {
		expect(computeReconnectBackoffMs(7)).toBe(RECONNECT_BACKOFF_CAP_MS);
		expect(computeReconnectBackoffMs(8)).toBe(RECONNECT_BACKOFF_CAP_MS);
	});

	it('Active + reconnectAttempted is a no-op', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const result = transitionSourceLifecycle(active, {kind: 'reconnectAttempted'}, clock);
		expect(result.state).toEqual(active);
		expect(result.action).toBe('noop');
	});

	it('determinism: same (state, event, clock-output) produces same output', () => {
		const stateA: SourceLifecycleState = {kind: 'active', since: 100n};
		const stateB: SourceLifecycleState = {kind: 'active', since: 100n};
		const clockA = makeFixedClock(200n);
		const clockB = makeFixedClock(200n);
		const event: SourceLifecycleEvent = {kind: 'fault', fault: 'networkError'};
		const resultA = transitionSourceLifecycle(stateA, event, clockA);
		const resultB = transitionSourceLifecycle(stateB, event, clockB);
		expect(resultA).toEqual(resultB);
	});

	it('rejects invalid state kind on entry', () => {
		const clock = makeClock();
		const bogus = {kind: 'bogus'} as unknown as SourceLifecycleState;
		expect(() => transitionSourceLifecycle(bogus, {kind: 'recovered'}, clock)).toThrow();
	});

	it('rejects invalid event kind on entry', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const bogusEvent = {kind: 'bogus'} as unknown as SourceLifecycleEvent;
		expect(() => transitionSourceLifecycle(active, bogusEvent, clock)).toThrow();
	});

	it('rejects invalid fault on event entry', () => {
		const clock = makeClock();
		const active = createInitialActiveState(clock);
		const bogusEvent = {kind: 'fault', fault: 'wat'} as unknown as SourceLifecycleEvent;
		expect(() => transitionSourceLifecycle(active, bogusEvent, clock)).toThrow();
	});
});

describe('SourceLifecycleRegistry', () => {
	it('register + dispatch fault returns releaseResources for known source', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock);
		const reg = registry.register('source-a');
		expect(reg.ok).toBe(true);
		const result = registry.dispatch('source-a', {kind: 'fault', fault: 'captureDeviceLost'});
		expect(result.ok).toBe(true);
		if (!result.ok) throw new Error('unreachable');
		expect(result.action).toBe('releaseResources');
	});

	it('rejects sources past MAX_TRACKED_SOURCES', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock, MAX_TRACKED_SOURCES);
		for (let i = 0; i < MAX_TRACKED_SOURCES; i += 1) {
			const result = registry.register(`s-${i}`);
			expect(result.ok).toBe(true);
		}
		expect(registry.size()).toBe(MAX_TRACKED_SOURCES);
		const overflow = registry.register('overflow');
		expect(overflow.ok).toBe(false);
		if (overflow.ok) throw new Error('unreachable');
		expect(overflow.error).toBe('capExceeded');
	});

	it('non-existent source dispatch returns unknownSource error', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock);
		const result = registry.dispatch('ghost', {kind: 'recovered'});
		expect(result.ok).toBe(false);
		if (result.ok) throw new Error('unreachable');
		expect(result.error).toBe('unknownSource');
	});

	it('snapshot returns sorted entries reflecting state mutations', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock);
		registry.register('zeta');
		registry.register('alpha');
		registry.dispatch('alpha', {kind: 'fault', fault: 'gpuDeviceLost'});
		const snap = registry.snapshot();
		expect(snap.length).toBe(2);
		expect(snap[0]?.sourceId).toBe('alpha');
		expect(snap[1]?.sourceId).toBe('zeta');
		expect(snap[0]?.state.kind).toBe('reconnecting');
		expect(snap[1]?.state.kind).toBe('active');
	});

	it('duplicate source registration returns duplicateSource error', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock);
		const first = registry.register('s');
		expect(first.ok).toBe(true);
		const second = registry.register('s');
		expect(second.ok).toBe(false);
		if (second.ok) throw new Error('unreachable');
		expect(second.error).toBe('duplicateSource');
	});

	it('invalid source id is rejected', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock);
		const result = registry.register('');
		expect(result.ok).toBe(false);
		const dispatch = registry.dispatch('', {kind: 'recovered'});
		expect(dispatch.ok).toBe(false);
		if (dispatch.ok) throw new Error('unreachable');
		expect(dispatch.error).toBe('invalidSourceId');
	});

	it('remove drops a source and frees a slot for reuse', () => {
		const clock = makeClock();
		const registry = new SourceLifecycleRegistry(clock, 2);
		expect(registry.register('a').ok).toBe(true);
		expect(registry.register('b').ok).toBe(true);
		expect(registry.register('c').ok).toBe(false);
		expect(registry.remove('a')).toBe(true);
		expect(registry.register('c').ok).toBe(true);
	});
});
