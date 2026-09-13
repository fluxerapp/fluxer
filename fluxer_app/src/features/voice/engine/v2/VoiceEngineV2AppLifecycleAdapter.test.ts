// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2AppDiagnosticsLogger} from '@app/features/voice/engine/v2/VoiceEngineV2AppDiagnosticsAdapter';
import {
	createVoiceEngineV2AppLifecycleAdapter,
	LIFECYCLE_OPERATION_CAP,
	TEARDOWN_PER_DISPOSABLE_TIMEOUT_MS,
	type VoiceEngineV2AppLifecycleAdapter,
	type VoiceEngineV2AppLifecycleDisposable,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppLifecycleAdapter';
import {createVoiceEngineV2DeterministicClockPort} from '@fluxer/voice_engine_v2/runtime';
import {describe, expect, it} from 'vitest';

interface LoggerCall {
	readonly level: 'trace' | 'debug' | 'info' | 'warn' | 'error';
	readonly payload: Record<string, unknown>;
}

interface FakeLogger extends VoiceEngineV2AppDiagnosticsLogger {
	readonly calls: ReadonlyArray<LoggerCall>;
}

function createFakeLogger(): FakeLogger {
	const calls: Array<LoggerCall> = [];
	const make = (level: LoggerCall['level']) => (payload: Record<string, unknown>) => {
		calls.push({level, payload: {...payload}});
	};
	return {
		get calls() {
			return calls;
		},
		trace: make('trace'),
		debug: make('debug'),
		info: make('info'),
		warn: make('warn'),
		error: make('error'),
	};
}

interface CallTracker {
	readonly events: ReadonlyArray<string>;
	push(event: string): void;
}

function createCallTracker(): CallTracker {
	const events: Array<string> = [];
	return {
		get events() {
			return events;
		},
		push(event: string): void {
			events.push(event);
		},
	};
}

function disposable(name: string, tracker: CallTracker): VoiceEngineV2AppLifecycleDisposable {
	return {
		name,
		async dispose(): Promise<void> {
			tracker.push(`dispose:${name}`);
		},
	};
}

function hangingDisposable(name: string, tracker: CallTracker): VoiceEngineV2AppLifecycleDisposable {
	return {
		name,
		dispose(): Promise<void> {
			tracker.push(`enter:${name}`);
			return new Promise<void>(() => {});
		},
	};
}

function throwingDisposable(name: string, tracker: CallTracker, error: unknown): VoiceEngineV2AppLifecycleDisposable {
	return {
		name,
		async dispose(): Promise<void> {
			tracker.push(`throw:${name}`);
			throw error;
		},
	};
}

function buildAdapter(
	disposables: ReadonlyArray<VoiceEngineV2AppLifecycleDisposable>,
	logger: FakeLogger,
	teardownPerDisposableTimeoutMs?: number,
): VoiceEngineV2AppLifecycleAdapter {
	return createVoiceEngineV2AppLifecycleAdapter({
		disposables,
		logger,
		clock: createVoiceEngineV2DeterministicClockPort(0, 1),
		...(teardownPerDisposableTimeoutMs === undefined ? {} : {teardownPerDisposableTimeoutMs}),
	});
}

describe('VoiceEngineV2AppLifecycleAdapter', () => {
	it('cancelOperation aborts the registered controller and logs lifecycle.cancelled', async () => {
		const logger = createFakeLogger();
		const adapter = buildAdapter([], logger);
		const controller = new AbortController();
		const seen: Array<string | null> = [];
		controller.signal.addEventListener('abort', () => {
			seen.push(typeof controller.signal.reason === 'string' ? controller.signal.reason : null);
		});

		adapter.register(7, controller, 'mediaAdapter');
		expect(adapter.registrySize).toBe(1);

		await adapter.cancelOperation(7, 'cancel-requested');

		expect(controller.signal.aborted).toBe(true);
		expect(seen).toEqual(['cancel-requested']);
		expect(adapter.registrySize).toBe(0);
		expect(logger.calls.some((entry) => entry.payload.code === 'lifecycle.cancelled')).toBe(true);
	});

	it('cancelOperation with an unknown id is a no-op and emits a debug log', async () => {
		const logger = createFakeLogger();
		const adapter = buildAdapter([], logger);

		await adapter.cancelOperation(99, 'no-such-operation');

		expect(adapter.registrySize).toBe(0);
		const debug = logger.calls.find((entry) => entry.payload.code === 'lifecycle.cancel.unknown');
		expect(debug).toBeDefined();
		expect(debug?.level).toBe('debug');
	});

	it('register throws lifecycleRegistryFull when the cap is exhausted', () => {
		const logger = createFakeLogger();
		const adapter = buildAdapter([], logger);
		for (let index = 1; index <= LIFECYCLE_OPERATION_CAP; index += 1) {
			adapter.register(index, new AbortController(), 'fillerAdapter');
		}
		expect(adapter.registrySize).toBe(LIFECYCLE_OPERATION_CAP);
		let captured: Error | undefined;
		try {
			adapter.register(LIFECYCLE_OPERATION_CAP + 1, new AbortController(), 'overflowAdapter');
		} catch (error) {
			captured = error as Error;
		}
		expect(captured).toBeDefined();
		expect((captured as Error & {code?: string}).code).toBe('lifecycleRegistryFull');
		expect(adapter.registrySize).toBe(LIFECYCLE_OPERATION_CAP);
	});

	it('teardown disposes in reverse creation order', async () => {
		const tracker = createCallTracker();
		const logger = createFakeLogger();
		const adapter = buildAdapter(
			[disposable('first', tracker), disposable('second', tracker), disposable('third', tracker)],
			logger,
		);

		await adapter.teardown();

		expect(tracker.events).toEqual(['dispose:third', 'dispose:second', 'dispose:first']);
		expect(adapter.isTornDown).toBe(true);
	});

	it('teardown is a no-op on subsequent calls', async () => {
		const tracker = createCallTracker();
		const logger = createFakeLogger();
		const adapter = buildAdapter([disposable('only', tracker)], logger);

		await adapter.teardown();
		await adapter.teardown();
		await adapter.teardown();

		expect(tracker.events).toEqual(['dispose:only']);
		const repeated = logger.calls.filter((entry) => entry.payload.code === 'lifecycle.teardown.repeated');
		expect(repeated.length).toBe(2);
	});

	it('teardown continues past a hanging disposable once the per-disposable timeout fires', async () => {
		const tracker = createCallTracker();
		const logger = createFakeLogger();
		const adapter = buildAdapter(
			[disposable('first', tracker), hangingDisposable('hang', tracker), disposable('last', tracker)],
			logger,
			5,
		);

		await adapter.teardown();

		expect(tracker.events).toEqual(['dispose:last', 'enter:hang', 'dispose:first']);
		const timeout = logger.calls.find((entry) => entry.payload.code === 'lifecycle.teardown.timeout');
		expect(timeout).toBeDefined();
		expect(timeout?.level).toBe('error');
		expect((timeout?.payload.detail as {name?: string}).name).toBe('hang');
	});

	it('teardown continues past a throwing disposable and logs lifecycle.teardown.error', async () => {
		const tracker = createCallTracker();
		const logger = createFakeLogger();
		const error = new Error('boom');
		const adapter = buildAdapter(
			[disposable('first', tracker), throwingDisposable('explode', tracker, error), disposable('last', tracker)],
			logger,
		);

		await adapter.teardown();

		expect(tracker.events).toEqual(['dispose:last', 'throw:explode', 'dispose:first']);
		const errorEntry = logger.calls.find((entry) => entry.payload.code === 'lifecycle.teardown.error');
		expect(errorEntry).toBeDefined();
		expect(errorEntry?.level).toBe('error');
		expect((errorEntry?.payload.detail as {name?: string}).name).toBe('explode');
	});

	it('cancelOperation after teardown is harmless and does not re-trigger disposal', async () => {
		const tracker = createCallTracker();
		const logger = createFakeLogger();
		const adapter = buildAdapter([disposable('only', tracker)], logger);

		await adapter.teardown();
		const eventsAfterTeardown = [...tracker.events];

		await adapter.cancelOperation(1, 'after-teardown');
		await adapter.cancelOperation(2, 'still-after-teardown');

		expect(tracker.events).toEqual(eventsAfterTeardown);
		expect(adapter.registrySize).toBe(0);
	});

	it('produces the same registry transitions across runs for the same input sequence', async () => {
		const sequence: Array<{op: 'register' | 'unregister' | 'cancel'; id: number}> = [
			{op: 'register', id: 1},
			{op: 'register', id: 2},
			{op: 'register', id: 3},
			{op: 'cancel', id: 2},
			{op: 'unregister', id: 1},
			{op: 'register', id: 4},
		];

		async function run(): Promise<Array<number>> {
			const logger = createFakeLogger();
			const adapter = buildAdapter([], logger);
			const sizes: Array<number> = [];
			for (const step of sequence) {
				if (step.op === 'register') {
					adapter.register(step.id, new AbortController(), 'detAdapter');
				} else if (step.op === 'unregister') {
					adapter.unregister(step.id);
				} else {
					await adapter.cancelOperation(step.id, 'deterministic');
				}
				sizes.push(adapter.registrySize);
			}
			return sizes;
		}

		const first = await run();
		const second = await run();
		const third = await run();
		expect(first).toEqual(second);
		expect(second).toEqual(third);
		expect(first).toEqual([1, 2, 3, 2, 1, 2]);
	});

	it('register rejects duplicate operationIds with lifecycleOperationAlreadyRegistered', () => {
		const logger = createFakeLogger();
		const adapter = buildAdapter([], logger);
		adapter.register(11, new AbortController(), 'dupAdapter');
		let captured: Error | undefined;
		try {
			adapter.register(11, new AbortController(), 'dupAdapter');
		} catch (error) {
			captured = error as Error;
		}
		expect(captured).toBeDefined();
		expect((captured as Error & {code?: string}).code).toBe('lifecycleOperationAlreadyRegistered');
		expect(adapter.registrySize).toBe(1);
	});

	it('exposes the documented constants for upstream wiring', () => {
		expect(LIFECYCLE_OPERATION_CAP).toBe(4096);
		expect(TEARDOWN_PER_DISPOSABLE_TIMEOUT_MS).toBe(5000);
	});

	it('unregister returns true when the operationId was tracked, false otherwise', () => {
		const logger = createFakeLogger();
		const adapter = buildAdapter([], logger);
		adapter.register(5, new AbortController(), 'unregAdapter');
		expect(adapter.unregister(5)).toBe(true);
		expect(adapter.unregister(5)).toBe(false);
		expect(adapter.registrySize).toBe(0);
	});
});
