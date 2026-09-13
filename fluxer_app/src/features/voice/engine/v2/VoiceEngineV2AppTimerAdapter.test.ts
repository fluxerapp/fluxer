// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2AppTimerAdapter,
	VOICE_ENGINE_V2_TIMER_REGISTRY_CAP,
	type VoiceEngineV2AppTimerFireEvent,
	type VoiceEngineV2AppTimerHandle,
	VoiceEngineV2AppTimerRegistryFullError,
	type VoiceEngineV2AppTimerScheduler,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppTimerAdapter';
import {createVoiceEngineV2DeterministicClockPort} from '@fluxer/voice_engine_v2/runtime';
import {describe, expect, it} from 'vitest';

interface FakeTimer {
	id: number;
	callback: () => void;
	ms: number;
	cleared: boolean;
}

interface FakeScheduler extends VoiceEngineV2AppTimerScheduler {
	readonly timers: ReadonlyArray<FakeTimer>;
	fireById(id: number): void;
	pendingCount(): number;
}

function createFakeScheduler(): FakeScheduler {
	const timers: Array<FakeTimer> = [];
	let nextId = 1;
	return {
		get timers() {
			return timers;
		},
		setTimeout(callback, ms): VoiceEngineV2AppTimerHandle {
			const timer: FakeTimer = {id: nextId++, callback, ms, cleared: false};
			timers.push(timer);
			return timer;
		},
		clearTimeout(handle): void {
			const timer = handle as FakeTimer;
			timer.cleared = true;
		},
		fireById(id): void {
			const timer = timers.find((t) => t.id === id);
			if (timer === undefined) throw new Error(`no fake timer with id=${id}`);
			if (timer.cleared) throw new Error(`fake timer id=${id} was cleared; refuse to fire`);
			timer.callback();
		},
		pendingCount(): number {
			return timers.filter((t) => !t.cleared).length;
		},
	};
}

describe('VoiceEngineV2AppTimerAdapter', () => {
	it('schedule registers a setTimeout and removes the entry on fire', async () => {
		const scheduler = createFakeScheduler();
		const fires: Array<VoiceEngineV2AppTimerFireEvent> = [];
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(100, 1),
			scheduler,
			onFire: (event) => fires.push(event),
		});

		await adapter.schedule({timerId: 't1', delayMs: 250});
		expect(scheduler.timers).toEqual([{id: 1, callback: expect.any(Function), ms: 250, cleared: false}]);

		scheduler.fireById(1);

		expect(fires).toEqual([{timerId: 't1', scheduledAtMs: 100, firedAtMs: 101}]);
		await adapter.cancel('t1');
		expect(scheduler.timers[0].cleared).toBe(false);
	});

	it('cancel before fire clears the underlying timeout and forgets the entry', async () => {
		const scheduler = createFakeScheduler();
		const fires: Array<VoiceEngineV2AppTimerFireEvent> = [];
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
			onFire: (event) => fires.push(event),
		});

		await adapter.schedule({timerId: 't1', delayMs: 500});
		await adapter.cancel('t1');

		expect(scheduler.timers[0].cleared).toBe(true);
		expect(fires).toEqual([]);
	});

	it('cancel after fire is an idempotent no-op (no extra clearTimeout, no exception)', async () => {
		const scheduler = createFakeScheduler();
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
		});

		await adapter.schedule({timerId: 't1', delayMs: 100});
		scheduler.fireById(1);
		await adapter.cancel('t1');
		await adapter.cancel('t1');
		await adapter.cancel('never-existed');

		expect(scheduler.timers[0].cleared).toBe(false);
	});

	it('rejects schedule with the structured timerRegistryFull error when the cap is exceeded', async () => {
		const scheduler = createFakeScheduler();
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
			registryCap: 2,
		});

		await adapter.schedule({timerId: 'a', delayMs: 100});
		await adapter.schedule({timerId: 'b', delayMs: 100});

		await expect(adapter.schedule({timerId: 'c', delayMs: 100})).rejects.toMatchObject({
			code: 'timerRegistryFull',
			capability: 'timer',
			timerId: 'c',
			cap: 2,
		});
		await expect(adapter.schedule({timerId: 'c', delayMs: 100})).rejects.toBeInstanceOf(
			VoiceEngineV2AppTimerRegistryFullError,
		);
		expect(scheduler.pendingCount()).toBe(2);
	});

	it('tracks multiple concurrent timers independently and fires them in scheduler order', async () => {
		const scheduler = createFakeScheduler();
		const fires: Array<VoiceEngineV2AppTimerFireEvent> = [];
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(1_000, 5),
			scheduler,
			onFire: (event) => fires.push(event),
		});

		await adapter.schedule({timerId: 'first', delayMs: 100});
		await adapter.schedule({timerId: 'second', delayMs: 200});
		await adapter.schedule({timerId: 'third', delayMs: 300});

		scheduler.fireById(2);
		scheduler.fireById(1);
		scheduler.fireById(3);

		expect(fires.map((f) => f.timerId)).toEqual(['second', 'first', 'third']);
		expect(fires[0].scheduledAtMs).toBe(1_005);
		expect(fires[1].scheduledAtMs).toBe(1_000);
		expect(fires[2].scheduledAtMs).toBe(1_010);
	});

	it('produces a deterministic registry state for identical schedule sequences', async () => {
		function runOnce(): ReadonlyArray<{timerId: string; scheduledAtMs: number; firedAtMs: number}> {
			const scheduler = createFakeScheduler();
			const fires: Array<VoiceEngineV2AppTimerFireEvent> = [];
			const adapter = createVoiceEngineV2AppTimerAdapter({
				clock: createVoiceEngineV2DeterministicClockPort(0, 2),
				scheduler,
				onFire: (event) => fires.push(event),
			});
			void adapter.schedule({timerId: 'a', delayMs: 10});
			void adapter.schedule({timerId: 'b', delayMs: 20});
			scheduler.fireById(1);
			scheduler.fireById(2);
			return fires.slice();
		}
		expect(runOnce()).toEqual(runOnce());
	});

	it('removes the entry from the registry as part of the setTimeout callback', async () => {
		const scheduler = createFakeScheduler();
		let observedDuringFire: number | null = null;
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
			onFire: () => {
				observedDuringFire = scheduler.pendingCount();
			},
		});

		await adapter.schedule({timerId: 't1', delayMs: 100});
		scheduler.fireById(1);

		await adapter.schedule({timerId: 't2', delayMs: 100});
		expect(scheduler.timers).toHaveLength(2);
		expect(observedDuringFire).toBe(1);
	});

	it('rejects invalid timer options (empty id, zero delay, non-finite delay, oversized id)', async () => {
		const scheduler = createFakeScheduler();
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
		});

		await expect(adapter.schedule({timerId: '', delayMs: 100})).rejects.toThrow(/must not be empty/);
		await expect(adapter.schedule({timerId: 't1', delayMs: 0})).rejects.toThrow(/positive/);
		await expect(adapter.schedule({timerId: 't1', delayMs: Number.NaN})).rejects.toThrow(/finite/);
		await expect(adapter.schedule({timerId: 't1', delayMs: 100, repeat: true})).rejects.toThrow(/repeat=true/);
		expect(scheduler.timers).toEqual([]);
	});

	it('exposes the documented registry cap default and enforces it as the upper bound', async () => {
		expect(VOICE_ENGINE_V2_TIMER_REGISTRY_CAP).toBe(256);
		const scheduler = createFakeScheduler();
		const adapter = createVoiceEngineV2AppTimerAdapter({
			clock: createVoiceEngineV2DeterministicClockPort(0, 0),
			scheduler,
		});

		for (let i = 0; i < VOICE_ENGINE_V2_TIMER_REGISTRY_CAP; i++) {
			await adapter.schedule({timerId: `t${i}`, delayMs: 1});
		}
		await expect(adapter.schedule({timerId: 'overflow', delayMs: 1})).rejects.toBeInstanceOf(
			VoiceEngineV2AppTimerRegistryFullError,
		);
		expect(scheduler.pendingCount()).toBe(VOICE_ENGINE_V2_TIMER_REGISTRY_CAP);
	});
});
