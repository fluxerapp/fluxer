// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {TimerPort, VoiceEngineV2TimerOptions} from '@fluxer/voice_engine_v2';
import type {VoiceEngineV2ClockPort} from '@fluxer/voice_engine_v2/runtime';

const VOICE_ENGINE_V2_TIMER_REGISTRY_CAP = 256;
const VOICE_ENGINE_V2_TIMER_ID_MAX_LENGTH = 128;
const VOICE_ENGINE_V2_TIMER_DELAY_MAX_MS = 24 * 60 * 60 * 1000;

type GlobalTimeoutHandle = number | NodeJS.Timeout;

type VoiceEngineV2AppTimerHandle = unknown;

export interface VoiceEngineV2AppTimerScheduler {
	setTimeout(callback: () => void, ms: number): VoiceEngineV2AppTimerHandle;
	clearTimeout(handle: VoiceEngineV2AppTimerHandle): void;
}

interface VoiceEngineV2AppTimerFireEvent {
	timerId: string;
	scheduledAtMs: number;
	firedAtMs: number;
}

export interface VoiceEngineV2AppTimerAdapterOptions {
	clock: VoiceEngineV2ClockPort;
	scheduler?: VoiceEngineV2AppTimerScheduler;
	registryCap?: number;
	onFire?: (event: VoiceEngineV2AppTimerFireEvent) => void;
	onError?: (operation: 'fire', timerId: string, error: unknown) => void;
}

interface RegistryEntry {
	handle: VoiceEngineV2AppTimerHandle;
	scheduledAtMs: number;
}

class VoiceEngineV2AppTimerRegistryFullError extends Error {
	readonly code = 'timerRegistryFull' as const;
	readonly capability = 'timer' as const;
	readonly timerId: string;
	readonly cap: number;
	constructor(timerId: string, cap: number) {
		super(`VoiceEngineV2AppTimerAdapter: registry is full (cap=${cap}) — refused timer "${timerId}"`);
		this.name = 'VoiceEngineV2AppTimerRegistryFullError';
		this.timerId = timerId;
		this.cap = cap;
	}
}

function defaultScheduler(): VoiceEngineV2AppTimerScheduler {
	return {
		setTimeout(callback, ms): VoiceEngineV2AppTimerHandle {
			return globalThis.setTimeout(callback, ms);
		},
		clearTimeout(handle): void {
			globalThis.clearTimeout(handle as GlobalTimeoutHandle);
		},
	};
}

function assertOptions(options: VoiceEngineV2TimerOptions): void {
	assert.ok(options !== null && typeof options === 'object', 'voice-engine-v2 timer options must be an object');
	assert.equal(typeof options.timerId, 'string', 'voice-engine-v2 timer timerId must be a string');
	assert.ok(options.timerId.length > 0, 'voice-engine-v2 timer timerId must not be empty');
	assert.ok(
		options.timerId.length <= VOICE_ENGINE_V2_TIMER_ID_MAX_LENGTH,
		'voice-engine-v2 timer timerId exceeds the maximum length',
	);
	assert.equal(typeof options.delayMs, 'number', 'voice-engine-v2 timer delayMs must be a number');
	assert.ok(Number.isFinite(options.delayMs), 'voice-engine-v2 timer delayMs must be finite');
	assert.ok(options.delayMs > 0, 'voice-engine-v2 timer delayMs must be positive (zero rejected)');
	assert.ok(
		options.delayMs <= VOICE_ENGINE_V2_TIMER_DELAY_MAX_MS,
		'voice-engine-v2 timer delayMs exceeds the maximum allowed delay',
	);
	if (options.repeat !== undefined) {
		assert.equal(typeof options.repeat, 'boolean', 'voice-engine-v2 timer repeat must be a boolean when provided');
		assert.ok(options.repeat === false, 'voice-engine-v2 timer repeat=true is not supported by this adapter');
	}
}

function readClockMs(clock: VoiceEngineV2ClockPort): number {
	const now = clock.now();
	assert.equal(typeof now, 'number', 'voice-engine-v2 timer clock.now must return a number');
	assert.ok(Number.isFinite(now), 'voice-engine-v2 timer clock.now must be finite');
	assert.ok(now >= 0, 'voice-engine-v2 timer clock.now must be non-negative');
	return now;
}

export function createVoiceEngineV2AppTimerAdapter(options: VoiceEngineV2AppTimerAdapterOptions): TimerPort {
	assert.ok(options !== null && typeof options === 'object', 'voice-engine-v2 timer adapter requires options');
	assert.ok(
		options.clock !== null && typeof options.clock === 'object',
		'voice-engine-v2 timer adapter requires a clock',
	);
	const clock = options.clock;
	const scheduler = options.scheduler ?? defaultScheduler();
	assert.equal(typeof scheduler.setTimeout, 'function', 'timer adapter scheduler must expose setTimeout');
	assert.equal(typeof scheduler.clearTimeout, 'function', 'timer adapter scheduler must expose clearTimeout');
	const cap = options.registryCap ?? VOICE_ENGINE_V2_TIMER_REGISTRY_CAP;
	assert.ok(Number.isInteger(cap) && cap > 0, 'voice-engine-v2 timer adapter registryCap must be a positive integer');
	const registry = new Map<string, RegistryEntry>();
	const onFire = options.onFire;
	const onError = options.onError;
	return {
		async schedule(timerOptions: VoiceEngineV2TimerOptions): Promise<void> {
			assertOptions(timerOptions);
			const existing = registry.get(timerOptions.timerId);
			if (existing !== undefined) {
				scheduler.clearTimeout(existing.handle);
				registry.delete(timerOptions.timerId);
			}
			if (registry.size >= cap) {
				throw new VoiceEngineV2AppTimerRegistryFullError(timerOptions.timerId, cap);
			}
			const scheduledAtMs = readClockMs(clock);
			const timerId = timerOptions.timerId;
			const handle = scheduler.setTimeout(() => {
				const entry = registry.get(timerId);
				if (entry === undefined) return;
				registry.delete(timerId);
				try {
					onFire?.({timerId, scheduledAtMs: entry.scheduledAtMs, firedAtMs: readClockMs(clock)});
				} catch (error) {
					onError?.('fire', timerId, error);
				}
			}, timerOptions.delayMs);
			assert.ok(registry.size < cap, 'voice-engine-v2 timer registry invariant: size must remain below cap pre-insert');
			registry.set(timerId, {handle, scheduledAtMs});
			assert.ok(registry.size <= cap, 'voice-engine-v2 timer registry invariant: size must not exceed cap');
		},
		async cancel(timerId: string): Promise<void> {
			assert.equal(typeof timerId, 'string', 'voice-engine-v2 timer cancel timerId must be a string');
			assert.ok(timerId.length > 0, 'voice-engine-v2 timer cancel timerId must not be empty');
			const entry = registry.get(timerId);
			if (entry === undefined) return;
			scheduler.clearTimeout(entry.handle);
			registry.delete(timerId);
			assert.ok(!registry.has(timerId), 'voice-engine-v2 timer registry must not retain cancelled entries');
		},
	};
}
