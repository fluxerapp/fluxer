// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const IDLE_DURATION_MS = 600_000;
const IDLE_CHECK_INTERVAL_MS = 30_000;

vi.mock('@app/features/platform/types/Env', () => ({
	IS_DEV: false,
	IS_PROD: true,
	MODE: 'test',
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {updatePresence: vi.fn()},
}));

async function loadIdle(getSystemIdleTimeMs?: () => Promise<number>) {
	vi.resetModules();
	if (getSystemIdleTimeMs) {
		Object.defineProperty(window, 'electron', {value: {getSystemIdleTimeMs}, configurable: true, writable: true});
	} else {
		Reflect.deleteProperty(window as unknown as Record<string, unknown>, 'electron');
	}
	const {default: Idle} = await import('@app/features/ui/state/Idle');
	return Idle;
}

describe('Idle', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		Reflect.deleteProperty(window as unknown as Record<string, unknown>, 'electron');
		vi.useRealTimers();
	});

	it('goes idle once nothing has reported activity for the idle duration', async () => {
		const Idle = await loadIdle();
		await vi.advanceTimersByTimeAsync(IDLE_DURATION_MS - IDLE_CHECK_INTERVAL_MS);
		expect(Idle.isIdle()).toBe(false);
		await vi.advanceTimersByTimeAsync(IDLE_CHECK_INTERVAL_MS);
		expect(Idle.isIdle()).toBe(true);
	});

	it('stays active while the app keeps seeing input', async () => {
		const Idle = await loadIdle();
		for (let elapsed = 0; elapsed < IDLE_DURATION_MS * 2; elapsed += IDLE_CHECK_INTERVAL_MS) {
			Idle.recordActivity();
			await vi.advanceTimersByTimeAsync(IDLE_CHECK_INTERVAL_MS);
		}
		expect(Idle.isIdle()).toBe(false);
	});

	it('stays active while the app sees input even when the system idle clock keeps climbing', async () => {
		const startedAt = Date.now();
		const Idle = await loadIdle(async () => Date.now() - startedAt);
		for (let elapsed = 0; elapsed < IDLE_DURATION_MS * 2; elapsed += IDLE_CHECK_INTERVAL_MS) {
			Idle.recordActivity();
			await vi.advanceTimersByTimeAsync(IDLE_CHECK_INTERVAL_MS);
		}
		expect(Idle.isIdle()).toBe(false);
	});

	it('leaves idle as soon as the app sees input again', async () => {
		const startedAt = Date.now();
		const Idle = await loadIdle(async () => Date.now() - startedAt);
		await vi.advanceTimersByTimeAsync(IDLE_DURATION_MS);
		expect(Idle.isIdle()).toBe(true);
		Idle.recordActivity();
		expect(Idle.isIdle()).toBe(false);
		await vi.advanceTimersByTimeAsync(IDLE_CHECK_INTERVAL_MS);
		expect(Idle.isIdle()).toBe(false);
	});

	it('stays active while the system reports input the app never sees', async () => {
		const Idle = await loadIdle(async () => 0);
		await vi.advanceTimersByTimeAsync(IDLE_DURATION_MS * 2);
		expect(Idle.isIdle()).toBe(false);
	});

	it('goes idle when neither the app nor the system reports activity', async () => {
		const startedAt = Date.now();
		const Idle = await loadIdle(async () => Date.now() - startedAt);
		await vi.advanceTimersByTimeAsync(IDLE_DURATION_MS);
		expect(Idle.isIdle()).toBe(true);
	});

	it('falls back to app activity when the system idle clock is unreadable', async () => {
		const Idle = await loadIdle(async () => Number.NaN);
		await vi.advanceTimersByTimeAsync(IDLE_DURATION_MS);
		expect(Idle.isIdle()).toBe(true);
		Idle.recordActivity();
		await vi.advanceTimersByTimeAsync(IDLE_CHECK_INTERVAL_MS);
		expect(Idle.isIdle()).toBe(false);
	});
});
