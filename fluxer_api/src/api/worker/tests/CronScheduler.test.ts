// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';
import {CronScheduler} from '../CronScheduler';
import type {WorkerService} from '../WorkerService';

function createLogger() {
	return {
		debug: vi.fn(),
		error: vi.fn(),
		info: vi.fn(),
	} as never;
}

describe('CronScheduler', () => {
	afterEach(() => {
		vi.useRealTimers();
	});

	it('does not persist routine cron ticks in the job ledger', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T06:30:00.000Z'));
		const addJob = vi.fn().mockResolvedValue(1n);
		const scheduler = new CronScheduler({addJob} as unknown as WorkerService, createLogger());
		scheduler.upsert('maintenance', 'flushUserActivityBuffer', {}, '* * * * * *');

		scheduler.start();
		await vi.advanceTimersByTimeAsync(1000);
		await scheduler.stop();

		expect(addJob).toHaveBeenCalledOnce();
		expect(addJob).toHaveBeenCalledWith('flushUserActivityBuffer', {}, expect.objectContaining({skipLedger: true}));
	});

	it('waits for an active enqueue tick during shutdown', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T06:30:00.000Z'));
		let finishEnqueue: (() => void) | undefined;
		const addJob = vi.fn(
			() =>
				new Promise<bigint>((resolve) => {
					finishEnqueue = () => resolve(1n);
				}),
		);
		const scheduler = new CronScheduler({addJob} as unknown as WorkerService, createLogger());
		scheduler.upsert('maintenance', 'flushUserActivityBuffer', {}, '* * * * * *');
		scheduler.start();
		await vi.advanceTimersByTimeAsync(1000);
		let stopped = false;
		const stopping = scheduler.stop().then(() => {
			stopped = true;
		});
		await Promise.resolve();
		expect(stopped).toBe(false);
		finishEnqueue!();
		await stopping;
		expect(stopped).toBe(true);
	});
});
