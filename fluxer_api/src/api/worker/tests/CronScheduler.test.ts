// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {afterEach, describe, expect, it, vi} from 'vitest';
import {CronScheduler} from '../CronScheduler';
import type {WorkerService} from '../WorkerService';

function createLogger(): LoggerInterface {
	const logger = {
		trace: vi.fn(),
		debug: vi.fn(),
		info: vi.fn(),
		warn: vi.fn(),
		error: vi.fn(),
		child: () => logger,
	};
	return logger as unknown as LoggerInterface;
}

function createScheduler(): {
	scheduler: CronScheduler;
	addJob: ReturnType<typeof vi.fn>;
	setnx: ReturnType<typeof vi.fn>;
} {
	const addJob = vi.fn().mockResolvedValue(1n);
	const setnx = vi.fn().mockResolvedValue(true);
	const workerService = {addJob} as unknown as WorkerService;
	const kvClient = {setnx} as unknown as IKVProvider;
	return {scheduler: new CronScheduler(workerService, createLogger(), kvClient), addJob, setnx};
}

describe('CronScheduler', () => {
	afterEach(() => {
		vi.useRealTimers();
	});

	it('fires a schedule whose second was skipped by a stalled tick', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		const {scheduler, addJob, setnx} = createScheduler();
		scheduler.upsert('expireAttachments', 'expireAttachments', {}, '5 * * * * *', {ledger: false});

		scheduler.start();
		await vi.advanceTimersByTimeAsync(3000);
		expect(addJob).not.toHaveBeenCalled();

		vi.setSystemTime(new Date('2026-01-01T00:00:10.400Z'));
		await vi.advanceTimersByTimeAsync(2000);
		scheduler.stop();

		const skippedSecond = Math.floor(Date.parse('2026-01-01T00:00:05.000Z') / 1000);
		expect(addJob).toHaveBeenCalledTimes(1);
		expect(addJob).toHaveBeenCalledWith(
			'expireAttachments',
			{},
			{
				jobKey: `cron:expireAttachments:${skippedSecond}`,
				skipLedger: true,
			},
		);
		expect(setnx).toHaveBeenCalledTimes(1);
	});

	it('fires a skipped schedule once instead of once per skipped second', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		const {scheduler, addJob} = createScheduler();
		scheduler.upsert('flushUserActivityBuffer', 'flushUserActivityBuffer', {}, '*/2 * * * * *', {ledger: false});

		scheduler.start();
		await vi.advanceTimersByTimeAsync(1000);
		addJob.mockClear();

		vi.setSystemTime(new Date('2026-01-01T00:00:20.400Z'));
		await vi.advanceTimersByTimeAsync(1000);
		scheduler.stop();

		expect(addJob).toHaveBeenCalledTimes(1);
	});

	it('does not replay schedules from before the scheduler started', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:30.000Z'));
		const {scheduler, addJob} = createScheduler();
		scheduler.upsert('expireAttachments', 'expireAttachments', {}, '5 * * * * *', {ledger: false});

		scheduler.start();
		await vi.advanceTimersByTimeAsync(2000);
		scheduler.stop();

		expect(addJob).not.toHaveBeenCalled();
	});
});
