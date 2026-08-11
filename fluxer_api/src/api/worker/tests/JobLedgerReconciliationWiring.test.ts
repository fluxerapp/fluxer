// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import type {CronScheduler} from '../CronScheduler';
import {registerCronJobs} from '../WorkerMain';
import {workerTasks} from '../WorkerTaskRegistry';

describe('job-ledger reconciliation wiring', () => {
	it('registers a periodic unledgered reconciliation task', () => {
		const upsert = vi.fn();

		registerCronJobs({upsert} as unknown as CronScheduler);

		expect(workerTasks).toHaveProperty('reconcileActiveJobs');
		expect(upsert).toHaveBeenCalledWith('reconcileActiveJobs', 'reconcileActiveJobs', {}, '0 0 * * * *');
	});
});
