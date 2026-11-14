// SPDX-License-Identifier: AGPL-3.0-or-later

import {JOB_STALE_AFTER_MS, JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const PAGE_SIZE = 500;
const MAX_CLEARED_PER_RUN = 1000;

const expireStaleJobs: WorkerTaskHandler = async (_payload, helpers) => {
	const result = await new JobLedgerRepository().expireStaleActiveJobs({
		staleBeforeMs: Date.now() - JOB_STALE_AFTER_MS,
		pageSize: PAGE_SIZE,
		maxCleared: MAX_CLEARED_PER_RUN,
	});
	if (!result.complete) {
		helpers.logger.warn({...result}, 'Stale job sweep reached its per-run cap');
	} else if (result.cleared > 0) {
		helpers.logger.info({...result}, 'Expired stale jobs');
	}
};

export default expireStaleJobs;
