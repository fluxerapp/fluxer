// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {JobLedgerRepository} from '../../jobs/JobLedgerRepository';
import {WORKER_QUEUE_RECOVERY_WINDOW_MS} from '../../jobs/JobSchedulingPolicy';

const ACTIVE_JOB_RECONCILE_GRACE_MS = ms('1 day');
const ACTIVE_JOB_RECONCILE_LIMIT = 500;

const reconcileActiveJobs: WorkerTaskHandler = async (_payload, helpers) => {
	const now = new Date();
	const result = await new JobLedgerRepository().reconcileActiveJobs({
		now,
		staleBefore: new Date(now.getTime() - WORKER_QUEUE_RECOVERY_WINDOW_MS - ACTIVE_JOB_RECONCILE_GRACE_MS),
		limit: ACTIVE_JOB_RECONCILE_LIMIT,
	});
	if (result.failedStaleQueued > 0 || result.removedMissing > 0 || result.removedTerminal > 0) {
		helpers.logger.info({...result}, 'Reconciled stale job-ledger active entries');
	} else {
		helpers.logger.debug({...result}, 'Job-ledger active entries are consistent');
	}
};

export default reconcileActiveJobs;
