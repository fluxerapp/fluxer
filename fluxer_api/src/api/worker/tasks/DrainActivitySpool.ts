// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {drainActivitySpoolNow} from '@app/api/infrastructure/activity/ActivityEvents';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const DRAIN_LOCK_KEY = 'activity:spool:drain';
const DRAIN_LOCK_TTL_SECONDS = 30;

const drainActivitySpool: WorkerTaskHandler = async (_payload, helpers) => {
	const {kvClient} = getWorkerDependencies();
	const token = randomUUID();
	if (!(await kvClient.acquireLock(DRAIN_LOCK_KEY, token, DRAIN_LOCK_TTL_SECONDS))) return;
	try {
		const result = await drainActivitySpoolNow({
			keepLock: () => kvClient.extendLock(DRAIN_LOCK_KEY, token, DRAIN_LOCK_TTL_SECONDS),
		});
		if (!result) return;
		if (result.published + result.expired + result.failed > 0 || result.error) {
			helpers.logger.info(
				{
					published: result.published,
					expired: result.expired,
					failed: result.failed,
					remaining: result.remaining,
					err: result.error ?? undefined,
				},
				'Drained activity spool',
			);
		}
	} finally {
		await kvClient.releaseLock(DRAIN_LOCK_KEY, token);
	}
};

export default drainActivitySpool;
