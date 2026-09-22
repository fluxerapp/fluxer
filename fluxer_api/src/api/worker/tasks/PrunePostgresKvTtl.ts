// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {expireLegacyDefaultTtlRows} from '@app/api/database/PostgresKvDefaultTtlExpiry';
import {pruneExpiredPostgresKvRows} from '@app/api/database/PostgresKvQueryExecutor';
import {expireLegacyJobLedgerRows} from '@app/api/jobs/PostgresJobLedgerExpiry';
import {getDefaultPostgresClient} from '@pkgs/postgres/src/Client';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const PRUNE_BATCH_SIZE = 5000;
const MAX_PRUNE_BATCHES_PER_RUN = 20;
const LEGACY_EXPIRY_BUDGET_MS = ms('2 minutes');

const prunePostgresKvTtl: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.database.backend !== 'postgres') {
		return;
	}
	const client = getDefaultPostgresClient();
	const deadlineMs = Date.now() + LEGACY_EXPIRY_BUDGET_MS;
	const legacyJobs = await expireLegacyJobLedgerRows(client, deadlineMs);
	if (legacyJobs !== null && (legacyJobs.deleted > 0 || legacyJobs.expiring > 0 || !legacyJobs.complete)) {
		helpers.logger.info({...legacyJobs}, 'Expired legacy job ledger rows');
	}
	const legacyDefaults = await expireLegacyDefaultTtlRows(client, deadlineMs);
	if (
		legacyDefaults !== null &&
		(legacyDefaults.deleted > 0 || legacyDefaults.expiring > 0 || !legacyDefaults.complete)
	) {
		helpers.logger.info({...legacyDefaults}, 'Expired rows written without their table default TTL');
	}
	let deleted = 0;
	for (let batch = 0; batch < MAX_PRUNE_BATCHES_PER_RUN; batch += 1) {
		const batchDeleted = await pruneExpiredPostgresKvRows(client, PRUNE_BATCH_SIZE);
		deleted += batchDeleted;
		if (batchDeleted < PRUNE_BATCH_SIZE) {
			break;
		}
	}
	if (deleted > 0) {
		helpers.logger.info({deleted}, 'Pruned expired Postgres KV rows');
	}
};

export default prunePostgresKvTtl;
