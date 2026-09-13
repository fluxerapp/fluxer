// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {pruneExpiredPostgresKvRows} from '@app/api/database/PostgresKvQueryExecutor';
import {getDefaultPostgresClient} from '@pkgs/postgres/src/Client';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const PRUNE_BATCH_SIZE = 5000;
const MAX_PRUNE_BATCHES_PER_RUN = 20;

const prunePostgresKvTtl: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.database.backend !== 'postgres') {
		return;
	}
	const client = getDefaultPostgresClient();
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
