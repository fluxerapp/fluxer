// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {Config} from '@app/api/Config';
import {ContentBlocklistCategory} from '@app/api/constants/ContentModeration';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {BannedFileShaRow} from '@app/api/database/types/AdminArchiveTypes';
import {dockerAvailable, startPostgresKv, stopPostgresKv} from '@app/api/test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import syncFileShaBlocklists from '@app/api/worker/tasks/SyncFileShaBlocklists';
import {
	clearWorkerDependencies,
	getWorkerDependencies,
	setWorkerDependenciesForTest,
} from '@app/api/worker/WorkerContext';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const KV_TABLE = 'kv_blocklist_feeds_disabled';
const FEED_SHA = 'a1'.repeat(32);
const ADMIN_BAZAAR_SHA = 'b2'.repeat(32);
const ADMIN_MANUAL_SHA = 'c3'.repeat(32);
const ADMIN_USER_ID = 1_234_567_890_123n;

function fileShaRow(sha256Hex: string, category: string, addedBy: bigint | null): BannedFileShaRow {
	return {
		sha256_hex: sha256Hex,
		category,
		severity: 2,
		content_type: null,
		source_url: null,
		added_at: new Date(),
		added_by: addedBy,
		notes: null,
	};
}

describe.skipIf(!dockerAvailable)('blocklist feeds turned off against postgres', () => {
	let raw: IPostgresClient;
	let executor: PostgresKvQueryExecutor;
	let previousFeedsEnabled: boolean;

	let postgresContainer: string;
	beforeAll(async () => {
		const started = await startPostgresKv({namePrefix: 'fluxer-feeds', kvTable: KV_TABLE, maxConnections: 4});
		postgresContainer = started.container;
		raw = started.client;
		executor = new PostgresKvQueryExecutor(raw);
	}, 900_000);

	beforeEach(async () => {
		previousFeedsEnabled = Config.blocklistFeeds.enabled;
		await raw.query(`DELETE FROM ${KV_TABLE}`);
		setCassandraQueryExecutorForTesting(executor);
		setWorkerDependenciesForTest({
			adminRepository: new AdminRepository(),
			kvClient: new MockKVProvider(),
			storageService: new MockStorageService(),
		});
		Config.blocklistFeeds.enabled = false;
	});

	afterEach(() => {
		Config.blocklistFeeds.enabled = previousFeedsEnabled;
	});

	afterAll(async () => {
		clearWorkerDependencies();
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		await stopPostgresKv(postgresContainer);
	});

	it('keeps file-SHA rows with added_by set and removes feed rows stored with a JSON null added_by', async () => {
		const repository = new AdminRepository();
		await repository.banFileSha(fileShaRow(FEED_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, null));
		await repository.banFileSha(fileShaRow(ADMIN_BAZAAR_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, ADMIN_USER_ID));
		await repository.banFileSha(fileShaRow(ADMIN_MANUAL_SHA, ContentBlocklistCategory.MANUAL, ADMIN_USER_ID));
		const feedRow = await raw.query<{added_by_type: string}>(
			`SELECT jsonb_typeof(row_data->'added_by') AS added_by_type FROM ${KV_TABLE}
WHERE table_name = 'banned_file_shas' AND row_data->>'sha256_hex' = $1`,
			[FEED_SHA],
		);
		expect(feedRow.rows).toEqual([{added_by_type: 'null'}]);

		await syncFileShaBlocklists({}, createTestWorkerHelpers({jobId: 4242n}));

		const remaining = await raw.query<{sha: string}>(
			`SELECT row_data->>'sha256_hex' AS sha FROM ${KV_TABLE} WHERE table_name = 'banned_file_shas' ORDER BY 1`,
		);
		expect(remaining.rows.map((row) => row.sha)).toEqual([ADMIN_BAZAAR_SHA, ADMIN_MANUAL_SHA]);
	});

	it('keeps a file-SHA ban an Admin took over after the purge read the table', async () => {
		const {adminRepository} = getWorkerDependencies();
		await adminRepository.banFileSha(fileShaRow(FEED_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, null));
		const staleRows = await adminRepository.loadAllBannedFileShas();
		await adminRepository.banFileSha(fileShaRow(FEED_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, ADMIN_USER_ID));
		const staleRead = vi.spyOn(adminRepository, 'loadAllBannedFileShas').mockResolvedValueOnce(staleRows);

		await syncFileShaBlocklists({}, createTestWorkerHelpers({jobId: 4242n}));

		staleRead.mockRestore();
		expect((await adminRepository.loadAllBannedFileShas()).map((row) => row.sha256_hex)).toContain(FEED_SHA);
	});
});
