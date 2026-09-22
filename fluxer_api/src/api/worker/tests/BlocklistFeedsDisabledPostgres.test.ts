// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {AdminRepository} from '@app/api/admin/AdminRepository';
import {Config} from '@app/api/Config';
import {ContentBlocklistCategory} from '@app/api/constants/ContentModeration';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {BannedFileShaRow} from '@app/api/database/types/AdminArchiveTypes';
import {startDockerContainer} from '@app/api/test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import syncDisposableEmailDomains from '@app/api/worker/tasks/SyncDisposableEmailDomains';
import syncFileShaBlocklists from '@app/api/worker/tasks/SyncFileShaBlocklists';
import {
	clearWorkerDependencies,
	getWorkerDependencies,
	setWorkerDependenciesForTest,
} from '@app/api/worker/WorkerContext';
import {
	getDefaultPostgresClient,
	type IPostgresClient,
	initPostgres,
	shutdownPostgres,
} from '@pkgs/postgres/src/Client';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const KV_TABLE = 'kv_blocklist_feeds_disabled';
const CONTAINER = `fluxer-feeds-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;
const FEED_SHA = 'a1'.repeat(32);
const ADMIN_BAZAAR_SHA = 'b2'.repeat(32);
const ADMIN_MANUAL_SHA = 'c3'.repeat(32);
const ADMIN_USER_ID = 1_234_567_890_123n;

async function sleep(ms: number): Promise<void> {
	await new Promise((resolve) => setTimeout(resolve, ms));
}

async function freePort(): Promise<number> {
	return new Promise((resolve, reject) => {
		const server = createServer();
		server.on('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const address = server.address();
			if (typeof address === 'string' || address === null) {
				reject(new Error('no port'));
				return;
			}
			const port = address.port;
			server.close(() => resolve(port));
		});
	});
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 4242n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

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

	beforeAll(async () => {
		const port = await freePort();
		startDockerContainer([
			'run',
			'-d',
			'--name',
			CONTAINER,
			'-e',
			'POSTGRES_USER=fluxer',
			'-e',
			'POSTGRES_PASSWORD=fluxer',
			'-e',
			'POSTGRES_DB=fluxer',
			'-p',
			`127.0.0.1:${port}:5432`,
			'postgres:16-alpine',
			'-c',
			'fsync=off',
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await sleep(500);
			const probe = spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({
					url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`,
					maxConnections: 4,
					kvTable: KV_TABLE,
				});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		raw = getDefaultPostgresClient();
		await ensurePostgresKvSchema(raw);
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
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('removes every disposable_email_domains row from the KV table', async () => {
		const repository = new AdminRepository();
		for (let i = 0; i < 1200; i++) {
			await repository.addDisposableEmailDomain(`disposable-${i}.example`);
		}
		const seeded = await raw.query<{count: number}>(
			`SELECT count(*)::int AS count FROM ${KV_TABLE} WHERE table_name = 'disposable_email_domains'`,
		);
		expect(seeded.rows[0]?.count).toBe(1200);

		await syncDisposableEmailDomains({}, createHelpers());

		const remaining = await raw.query<{count: number}>(
			`SELECT count(*)::int AS count FROM ${KV_TABLE} WHERE table_name = 'disposable_email_domains'`,
		);
		expect(remaining.rows[0]?.count).toBe(0);
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

		await syncFileShaBlocklists({}, createHelpers());

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

		await syncFileShaBlocklists({}, createHelpers());

		staleRead.mockRestore();
		expect(await adminRepository.isFileShaBanned(FEED_SHA)).toBe(true);
	});
});
