// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {BANNED_FILE_SHAS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import bulkBanFileShas from '@app/api/worker/tasks/admin_bulk/BulkBanFileShas';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {JobCancelledError, type WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BulkJobResult {
	successful_count: number;
	failed_count: number;
	failed: Array<{
		id: string;
		error: string;
	}>;
}

const FIRST_SHA = 'a1'.repeat(32);
const SECOND_SHA = 'b2'.repeat(32);
const NON_HEX_SHA = 'zz'.repeat(32);

function createHelpers(overrides: Partial<WorkerTaskHelpers> = {}): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 4242n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
		...overrides,
	};
}

describe('Bulk ban file SHAs', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setWorkerDependenciesForTest({
			adminRepository: getAdminRepository(),
			snowflakeService: getSnowflakeService(),
		});
		setInjectedWorkerService(new SyncTaskWorkerService({bulkBanFileShas}));
	});

	afterAll(async () => {
		clearWorkerDependencies();
		setInjectedWorkerService(new NoopWorkerService());
		await harness?.shutdown();
	});

	function refreshPublishes(): Array<Array<string>> {
		return (harness.kvProvider as MockKVProvider).publishSpy.mock.calls.filter(
			([channel]: Array<string>) => channel === BANNED_FILE_SHAS_REFRESH_CHANNEL,
		);
	}

	async function listAuditLogs(): Promise<Array<AdminAuditLog>> {
		return getAdminRepository().listAllAuditLogsPaginated(100);
	}

	async function runTask(payload: Record<string, unknown>, helpers = createHelpers()): Promise<BulkJobResult> {
		return (await bulkBanFileShas(payload, helpers)) as unknown as BulkJobResult;
	}

	it('bans every hash with its own audit row and publishes one cache refresh', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.BAN_FILE_SHA_ADD,
		]);

		await createBuilder(harness, admin.token)
			.put('/admin/blocklists/file-sha/entries')
			.header('X-Audit-Log-Reason', 'CSAM hash feed')
			.body({sha256_list: [FIRST_SHA, SECOND_SHA]})
			.execute();

		expect(refreshPublishes()).toEqual([[BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh']]);
		const logs = await listAuditLogs();
		expect(
			logs
				.filter((log) => log.action === 'ban_file_sha')
				.map((log) => ({
					adminUserId: log.adminUserId.toString(),
					targetType: log.targetType,
					auditLogReason: log.auditLogReason,
					sha256: log.metadata.get('sha256'),
				})),
		).toEqual([
			{adminUserId: admin.userId, targetType: 'file_sha', auditLogReason: 'CSAM hash feed', sha256: FIRST_SHA},
			{adminUserId: admin.userId, targetType: 'file_sha', auditLogReason: 'CSAM hash feed', sha256: SECOND_SHA},
		]);
		const summary = logs.find((log) => log.action === 'bulk_ban_file_shas');
		expect(summary).toBeDefined();
		expect(summary?.targetType).toBe('bulk_job');
		expect(summary?.auditLogReason).toBe('CSAM hash feed');
		expect(Object.fromEntries(summary!.metadata)).toMatchObject({
			count: '2',
			processed: '2',
			successful: '2',
			failed: '0',
		});
	});

	it('reports a non-hexadecimal hash as a failed item and still bans the rest', async () => {
		const result = await runTask({
			sha256_list: [NON_HEX_SHA, FIRST_SHA],
			admin_user_id: '7',
			audit_log_reason: 'Feed import',
		});

		expect(result.successful_count).toBe(1);
		expect(result.failed).toEqual([{id: NON_HEX_SHA, error: 'invalid_sha256'}]);
		const logs = await listAuditLogs();
		expect(logs.filter((log) => log.action === 'ban_file_sha').map((log) => log.metadata.get('sha256'))).toEqual([
			FIRST_SHA,
		]);
	});

	it('publishes the cache refresh for the hashes already banned when the job is cancelled', async () => {
		let checks = 0;
		const helpers = createHelpers({
			shouldCancel: async () => {
				checks += 1;
				return checks > 1;
			},
		});

		await expect(
			bulkBanFileShas(
				{sha256_list: [FIRST_SHA, SECOND_SHA], admin_user_id: '7', audit_log_reason: 'Feed import'},
				helpers,
			),
		).rejects.toBeInstanceOf(JobCancelledError);

		expect(refreshPublishes()).toEqual([[BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh']]);
		const logs = await listAuditLogs();
		expect(logs.filter((log) => log.action === 'ban_file_sha').map((log) => log.metadata.get('sha256'))).toEqual([
			FIRST_SHA,
		]);
		expect(Object.fromEntries(logs.find((log) => log.action === 'bulk_ban_file_shas')!.metadata)).toMatchObject({
			successful: '1',
			cancelled: 'true',
		});
	});
});
