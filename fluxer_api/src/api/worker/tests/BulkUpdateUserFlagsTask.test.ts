// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getGatewayService, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	getAdminRepository,
	getChannelRepository,
	getGuildRepository,
	getKVAccountDeletionQueue,
	getKVBulkMessageDeletionQueue,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import bulkUpdateUserFlags from '@app/api/worker/tasks/admin_bulk/BulkUpdateUserFlags';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface BulkJobResult {
	successful_count: number;
	failed_count: number;
	failed: Array<{
		id: string;
		error: string;
	}>;
}

const ADMIN_USER_ID = 4000000000000000000n;
const JOB_ID = 7100000000000000000n;
const MISSING_USER_ID = 4200000000000000001n;

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: JOB_ID,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function installWorkerDependencies(): void {
	setWorkerDependenciesForTest({
		adminRepository: getAdminRepository(),
		snowflakeService: getSnowflakeService(),
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		channelRepository: getChannelRepository(),
		userCacheService: createUserCacheService(),
		deletionQueueService: getKVAccountDeletionQueue(),
		bulkMessageDeletionQueueService: getKVBulkMessageDeletionQueue(),
		gatewayService: getGatewayService(),
		stripe: null,
	});
}

describe('bulkUpdateUserFlags task', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
		installWorkerDependencies();
	});
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('writes a per-user audit row carrying the admin reason and records failed items', async () => {
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		const result = (await bulkUpdateUserFlags(
			{
				user_ids: [first.userId, MISSING_USER_ID.toString(), second.userId],
				add_flags: [UserFlags.SPAMMER.toString()],
				remove_flags: [UserFlags.HAS_SESSION_STARTED.toString()],
				admin_user_id: ADMIN_USER_ID.toString(),
				audit_log_reason: 'Lilith spam sweep',
			},
			createHelpers(),
		)) as unknown as BulkJobResult;

		expect(result.successful_count).toBe(2);
		expect(result.failed_count).toBe(1);
		expect(result.failed).toEqual([{id: MISSING_USER_ID.toString(), error: 'UNKNOWN_USER'}]);

		const userRepository = getUserRepository();
		const updatedFirst = await userRepository.findUnique(createUserID(BigInt(first.userId)));
		expect(updatedFirst!.flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		expect(updatedFirst!.flags & UserFlags.HAS_SESSION_STARTED).toBe(0n);

		const auditLogs = await new AdminRepository().listAllAuditLogsPaginated(50);
		const perUserLogs = auditLogs.filter((log) => log.action === 'update_flags');
		expect(perUserLogs.map((log) => log.targetId).sort()).toEqual([BigInt(first.userId), BigInt(second.userId)].sort());
		for (const log of perUserLogs) {
			expect(log.targetType).toBe('user');
			expect(log.adminUserId.toString()).toBe(ADMIN_USER_ID.toString());
			expect(log.auditLogReason).toBe('Lilith spam sweep');
			expect(log.metadata.get('add_flags')).toBe(UserFlags.SPAMMER.toString());
			expect(log.metadata.get('remove_flags')).toBe(UserFlags.HAS_SESSION_STARTED.toString());
			expect(log.metadata.get('new_flags')).toBe(UserFlags.SPAMMER.toString());
		}
		expect(auditLogs.some((log) => log.action === 'update_flags' && log.targetId === MISSING_USER_ID)).toBe(false);
	});

	test('writes one summary row against the job id', async () => {
		const account = await createTestAccount(harness);
		await bulkUpdateUserFlags(
			{
				user_ids: [account.userId, MISSING_USER_ID.toString()],
				add_flags: [UserFlags.STAFF.toString()],
				remove_flags: [],
				admin_user_id: ADMIN_USER_ID.toString(),
				audit_log_reason: 'Staff grant',
			},
			createHelpers(),
		);

		const auditLogs = await new AdminRepository().listAllAuditLogsPaginated(50);
		const summaryLogs = auditLogs.filter((log) => log.action === 'bulk_update_user_flags');
		expect(summaryLogs).toHaveLength(1);
		const summary = summaryLogs[0]!;
		expect(summary.targetType).toBe('bulk_job');
		expect(summary.targetId).toBe(JOB_ID);
		expect(summary.auditLogReason).toBe('Staff grant');
		expect(summary.metadata.get('user_count')).toBe('2');
		expect(summary.metadata.get('add_flags')).toBe(UserFlags.STAFF.toString());
		expect(summary.metadata.has('remove_flags')).toBe(false);
		expect(summary.metadata.get('processed')).toBe('2');
		expect(summary.metadata.get('successful')).toBe('1');
		expect(summary.metadata.get('failed')).toBe('1');
	});
});
