// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getHistoricalOutcomeRepository} from '@app/api/middleware/ServiceMiddleware';
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
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import bulkUpdateSuspiciousActivityFlags from '@app/api/worker/tasks/admin_bulk/BulkUpdateSuspiciousActivityFlags';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	PHONE_GATE_PROMOTED_FROM_DEFERRAL,
	SuspiciousActivityFlags,
} from '@fluxer/constants/src/UserConstants';
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
const JOB_ID = 7200000000000000000n;
const MISSING_USER_ID = 4200000000000000002n;
const RISK_CONTEXT_IP = '203.0.113.77';

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

describe('bulkUpdateSuspiciousActivityFlags task', () => {
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

	async function setSuspiciousFlags(account: TestAccount, flags: number): Promise<void> {
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flags: flags,
			})
			.execute();
	}

	async function readSuspiciousFlags(account: TestAccount): Promise<number> {
		const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
		return user!.suspiciousActivityFlags ?? 0;
	}

	test('writes a per-user audit row with the admin reason, records the risk outcome, and reports failures', async () => {
		const account = await createTestAccount(harness);
		await setSuspiciousFlags(account, 0);
		await getHistoricalOutcomeRepository().upsertLatestContext({
			userId: account.userId,
			ip: RISK_CONTEXT_IP,
			subnet: null,
			emailDomain: null,
			asn: null,
			updatedAt: new Date(),
		});

		const result = (await bulkUpdateSuspiciousActivityFlags(
			{
				user_ids: [account.userId, MISSING_USER_ID.toString()],
				add_flags: ['REQUIRE_VERIFIED_EMAIL'],
				remove_flags: [],
				admin_user_id: ADMIN_USER_ID.toString(),
				audit_log_reason: 'Lilith verification sweep',
			},
			createHelpers(),
		)) as unknown as BulkJobResult;

		expect(result.successful_count).toBe(1);
		expect(result.failed).toEqual([{id: MISSING_USER_ID.toString(), error: 'UNKNOWN_USER'}]);
		expect(await readSuspiciousFlags(account)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);

		const auditLogs = await new AdminRepository().listAllAuditLogsPaginated(50);
		const perUserLogs = auditLogs.filter((log) => log.action === 'update_suspicious_activity_flags');
		expect(perUserLogs).toHaveLength(1);
		expect(perUserLogs[0]!.targetType).toBe('user');
		expect(perUserLogs[0]!.targetId).toBe(BigInt(account.userId));
		expect(perUserLogs[0]!.adminUserId.toString()).toBe(ADMIN_USER_ID.toString());
		expect(perUserLogs[0]!.auditLogReason).toBe('Lilith verification sweep');
		expect(perUserLogs[0]!.metadata.get('flags')).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL.toString());

		const summaryLogs = auditLogs.filter((log) => log.action === 'bulk_update_suspicious_activity_flags');
		expect(summaryLogs).toHaveLength(1);
		expect(summaryLogs[0]!.targetType).toBe('bulk_job');
		expect(summaryLogs[0]!.targetId).toBe(JOB_ID);
		expect(summaryLogs[0]!.auditLogReason).toBe('Lilith verification sweep');
		expect(summaryLogs[0]!.metadata.get('user_count')).toBe('2');
		expect(summaryLogs[0]!.metadata.get('add_flags')).toBe('REQUIRE_VERIFIED_EMAIL');
		expect(summaryLogs[0]!.metadata.has('remove_flags')).toBe(false);
		expect(summaryLogs[0]!.metadata.get('successful')).toBe('1');
		expect(summaryLogs[0]!.metadata.get('failed')).toBe('1');

		const outcomes = await getHistoricalOutcomeRepository().listByIp(RISK_CONTEXT_IP, new Date(0), 50);
		expect(outcomes.map((outcome) => [outcome.outcomeCode, outcome.source])).toEqual([
			['challenged', 'admin_update_suspicious_activity_flags'],
		]);
	});

	test('removing a phone requirement clears the deferral bookkeeping bits', async () => {
		const account = await createTestAccount(harness);
		await setSuspiciousFlags(
			account,
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
				DEFERRED_PHONE_ON_COMMUNITY_JOIN |
				PHONE_GATE_PROMOTED_FROM_DEFERRAL,
		);

		const result = (await bulkUpdateSuspiciousActivityFlags(
			{
				user_ids: [account.userId],
				add_flags: [],
				remove_flags: ['REQUIRE_VERIFIED_PHONE'],
				admin_user_id: ADMIN_USER_ID.toString(),
				audit_log_reason: 'Phone gate lifted',
			},
			createHelpers(),
		)) as unknown as BulkJobResult;

		expect(result.failed).toEqual([]);
		expect(await readSuspiciousFlags(account)).toBe(0);

		const auditLogs = await new AdminRepository().listAllAuditLogsPaginated(50);
		const perUserLogs = auditLogs.filter((log) => log.action === 'update_suspicious_activity_flags');
		expect(perUserLogs).toHaveLength(1);
		expect(perUserLogs[0]!.auditLogReason).toBe('Phone gate lifted');
		expect(perUserLogs[0]!.metadata.get('flags')).toBe('0');
	});

	test('keeps the deferral bits when the deferrable phone flags are unchanged', async () => {
		const account = await createTestAccount(harness);
		await setSuspiciousFlags(
			account,
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		);

		await bulkUpdateSuspiciousActivityFlags(
			{
				user_ids: [account.userId],
				add_flags: ['REQUIRE_VERIFIED_EMAIL'],
				remove_flags: [],
				admin_user_id: ADMIN_USER_ID.toString(),
				audit_log_reason: 'Add email requirement',
			},
			createHelpers(),
		);

		expect(await readSuspiciousFlags(account)).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
				DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		);
	});

	test('rejects an unknown suspicious flag name instead of silently ignoring it', async () => {
		const account = await createTestAccount(harness);
		await setSuspiciousFlags(account, 0);

		await expect(
			bulkUpdateSuspiciousActivityFlags(
				{
					user_ids: [account.userId],
					add_flags: ['REQUIRE_VERIFIED_EMAIL', 'REQUIRE_VERIFIED_EMAILL'],
					remove_flags: [],
					admin_user_id: ADMIN_USER_ID.toString(),
					audit_log_reason: 'Typo sweep',
				},
				createHelpers(),
			),
		).rejects.toThrow('Unknown suspicious activity flag names: REQUIRE_VERIFIED_EMAILL');

		expect(await readSuspiciousFlags(account)).toBe(0);
		const auditLogs = await new AdminRepository().listAllAuditLogsPaginated(50);
		expect(auditLogs).toEqual([]);
	});
});
