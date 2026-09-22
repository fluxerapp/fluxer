// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID, createUserID} from '@app/api/BrandedTypes';
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
import {ReportStatus} from '@app/api/report/IReportRepository';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {drainSearchTasks} from '@app/api/search/SearchTaskTracker';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import bulkScheduleUserDeletion from '@app/api/worker/tasks/admin_bulk/BulkScheduleUserDeletion';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHelpers, WorkerTaskResult} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ReportResponse {
	report_id: string;
}

interface BulkJobResult {
	successful_count: number;
	failed_count: number;
	failed: Array<{id: string; error: string}>;
}

const AUDIT_LOG_REASON = 'Lilith spam sweep 2026-09-14';

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

function installWorkerDependencies(): void {
	setWorkerDependenciesForTest({
		adminRepository: getAdminRepository(),
		snowflakeService: getSnowflakeService(),
		userRepository: getUserRepository(),
		userCacheService: createUserCacheService(),
		guildRepository: getGuildRepository(),
		channelRepository: getChannelRepository(),
		gatewayService: getGatewayService(),
		deletionQueueService: getKVAccountDeletionQueue(),
		bulkMessageDeletionQueueService: getKVBulkMessageDeletionQueue(),
		stripe: null,
	});
}

async function runBulkJob(
	userIds: Array<string>,
	adminUserId: string,
	reasonCode: number = DeletionReasons.SPAM,
): Promise<BulkJobResult> {
	installWorkerDependencies();
	const result = (await bulkScheduleUserDeletion(
		{
			user_ids: userIds,
			reason_code: reasonCode,
			days_until_deletion: 60,
			public_reason: null,
			admin_user_id: adminUserId,
			audit_log_reason: AUDIT_LOG_REASON,
		},
		createHelpers(),
	)) as WorkerTaskResult;
	return result as unknown as BulkJobResult;
}

async function reportUser(harness: ApiTestHarness, reporter: TestAccount, targetUserId: string): Promise<string> {
	const report = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/user')
		.body({user_id: targetUserId, category: 'spam_account'})
		.expect(HTTP_STATUS.OK)
		.execute();
	await drainSearchTasks();
	return report.report_id;
}

async function getReportStatus(reportId: string): Promise<number | null> {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	return report?.status ?? null;
}

async function listAuditLogs(action: string): Promise<Array<AdminAuditLog>> {
	const logs = await new AdminRepository().listAllAuditLogsPaginated(500);
	return logs.filter((log) => log.action === action);
}

async function isSessionAlive(harness: ApiTestHarness, token: string): Promise<boolean> {
	const response = await harness.requestJson({path: '/users/@me', headers: {Authorization: token}});
	return response.status === HTTP_STATUS.OK;
}

describe('bulkScheduleUserDeletion', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		clearWorkerDependencies();
		await harness.shutdown();
	});
	test('runs the same side effects as the single-user endpoint for every user in the job', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		expect(await getReportStatus(reportId)).toBe(ReportStatus.PENDING);
		const result = await runBulkJob([target.userId], admin.userId);
		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(0);
		expect(await getReportStatus(reportId)).toBe(ReportStatus.RESOLVED);
		const perUserLogs = await listAuditLogs('schedule_deletion');
		const targetLog = perUserLogs.find((log) => log.targetId === BigInt(target.userId));
		expect(targetLog).toBeDefined();
		expect(targetLog!.auditLogReason).toBe(AUDIT_LOG_REASON);
		expect(targetLog!.adminUserId.toString()).toBe(admin.userId);
		expect(targetLog!.metadata.get('reason_code')).toBe(DeletionReasons.SPAM.toString());
		expect(targetLog!.metadata.get('days')).toBe('60');
		expect(await isSessionAlive(harness, target.token)).toBe(false);
		expect(await new AdminRepository().isEmailBanned(target.email)).toBe(true);
		const resolutionLogs = await listAuditLogs('auto_resolve_reports_on_deletion');
		expect(resolutionLogs.some((log) => log.targetId === BigInt(target.userId))).toBe(true);
		const summaryLogs = await listAuditLogs('bulk_schedule_deletion');
		expect(summaryLogs).toHaveLength(1);
		expect(summaryLogs[0]!.targetType).toBe('bulk_job');
		expect(summaryLogs[0]!.targetId).toBe(4242n);
		expect(summaryLogs[0]!.auditLogReason).toBe(AUDIT_LOG_REASON);
		expect(summaryLogs[0]!.metadata.get('user_count')).toBe('1');
		expect(summaryLogs[0]!.metadata.get('reason_code')).toBe(DeletionReasons.SPAM.toString());
		expect(summaryLogs[0]!.metadata.get('days')).toBe('60');
		expect(summaryLogs[0]!.metadata.get('successful')).toBe('1');
		expect(summaryLogs[0]!.metadata.get('failed')).toBe('0');
	});
	test('the single-user endpoint produces the same report, audit, session and ban outcome', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.header('X-Audit-Log-Reason', AUDIT_LOG_REASON)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await getReportStatus(reportId)).toBe(ReportStatus.RESOLVED);
		const perUserLogs = await listAuditLogs('schedule_deletion');
		const targetLog = perUserLogs.find((log) => log.targetId === BigInt(target.userId));
		expect(targetLog).toBeDefined();
		expect(targetLog!.auditLogReason).toBe(AUDIT_LOG_REASON);
		expect(targetLog!.metadata.get('reason_code')).toBe(DeletionReasons.SPAM.toString());
		expect(await isSessionAlive(harness, target.token)).toBe(false);
		expect(await new AdminRepository().isEmailBanned(target.email)).toBe(true);
	});
	test('keeps deleting the remaining users after one of them fails and reports the failure', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const missingUserId = '999999999999999999';
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		const result = await runBulkJob([missingUserId, target.userId], admin.userId);
		expect(result.successful_count).toBe(1);
		expect(result.failed_count).toBe(1);
		expect(result.failed.map((failure) => failure.id)).toEqual([missingUserId]);
		expect(result.failed[0]!.error).toBeTruthy();
		const updatedTarget = await getUserRepository().findUnique(createUserID(BigInt(target.userId)));
		expect(updatedTarget).not.toBeNull();
		expect(updatedTarget!.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
		expect(updatedTarget!.pendingDeletionAt).not.toBeNull();
		expect(await getReportStatus(reportId)).toBe(ReportStatus.RESOLVED);
		const perUserLogs = await listAuditLogs('schedule_deletion');
		expect(perUserLogs.filter((log) => log.targetId === BigInt(target.userId))).toHaveLength(1);
		expect(perUserLogs.some((log) => log.targetId === BigInt(missingUserId))).toBe(false);
		const summaryLogs = await listAuditLogs('bulk_schedule_deletion');
		expect(summaryLogs).toHaveLength(1);
		expect(summaryLogs[0]!.metadata.get('successful')).toBe('1');
		expect(summaryLogs[0]!.metadata.get('failed')).toBe('1');
	});
	test('a user-requested bulk deletion neither bans identifiers nor resolves reports', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		const result = await runBulkJob([target.userId], admin.userId, DeletionReasons.USER_REQUESTED);
		expect(result.successful_count).toBe(1);
		expect(await getReportStatus(reportId)).toBe(ReportStatus.PENDING);
		expect(await new AdminRepository().isEmailBanned(target.email)).toBe(false);
		const perUserLogs = await listAuditLogs('schedule_deletion');
		expect(perUserLogs.filter((log) => log.targetId === BigInt(target.userId))).toHaveLength(1);
	});
});
