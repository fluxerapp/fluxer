// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID, createUserID} from '@app/api/BrandedTypes';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {createTestChannelService} from '@app/api/channel/tests/CrosspostTestUtils';
import {getReportServiceInstance} from '@app/api/middleware/ServiceMiddleware';
import {getGatewayService, getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	getAdminRepository,
	getChannelRepository,
	getGuildRepository,
	getKVAccountDeletionQueue,
	getKVBulkMessageDeletionQueue,
	getLimitConfigService,
	getUserPermissionUtils,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {ReportStatus} from '@app/api/report/IReportRepository';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {getReportSearchService} from '@app/api/SearchFactory';
import {drainSearchTasks} from '@app/api/search/SearchTaskTracker';
import {createTestStoreEntitlementService} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import bulkScheduleUserDeletion from '@app/api/worker/tasks/admin_bulk/BulkScheduleUserDeletion';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import type {WorkerTaskHelpers, WorkerTaskResult} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

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
		channelService: createTestChannelService(),
		userPermissionUtils: getUserPermissionUtils(),
		limitConfigService: getLimitConfigService(),
		deletionQueueService: getKVAccountDeletionQueue(),
		bulkMessageDeletionQueueService: getKVBulkMessageDeletionQueue(),
		stripe: null,
		storeEntitlementService: createTestStoreEntitlementService(),
	});
}

async function runBulkJob(
	userIds: Array<string>,
	adminUserId: string,
	reasonCode: number = DeletionReasons.SPAM,
	extraPayload: Record<string, unknown> = {},
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
			...extraPayload,
		},
		createHelpers(),
	)) as WorkerTaskResult;
	return result as unknown as BulkJobResult;
}

async function reportUser(
	harness: ApiTestHarness,
	reporter: TestAccount,
	targetUserId: string,
	category = 'spam_account',
): Promise<string> {
	const report = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/user')
		.body({user_id: targetUserId, category})
		.expect(HTTP_STATUS.OK)
		.execute();
	await drainSearchTasks();
	return report.report_id;
}

async function reportMessage(
	harness: ApiTestHarness,
	reporter: TestAccount,
	author: TestAccount,
	channelId: string,
	category: string,
): Promise<string> {
	const message = await sendChannelMessage(harness, author.token, channelId, 'Reported content');
	const report = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/message')
		.body({channel_id: channelId, message_id: message.id, category})
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

async function autoResolveSummary(targetUserId: string): Promise<Record<string, string>> {
	const logs = (await listAuditLogs('auto_resolve_reports_on_deletion')).filter(
		(log) => log.targetId === BigInt(targetUserId),
	);
	expect(logs).toHaveLength(1);
	return Object.fromEntries(logs[0]!.metadata);
}

async function reportResolvedEmails(harness: ApiTestHarness, reporter: TestAccount) {
	return (await listTestEmails(harness, {recipient: reporter.email})).filter(
		(email) => email.type === 'report_resolved',
	);
}

async function systemDmMessages(harness: ApiTestHarness, account: TestAccount): Promise<Array<string>> {
	const channels = await createBuilder<Array<{id: string; recipients?: Array<{id: string}>}>>(harness, account.token)
		.get('/users/@me/channels')
		.expect(HTTP_STATUS.OK)
		.execute();
	const systemChannels = channels.filter((channel) => channel.recipients?.some((recipient) => recipient.id === '0'));
	const messages = await Promise.all(
		systemChannels.map((channel) =>
			createBuilder<Array<{content: string | null}>>(harness, account.token)
				.get(`/channels/${channel.id}/messages?limit=50`)
				.expect(HTTP_STATUS.OK)
				.execute(),
		),
	);
	return messages.flat().map((message) => message.content ?? '');
}

const DELETION_WORDING = /delet|\bban|suspend|terminat|remov|enforce/i;

function renderGenericNotice(username: string, reportId: string): {subject: string; body: string} {
	const rendered = getEmailTemplate('report_resolved', null, {username, reportId, publicComment: '', hasComment: 'no'});
	if (!rendered.ok) {
		throw new Error('report_resolved did not render');
	}
	return rendered.value;
}

async function scheduleDeletionThroughEndpoint(harness: ApiTestHarness, target: TestAccount): Promise<void> {
	const admin = await createTestAccount(harness);
	await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
	await createBuilder(harness, admin.token)
		.put(`/admin/users/${target.userId}/deletion`)
		.header('X-Audit-Log-Reason', AUDIT_LOG_REASON)
		.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
		.expect(HTTP_STATUS.OK)
		.execute();
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
		expect(await autoResolveSummary(target.userId)).toEqual({
			outcome: 'completed',
			found_count: '1',
			resolved_count: '1',
			failed_count: '0',
			notified_count: '1',
		});
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
		expect((await autoResolveSummary(target.userId)).notified_count).toBe('1');
	});
	test('an auto-resolved report sends the reporter the generic notice through the bulk job', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		await clearTestEmails(harness);
		await runBulkJob([target.userId], admin.userId);
		const sent = await reportResolvedEmails(harness, reporter);
		expect(sent.map((email) => email.metadata)).toEqual([{report_id: reportId, public_comment: ''}]);
		const notice = renderGenericNotice('reporter', reportId);
		expect(notice.body).toContain(reportId);
		expect(`${notice.subject}\n${notice.body}`).not.toMatch(DELETION_WORDING);
		const me = await createBuilder<{username: string}>(harness, reporter.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await systemDmMessages(harness, reporter)).toEqual([renderGenericNotice(me.username, reportId).body]);
	});
	test('an auto-resolved report sends the reporter a DM and an email with no deletion wording', async () => {
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		await clearTestEmails(harness);
		await scheduleDeletionThroughEndpoint(harness, target);
		const sent = await reportResolvedEmails(harness, reporter);
		expect(sent.map((email) => email.metadata)).toEqual([{report_id: reportId, public_comment: ''}]);
		const me = await createBuilder<{username: string}>(harness, reporter.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		const dms = await systemDmMessages(harness, reporter);
		expect(dms).toEqual([renderGenericNotice(me.username, reportId).body]);
		expect(await listTestEmails(harness, {recipient: reporter.email})).toHaveLength(1);
	});
	test('an auto-resolved DSA notice sends the notifier the decision email', async () => {
		const target = await createTestAccount(harness);
		const email = createUniqueEmail('dsa-auto-resolve');
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email})
			.expect(HTTP_STATUS.OK)
			.execute();
		const code = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'dsa_report_verification')
			?.metadata.code;
		const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
			.post('/reports/dsa/email/verify')
			.body({email, code})
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await createBuilderWithoutAuth<ReportResponse>(harness)
			.post('/reports/dsa')
			.body({
				ticket,
				report_type: 'user',
				category: 'harassment',
				user_id: target.userId,
				reporter_full_legal_name: 'Jane Doe',
				reporter_country_of_residence: 'NL',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await drainSearchTasks();
		await clearTestEmails(harness);
		await scheduleDeletionThroughEndpoint(harness, target);
		expect(await getReportStatus(report.report_id)).toBe(ReportStatus.RESOLVED);
		const sent = (await listTestEmails(harness)).filter((entry) => entry.to === email);
		expect(sent.map((entry) => [entry.type, entry.metadata])).toEqual([
			['dsa_report_resolved', {report_id: report.report_id, public_comment: ''}],
		]);
		expect((await autoResolveSummary(target.userId)).notified_count).toBe('1');
	});
	test('a deletion with no pending reports records a completed run with zero counts', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const target = await createTestAccount(harness);
		await runBulkJob([target.userId], admin.userId);
		expect(await autoResolveSummary(target.userId)).toEqual({
			outcome: 'completed',
			found_count: '0',
			resolved_count: '0',
			failed_count: '0',
			notified_count: '0',
		});
	});
	test('a report search failure leaves reports pending and is recorded', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		const search = getReportSearchService()!;
		const spy = vi.spyOn(search, 'searchReports').mockRejectedValue(new Error('search cluster unreachable'));
		try {
			const result = await runBulkJob([target.userId], admin.userId);
			expect(result.successful_count).toBe(1);
		} finally {
			spy.mockRestore();
		}
		expect(await getReportStatus(reportId)).toBe(ReportStatus.PENDING);
		expect(await autoResolveSummary(target.userId)).toEqual({
			outcome: 'enumeration_failed',
			found_count: '0',
			resolved_count: '0',
			failed_count: '0',
			notified_count: '0',
		});
	});
	test('a report that fails to resolve is counted and the others still resolve', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const firstReporter = await createTestAccount(harness);
		const secondReporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const firstReportId = await reportUser(harness, firstReporter, target.userId);
		const secondReportId = await reportUser(harness, secondReporter, target.userId);
		const reportService = getReportServiceInstance();
		const resolveReport = reportService.resolveReport.bind(reportService);
		const spy = vi.spyOn(reportService, 'resolveReport').mockImplementation(async (reportId, ...rest) => {
			if (reportId.toString() === firstReportId) {
				throw new Error('storage write timed out');
			}
			return resolveReport(reportId, ...rest);
		});
		await clearTestEmails(harness);
		try {
			const result = await runBulkJob([target.userId], admin.userId);
			expect(result.successful_count).toBe(1);
		} finally {
			spy.mockRestore();
		}
		expect(await getReportStatus(firstReportId)).toBe(ReportStatus.PENDING);
		expect(await getReportStatus(secondReportId)).toBe(ReportStatus.RESOLVED);
		expect(await reportResolvedEmails(harness, firstReporter)).toEqual([]);
		expect(await reportResolvedEmails(harness, secondReporter)).toHaveLength(1);
		expect(await autoResolveSummary(target.userId)).toEqual({
			outcome: 'completed',
			found_count: '2',
			resolved_count: '1',
			failed_count: '1',
			notified_count: '1',
		});
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
	test('an abuse deletion resolves a spam report against the user', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		const result = await runBulkJob([target.userId], admin.userId, DeletionReasons.BAN_EVASION);
		expect(result.successful_count).toBe(1);
		expect(await getReportStatus(reportId)).toBe(ReportStatus.RESOLVED);
	});
	test('an abuse deletion leaves child safety, underage user and self harm reports open', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 3);
		const [target, selfHarmReporter, userReporter] = members as [TestAccount, TestAccount, TestAccount];
		const childSafetyReportId = await reportMessage(harness, owner, target, systemChannel.id, 'child_safety');
		const selfHarmReportId = await reportMessage(harness, selfHarmReporter, target, systemChannel.id, 'self_harm');
		const underageReportId = await reportUser(harness, userReporter, target.userId, 'underage_user');
		const spamReportId = await reportUser(harness, await createTestAccount(harness), target.userId);
		const result = await runBulkJob([target.userId], admin.userId, DeletionReasons.SPAM);
		expect(result.successful_count).toBe(1);
		expect(await getReportStatus(spamReportId)).toBe(ReportStatus.RESOLVED);
		expect(await getReportStatus(childSafetyReportId)).toBe(ReportStatus.PENDING);
		expect(await getReportStatus(selfHarmReportId)).toBe(ReportStatus.PENDING);
		expect(await getReportStatus(underageReportId)).toBe(ReportStatus.PENDING);
	});
	test.each([
		['OTHER', DeletionReasons.OTHER],
		['INACTIVITY', DeletionReasons.INACTIVITY],
		['CHILD_SEXUAL_CONTENT', DeletionReasons.CHILD_SEXUAL_CONTENT],
		['CHILD_SAFETY_VIOLATION', DeletionReasons.CHILD_SAFETY_VIOLATION],
	])('a deletion for %s leaves reports open', async (_label, reasonCode) => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const reportId = await reportUser(harness, reporter, target.userId);
		const result = await runBulkJob([target.userId], admin.userId, reasonCode);
		expect(result.successful_count).toBe(1);
		expect(await getReportStatus(reportId)).toBe(ReportStatus.PENDING);
		const resolutionLogs = await listAuditLogs('auto_resolve_reports_on_deletion');
		expect(resolutionLogs.some((log) => log.targetId === BigInt(target.userId))).toBe(false);
	});
	test('a payload with notify_user false emails nobody and records it in the summary', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		await clearTestEmails(harness);
		const result = await runBulkJob([first.userId, second.userId], admin.userId, DeletionReasons.SPAM, {
			notify_user: false,
		});
		expect(result.successful_count).toBe(2);
		expect(await listTestEmails(harness, {recipient: first.email})).toEqual([]);
		expect(await listTestEmails(harness, {recipient: second.email})).toEqual([]);
		const perUserLogs = await listAuditLogs('schedule_deletion');
		expect(perUserLogs.every((log) => log.metadata.get('notify_user') === 'false')).toBe(true);
		const summaryLogs = await listAuditLogs('bulk_schedule_deletion');
		expect(summaryLogs[0]!.metadata.get('notify_user')).toBe('false');
	});
	test('a payload queued without notify_user emails every user', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		await clearTestEmails(harness);
		const result = await runBulkJob([first.userId, second.userId], admin.userId);
		expect(result.successful_count).toBe(2);
		for (const target of [first, second]) {
			const emails = await listTestEmails(harness, {recipient: target.email});
			expect(emails.map((email) => email.type)).toEqual(['account_scheduled_deletion']);
		}
		const summaryLogs = await listAuditLogs('bulk_schedule_deletion');
		expect(summaryLogs[0]!.metadata.get('notify_user')).toBe('true');
	});
	test('the bulk job route queues notify_user in the payload', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const target = await createTestAccount(harness);
		const queued: Array<{task: string; payload: WorkerJobPayload}> = [];
		setInjectedWorkerService({
			addJob: async (task, payload) => {
				queued.push({task, payload});
				return 1n;
			},
			cancelJob: async () => false,
			retryDeadLetterJob: async () => false,
		});
		await createBuilder(harness, admin.token)
			.post('/admin/bulk-jobs')
			.body({
				task: 'schedule_user_deletion',
				user_ids: [target.userId],
				reason_code: DeletionReasons.SPAM,
				notify_user: false,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(queued).toHaveLength(1);
		expect(queued[0]!.task).toBe('bulkScheduleUserDeletion');
		expect(queued[0]!.payload.notify_user).toBe(false);
	});
	test('leaves a deletion another admin already scheduled in place and reports the user as not scheduled', async () => {
		const scheduler = await createTestAccount(harness);
		await setUserACLs(harness, scheduler, ['admin:authenticate', 'user:delete']);
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const scheduled = await createTestAccount(harness);
		const fresh = await createTestAccount(harness);
		await createBuilder(harness, scheduler.token)
			.put(`/admin/users/${scheduled.userId}/deletion`)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 90})
			.expect(HTTP_STATUS.OK)
			.execute();
		const before = await getUserRepository().findUnique(createUserID(BigInt(scheduled.userId)));
		const result = await runBulkJob([scheduled.userId, fresh.userId], admin.userId);
		expect(result.successful_count).toBe(1);
		expect(result.failed).toEqual([{id: scheduled.userId, error: 'A deletion is already scheduled for this account'}]);
		const after = await getUserRepository().findUnique(createUserID(BigInt(scheduled.userId)));
		expect(after?.pendingDeletionAt?.getTime()).toBe(before?.pendingDeletionAt?.getTime());
		expect(after?.deletionScheduledBy?.toString()).toBe(scheduler.userId);
	});
});

describe('auto-resolve on scheduled deletion without report search', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'disabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	test('leaves the report pending and records that search was unavailable', async () => {
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const report = await createBuilder<ReportResponse>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: target.userId, category: 'spam_account'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(getReportSearchService()).toBeNull();
		await clearTestEmails(harness);
		await scheduleDeletionThroughEndpoint(harness, target);
		expect(await getReportStatus(report.report_id)).toBe(ReportStatus.PENDING);
		expect(await reportResolvedEmails(harness, reporter)).toEqual([]);
		expect(await autoResolveSummary(target.userId)).toEqual({
			outcome: 'search_unavailable',
			found_count: '0',
			resolved_count: '0',
			failed_count: '0',
			notified_count: '0',
		});
	});
});
