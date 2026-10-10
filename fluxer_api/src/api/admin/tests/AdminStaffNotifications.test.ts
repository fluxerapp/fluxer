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
import {setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {getAdminRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {getReportFlowVariant} from '@app/api/report/flows/ReportFlowRegistry';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, setPendingDeletionAt} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const DELETION_TEMPLATES = [
	'account_deletion_scheduled_requested',
	'account_deletion_scheduled_inactivity',
	'scheduled_deletion_notification',
	'account_scheduled_deletion',
];

describe('Admin staff notifications', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function auditMetadata(action: string, targetId: string): Promise<Map<string, string> | undefined> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
		return logs.find((log) => log.action === action && log.targetId.toString() === targetId)?.metadata;
	}

	async function emailsTo(account: TestAccount) {
		return listTestEmails(harness, {recipient: account.email});
	}

	async function systemDmMessages(account: TestAccount): Promise<Array<{content: string | null}>> {
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
		return messages.flat();
	}

	async function expireTempBan(target: TestAccount) {
		const users = getUserRepository();
		const user = (await users.findUnique(createUserID(BigInt(target.userId))))!;
		await users.patchUpsert(user.id, {temp_banned_until: new Date(Date.now() - 3_600_000)}, user.toRow());
	}

	async function scheduleDeletion(target: TestAccount, body: Record<string, unknown>) {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({days_until_deletion: 60, ...body})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function tempBan(target: TestAccount, body: Record<string, unknown>) {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	describe('schedule deletion', () => {
		test.each([
			[DeletionReasons.USER_REQUESTED, 'account_deletion_scheduled_requested'],
			[DeletionReasons.INACTIVITY, 'account_deletion_scheduled_inactivity'],
			[DeletionReasons.OTHER, 'scheduled_deletion_notification'],
			[DeletionReasons.SPAM, 'account_scheduled_deletion'],
		])('reason code %s sends %s', async (reasonCode, template) => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await scheduleDeletion(target, {reason_code: reasonCode, public_reason: 'Shown to the user'});
			const sent = (await emailsTo(target)).filter((email) => DELETION_TEMPLATES.includes(email.type));
			expect(sent.map((email) => email.type)).toEqual([template]);
			expect(sent[0]?.metadata.reason).toBe('Shown to the user');
			const metadata = await auditMetadata('schedule_deletion', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
			expect(metadata?.get('notification_template')).toBe(template);
		});

		test('notify_user false sends nothing and records it', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await scheduleDeletion(target, {reason_code: DeletionReasons.SPAM, notify_user: false});
			expect((await emailsTo(target)).filter((email) => DELETION_TEMPLATES.includes(email.type))).toEqual([]);
			const metadata = await auditMetadata('schedule_deletion', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
			expect(metadata?.has('notification_template')).toBe(false);
		});
	});

	describe('temp ban', () => {
		test('emails by default', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 24, reason: 'Spam'});
			expect((await emailsTo(target)).map((email) => email.type)).toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
		});

		test('notify_user false sends nothing', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('a permanent ban sends nothing', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 0});
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('false');
		});
	});

	describe('unban', () => {
		test('emails the public reason and never the audit log reason', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.header('X-Audit-Log-Reason', 'Private staff note')
				.body({public_reason: 'Appeal accepted'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const email = (await emailsTo(target)).find((entry) => entry.type === 'unban_notification');
			expect(email?.metadata.reason).toBe('Appeal accepted');
			expect(JSON.stringify(email)).not.toContain('Private staff note');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
			expect(metadata?.get('public_reason')).toBe('Appeal accepted');
		});

		test('accepts a request without a body', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.header('X-Audit-Log-Reason', 'Private staff note')
				.expect(HTTP_STATUS.OK)
				.execute();
			const email = (await emailsTo(target)).find((entry) => entry.type === 'unban_notification');
			expect(email?.metadata.reason).toBe('');
			expect(JSON.stringify(email)).not.toContain('Private staff note');
		});

		test('notify_user false sends nothing', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.body({notify_user: false})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('a temp ban that already expired is not emailed', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await expireTempBan(target);
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('an account pending deletion is not told it can log back in', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await scheduleDeletion(target, {reason_code: DeletionReasons.SPAM, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			expect((await auditMetadata('unban', target.userId))?.get('notification_sent')).toBe('false');
		});

		test('a user who was never banned is not emailed', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			expect((await auditMetadata('unban', target.userId))?.get('notification_sent')).toBe('false');
		});
	});

	describe('report resolve', () => {
		async function fileReport(): Promise<{reporter: TestAccount; reportId: string}> {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			const report = await createBuilder<{report_id: string}>(harness, reporter.token)
				.post('/reports/user')
				.body({user_id: reported.userId, category: 'harassment'})
				.execute();
			return {reporter, reportId: report.report_id};
		}

		async function fileDsaGuildNotice(): Promise<{email: string; reportId: string}> {
			const {guild} = await setupTestGuildWithMembers(harness, 1);
			const email = createUniqueEmail('dsa-resolve');
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
			const report = await createBuilderWithoutAuth<{report_id: string}>(harness)
				.post('/reports/dsa')
				.body({
					ticket,
					report_type: 'guild',
					guild_id: guild.id,
					revision_hash: getReportFlowVariant('guild', 'dsa').revisionHash,
					steps: [
						{screen_id: 'community_parts', item_ids: ['activity']},
						{screen_id: 'root_guild', option_id: 'abuse'},
						{screen_id: 'abuse_guild', option_id: 'raid'},
					],
					good_faith_confirmed: true,
					additional_info: 'This community organizes raids on other communities.',
					reporter_full_legal_name: 'Jane Doe',
					reporter_country_of_residence: 'NL',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			return {email, reportId: report.report_id};
		}

		async function resolve(reportId: string, body: Record<string, unknown>) {
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', ...body})
				.expect(HTTP_STATUS.OK)
				.execute();
		}

		async function setReporterFlag(reporter: TestAccount, flag: bigint) {
			const users = getUserRepository();
			const user = (await users.findUnique(createUserID(BigInt(reporter.userId))))!;
			await users.patchUpsert(user.id, {flags: user.flags | flag}, user.toRow());
		}

		test('notifies the reporter by default', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'Handled'});
			const sent = (await emailsTo(reporter)).filter((email) => email.type === 'report_resolved');
			expect(sent).toHaveLength(1);
			expect(sent[0]?.metadata).toEqual({report_id: reportId, public_comment: 'Handled'});
			const dms = await systemDmMessages(reporter);
			expect(dms).toHaveLength(1);
			expect(dms[0]?.content).toContain('Handled');
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('notify_reporter')).toBe('true');
			expect(metadata?.get('reporter_dm_sent')).toBe('true');
			expect(metadata?.get('reporter_email_sent')).toBe('true');
			expect(metadata?.get('reporter_dsa_email_sent')).toBe('false');
			expect(metadata?.has('reporter_notice_skipped')).toBe(false);
			expect(metadata?.has('internal_comment')).toBe(false);
			const stored = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
			expect(stored?.publicComment).toBe('Handled');
		});

		test('notify_reporter false still sends the generic notice without the comment', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'Staff only wording', notify_reporter: false});
			const sent = (await emailsTo(reporter)).filter((email) => email.type === 'report_resolved');
			expect(sent).toHaveLength(1);
			expect(sent[0]?.metadata).toEqual({report_id: reportId, public_comment: ''});
			const dms = await systemDmMessages(reporter);
			expect(dms).toHaveLength(1);
			expect(dms[0]?.content).toContain(reportId);
			expect(dms[0]?.content).not.toContain('Staff only wording');
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('notify_reporter')).toBe('false');
			expect(metadata?.get('reporter_dm_sent')).toBe('true');
			expect(metadata?.get('reporter_email_sent')).toBe('true');
			expect(metadata?.has('reporter_notice_skipped')).toBe(false);
			expect(metadata?.get('internal_comment')).toBe('Staff only wording');
			const stored = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
			expect(stored?.publicComment).toBeNull();
			const detail = await createBuilder<{public_comment: string | null}>(harness, admin.token)
				.get(`/admin/reports/${reportId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(detail.public_comment).toBeNull();
		});

		test('a whitespace comment sends the generic notice', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: '   '});
			const sent = (await emailsTo(reporter)).filter((email) => email.type === 'report_resolved');
			expect(sent.map((email) => email.metadata.public_comment)).toEqual(['']);
		});

		test('a DSA notice emails the verified address and sends no DM', async () => {
			const {email, reportId} = await fileDsaGuildNotice();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'We removed the community.'});
			const sent = await listTestEmails(harness);
			expect(sent.map((entry) => [entry.to, entry.type])).toEqual([[email, 'dsa_report_resolved']]);
			expect(sent[0]?.metadata).toEqual({report_id: reportId, public_comment: 'We removed the community.'});
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('report_type')).toBe('2');
			expect(metadata?.get('reporter_dm_sent')).toBe('false');
			expect(metadata?.get('reporter_email_sent')).toBe('false');
			expect(metadata?.get('reporter_dsa_email_sent')).toBe('true');
			expect(metadata?.has('reporter_notice_skipped')).toBe(false);
		});

		test('a DSA notice resolved with notify_reporter false gets the decision email without the comment', async () => {
			const {email, reportId} = await fileDsaGuildNotice();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'Staff only wording', notify_reporter: false});
			const sent = await listTestEmails(harness, {recipient: email});
			expect(sent.map((entry) => entry.type)).toEqual(['dsa_report_resolved']);
			expect(sent[0]?.metadata).toEqual({report_id: reportId, public_comment: ''});
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('reporter_dsa_email_sent')).toBe('true');
			expect(metadata?.get('internal_comment')).toBe('Staff only wording');
			const stored = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
			expect(stored?.publicComment).toBeNull();
		});

		test('a deleted reporter gets nothing and the audit row says why', async () => {
			const {reporter, reportId} = await fileReport();
			await deleteAccount(harness, reporter.token, reporter.password);
			await setPendingDeletionAt(harness, reporter.userId, new Date(Date.now() - 60_000));
			await createBuilderWithoutAuth(harness)
				.post(`/test/worker/process-pending-deletion/${reporter.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'Handled'});
			expect((await listTestEmails(harness)).filter((email) => email.type === 'report_resolved')).toEqual([]);
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('reporter_dm_sent')).toBe('false');
			expect(metadata?.get('reporter_email_sent')).toBe('false');
			expect(metadata?.get('reporter_dsa_email_sent')).toBe('false');
			expect(metadata?.get('reporter_notice_skipped')).toBe('reporter_account_deleted');
		});

		test('a reporter barred from reporting gets nothing and the audit row says why', async () => {
			const {reporter, reportId} = await fileReport();
			await setReporterFlag(reporter, UserFlags.REPORT_BANNED);
			await clearTestEmails(harness);
			await resolve(reportId, {public_comment: 'Handled'});
			expect((await emailsTo(reporter)).filter((email) => email.type === 'report_resolved')).toEqual([]);
			expect(await systemDmMessages(reporter)).toEqual([]);
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('reporter_dm_sent')).toBe('false');
			expect(metadata?.get('reporter_email_sent')).toBe('false');
			expect(metadata?.get('reporter_notice_skipped')).toBe('reporter_report_banned');
		});

		async function resolveAuditRows(reportId: string) {
			const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
			return logs.filter((log) => log.action === 'resolve_report' && log.targetId.toString() === reportId);
		}

		test('two resolves at the same time apply once', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			const attempts = await Promise.all(
				['First comment', 'Second comment'].map((comment) =>
					createBuilder<{public_comment: string | null; code?: string}>(harness, admin.token)
						.patch(`/admin/reports/${reportId}`)
						.body({status: 'resolved', public_comment: comment})
						.executeRaw(),
				),
			);
			expect(attempts.map(({response}) => response.status).sort()).toEqual([HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST]);
			const winner = attempts.find(({response}) => response.status === HTTP_STATUS.OK)!;
			const loser = attempts.find(({response}) => response.status === HTTP_STATUS.BAD_REQUEST)!;
			expect(loser.json.code).toBe(APIErrorCodes.REPORT_ALREADY_RESOLVED);
			const stored = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
			expect(stored?.status).toBe(1);
			expect(stored?.publicComment).toBe(winner.json.public_comment);
			expect((await emailsTo(reporter)).filter((email) => email.type === 'report_resolved')).toHaveLength(1);
			expect(await systemDmMessages(reporter)).toHaveLength(1);
			expect(await resolveAuditRows(reportId)).toHaveLength(1);
		});

		test('resolving a resolved report fails and sends nothing more', async () => {
			const {reporter, reportId} = await fileReport();
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', public_comment: 'Handled'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', public_comment: 'Handled again'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.REPORT_ALREADY_RESOLVED)
				.execute();
			expect((await emailsTo(reporter)).filter((email) => email.type === 'report_resolved')).toEqual([]);
			expect(await systemDmMessages(reporter)).toHaveLength(1);
			expect(await resolveAuditRows(reportId)).toHaveLength(1);
			const stored = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
			expect(stored?.publicComment).toBe('Handled');
		});
	});
});
